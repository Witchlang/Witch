//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
mod util;
use std::collections::HashMap;
use std::ops::Range;

use crate::error::{Error, Result};
use witch_parser::ast::{Ast, Operator};
use witch_parser::types::{Type, TypeDecl};
use witch_runtime::value::{Function, Value};
use witch_runtime::vm::Op;

#[derive(Debug, Clone)]
pub struct Upvalue {
    index: usize,

    // Whether the index refers to a local variable, or an upvalue in the parent scope
    is_local: bool,
}

/// Contains all compile-time information about a locally scoped
/// variable.
#[derive(Debug, Clone)]
pub struct LocalVariable {
    name: String,
    is_captured: bool,
    r#type: Type,
    is_mutable: bool,
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub locals: Vec<LocalVariable>,
    pub types: HashMap<String, Type>,
    pub upvalues: Vec<Upvalue>,
}

impl Scope {
    /// Adds an upvalue to the current scope. `is_local` refers to whether the index points to
    /// a scope-local variable or the upvalue index of the parent scope.
    pub fn add_upvalue(&mut self, index: usize, is_local: bool) -> Option<usize> {
        // If the upvalue already exists, simply return its index
        for (i, u) in self.upvalues.iter().enumerate() {
            if u.index == index && u.is_local == is_local {
                return Some(i);
            }
        }

        self.upvalues.push(Upvalue { index, is_local });
        Some(self.upvalues.len() - 1)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Cached {
    pub bytecode: Vec<u8>,
    pub flushed: bool,
}

#[derive(Debug, Clone)]
pub struct Context {
    pub scopes: Vec<Scope>,

    /// Lineage is a straight line of parent relationships from the current
    /// `compile` iteration up the the root node.
    pub lineage: Vec<Ast>,

    /// If we are currently within a function declaration, this is it's type.
    pub current_function_type: Option<Type>,

    /// If we're currently in an assignment, keep track of Ident and the assigned Type (may be Unknown)
    pub assignment_ctx: Option<(String, Type)>,

    /// Values get cached in order keep the subsequent programs smaller
    pub prelude: Option<Vec<u8>>,
    pub functions_cache: Vec<Cached>,
    pub value_cache: Vec<Cached>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            lineage: Default::default(),
            current_function_type: None,
            assignment_ctx: None,
            prelude: None,
            functions_cache: Default::default(),
            value_cache: Default::default(),
        }
    }
}

impl Context {
    pub fn scope(&mut self) -> Result<&mut Scope> {
        self.scopes.last_mut().ok_or(Error::fatal())
    }

    pub fn scope_by_index(&mut self, scope_idx: usize) -> Result<&mut Scope> {
        self.scopes.get_mut(scope_idx).ok_or(Error::fatal())
    }

    /// Ensures that a type is not a Type::Typevar, instead it resolves it to either a
    /// generic constraint or a custom type.
    pub fn resolve_type(&self, ty: Type) -> Type {
        if !matches!(ty, Type::TypeVar { .. }) {
            return ty;
        }

        match ty {
            Type::TypeVar { name, .. } => {
                // Start with the current scope and walk upwards.
                for scope in self.scopes.iter().rev() {
                    dbg!(&name, &scope);
                    // TODO check generics first and handle inner types
                    if scope.types.contains_key(&name) {
                        return scope.types.get(&name).unwrap().clone();
                    }
                }
                Type::Unknown
            }
            ty => ty,
        }
    }

    /// For non-local variables, we recursively walk upwards from our current scope until we find it.
    /// When it is found, it is marked as `captured`. We then add the upvalue to each scope on the way back,
    /// giving us a linked list of sorts pointing to the original variable.
    pub fn resolve_upvalue(
        &mut self,
        ident: &str,
        scope_idx: usize,
    ) -> Result<Option<(usize, Type)>> {
        // If we are in the outermost scope, there are no upvalues to resolve
        if scope_idx == 0 {
            return Ok(None);
        }

        // Check the immediate parent scope for the variable
        let mut parent_local = None;
        for (local_index, variable) in self
            .scope_by_index(scope_idx - 1)?
            .locals
            .iter()
            .enumerate()
        {
            if variable.name == *ident {
                parent_local = Some((local_index, variable.clone()));
                break;
            }
        }

        // If its found, we mark it as captured and return its index and type
        if let Some((local_index, variable)) = parent_local {
            self.scope_by_index(scope_idx - 1)?.locals[local_index].is_captured = true;

            let ty = variable.r#type.clone();
            return Ok(self
                .scope_by_index(scope_idx)?
                .add_upvalue(local_index, true)
                .map(|idx| (idx, ty)));
        }

        // If its not found in the parent scope, we recurse up the stack of scopes
        // until it is found (or not).
        if let Some((idx, ty)) = self.resolve_upvalue(ident, scope_idx - 1)? {
            return Ok(self
                .scope_by_index(scope_idx)?
                .add_upvalue(idx, false)
                .map(|idx| (idx, ty)));
        }

        Ok(None)
    }

    /// Flushes the current context.
    /// Unflushed cached values get returned as a `prelude` bytecode and marked as such.
    /// Scopes and AST lineage of the context get reset.
    pub fn flush(&mut self) -> Vec<u8> {
        let mut bc = self.prelude.take().unwrap_or(vec![]);

        let mut len: usize = 0;
        for cached in self.value_cache.iter_mut() {
            if !cached.flushed {
                cached.flushed = true;
                bc.append(&mut cached.bytecode.clone());
                len += 1;
            }
        }
        bc.push(Op::SetupValueCache as u8);
        let len: [u8; std::mem::size_of::<u64>()] = len.to_ne_bytes();
        bc.append(&mut len.to_vec());

        let mut len: usize = 0;
        for cached in self.functions_cache.iter_mut() {
            if !cached.flushed {
                cached.flushed = true;
                bc.append(&mut cached.bytecode.clone());
                len += 1;
            }
        }
        bc.push(Op::SetupFunctionCache as u8);
        let len: [u8; std::mem::size_of::<usize>()] = len.to_ne_bytes();
        bc.append(&mut len.to_vec());
        bc
    }

    /// Adds a value bytecode to the cache, unless it has already been cached
    /// before. Whether the cached value has been flushed in a previous script
    /// has no bearing on the cached entry index.
    pub fn cache_value(&mut self, value_bytecode: Vec<u8>) -> usize {
        if let Some(idx) = self
            .value_cache
            .iter()
            .position(|cached| &cached.bytecode == &value_bytecode)
        {
            idx
        } else {
            self.value_cache.push(Cached {
                bytecode: value_bytecode,
                flushed: false,
            });
            self.value_cache.len() - 1
        }
    }

    /// Adds a fn bytecode to the cache, unless it has already been cached
    /// before. Whether the cached value has been flushed in a previous script
    /// has no bearing on the cached entry index.
    pub fn cache_fn(&mut self, fn_bytecode: Vec<u8>) -> usize {
        if let Some(idx) = self
            .functions_cache
            .iter()
            .position(|cached| &cached.bytecode == &fn_bytecode)
        {
            idx
        } else {
            self.functions_cache.push(Cached {
                bytecode: fn_bytecode,
                flushed: false,
            });
            self.functions_cache.len() - 1
        }
    }
}

/// Turns an AST into bytecode
pub fn compile<'a>(ctx: &mut Context, ast: &Ast) -> Result<(Vec<u8>, Type)> {
    ctx.lineage.push(ast.clone());

    let (bytecode, return_type) = match &ast {
        Ast::Assignment { ident, expr, span } => assignment(ctx, ident, expr, span)?,
        Ast::Call {
            expr,
            args,
            span: _,
        } => call(ctx, expr, args)?,
        Ast::Function {
            is_variadic,
            args,
            returns,
            body,
            generics,
        } => function(ctx, is_variadic, args, returns, body, generics.clone())?,
        Ast::If {
            predicate,
            then_,
            else_,
            span,
        } => if_(ctx, predicate, then_, else_)?,
        Ast::Infix { lhs, op, rhs, .. } => infix(ctx, lhs, op, rhs)?,
        //  Ast::Member { container, key } => member(ctx, container, key)?,
        Ast::Let {
            ident,
            annotated_type,
            expr,
            span,
        } => let_(ctx, ident, annotated_type, expr, span)?,
        Ast::Return { expr, span: _ } => return_(ctx, expr)?,
        Ast::Statement { stmt, rest, span } => statement(ctx, stmt.clone(), rest.clone(), span)?,
        Ast::Struct {
            ident,
            fields,
            span,
        } => struct_literal(ctx, ident, fields, span)?,
        Ast::Type { name, decl, span } => decl_type(ctx, name, decl, span)?,
        Ast::Value(v) => value(ctx, v)?,
        Ast::Var(ident) => var(ctx, ident)?,
        x => todo!("{:?}", x),
    };

    Ok((bytecode, return_type))
}

/// Assigns a local variable by setting its new value without initializing it.
/// It needs to either be mutable or the parent expr needs to be `Ast::Let`.
fn assignment(
    ctx: &mut Context,
    ident: &str,
    expr: &Box<Ast>,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    let (mut expr_bytes, expr_type) = compile(ctx, expr)?;

    // Find a local var with the right name, get its index
    let mut local_variable = None;
    for (i, local) in ctx.scope()?.locals.iter().enumerate() {
        if local.name == ident {
            local_variable = Some((i, local));
            break;
        }
    }

    if let Some((local_variable, local)) = local_variable {
        if let Ok(local_variable) = u8::try_from(local_variable) {
            let var_type = local.r#type.clone();
            if var_type != Type::Unknown && var_type != expr_type {
                panic!(
                    "cant coerce between types!!! var ({:?}): {:?}, expr: {:?}",
                    local.name, local.r#type, &expr_type
                );
            }
            expr_bytes.push(Op::Set as u8);
            expr_bytes.push(local_variable);
            return Ok((expr_bytes, expr_type));
        }
        unreachable!()
    } else if false
    //let Some((upvalue, ty)) = resolve_upvalue(&mut program.compiler_stack, compiler, ident)
    {
        todo!("upvalue");
        // if ty != Type::Unknown && !ty.equals(&expr_type) {
        //     panic!(
        //         "cant coerce between types!!! upvalue type: {:?}, expr: {:?}",
        //         &ty, &expr_type
        //     );
        // }
        // bytecode.append(&mut expr_bytes);
        // bytecode.push(OpCode::SetUpvalue as u8);
        // bytecode.push(upvalue);
        // return_type = expr_type;
    } else {
        panic!("Attempted to reassign unknown variable {:?}", ident)
    }
}

/// Functions are first-class, meaning that essentially anything can be called as
/// a function and hopefully it'll evaluate to something callable.
/// While the emitted bytecode is simple, some (a lot) more effort is required for the type checking
/// during compilation.
fn call(ctx: &mut Context, expr: &Box<Ast>, args: &Vec<Ast>) -> Result<(Vec<u8>, Type)> {
    let mut bytecode = vec![];
    // If this is being called on an Entry expression, that means a method call.
    // Method call means we have an implicit `self` variable as a first argument.
    // Let's stick the Entry object on the stack before the arguments then.
    if let Ast::Member { container, .. } = *expr.clone() {
        let (mut bc, _) = compile(ctx, &container)?;
        bytecode.append(&mut bc);
    }

    let mut args_with_types = vec![];
    for arg in args.iter() {
        let (mut bc, arg_type) = compile(ctx, arg)?;
        bytecode.append(&mut bc);
        args_with_types.push((arg.clone(), arg_type));
    }

    let (mut bc, mut called_type) = compile(ctx, expr)?;

    // This handles the recursion edge case. When we're calling a function recursively, it's not actually bound to the
    // variable name at the time of compiling the function body, so the variable we're calling is undefined at this point.
    // That makes it very hard to type check and infer the return type!
    // Hopefully, EITHER the LET expression OR the FUNCTION expressions are annotated.
    // If the LET expression is annotated, the Expr::Var evaluation will return the correct type as that information
    // is tracked during initialization. If not, the type information for our current function declaration is tracked
    // within the current context object.
    if let (Type::Unknown, Ast::Var(ident), Some((assign_ident, _)), Some(function_type)) = (
        called_type.clone(),
        &*(expr.clone()),
        &ctx.assignment_ctx,
        ctx.current_function_type.clone(),
    ) {
        if ident == assign_ident {
            // RECURSION ALERT
            called_type = function_type;
        }
    }

    match called_type {
        Type::Function {
            args: arg_types,
            is_variadic,
            returns,
            mut generics,
        } => {
            // If there is a surrounding function type context (like if this function is an argument to
            // a surrounding function), we inherit its generics definitions where we dont make our own
            if let Some(Type::Function {
                generics: surrounding_generics,
                ..
            }) = &ctx.current_function_type
            {
                for (k, v) in surrounding_generics.iter() {
                    generics.entry(k.to_string()).or_insert(v.clone());
                }
            }

            // Compare arguments length against the type.
            // If the type is not variadic, they len's should be the same.
            if args.len() != arg_types.len() {
                if !is_variadic {
                    panic!(
                        "wrong amount of arguments. want: {:?}, got: {:?}",
                        &arg_types.len(),
                        &args.len()
                    );
                // If it is, we ensure that all arguments after the last arg type are the same
                } else {
                    for (idx, (_, ty)) in args_with_types[(arg_types.len() - 1)..args.len() - 1]
                        .iter()
                        .enumerate()
                    {
                        if arg_types.last().unwrap() != ty {
                            panic!(
                                "wrong type in arg nr {}! wants {:?}, got {:?}",
                                idx,
                                arg_types.last(),
                                ty
                            )
                        }
                    }
                }
            }

            // Type check arguments and update the `bindings` map with what our generics correspond to for this call
            let mut bindings = HashMap::new();
            for (idx, wanted_type) in arg_types.into_iter().enumerate() {
                let supplied_type = args_with_types[idx].1.clone();

                // If wanted type is a variable type, check if its supplied in the function's generics constraints.
                // If it is, compare it against the supplied type and update the bindings table.
                // If it isn't, resolve it to an actual type and compare it against the supplied one.
                match wanted_type {
                    ref wanted @ Type::TypeVar { ref name, .. } => {
                        if let Some(constraint) = generics.get(name) {
                            if &supplied_type == constraint {
                                bindings.insert(wanted.clone(), supplied_type);
                            }
                        } else {
                            let ty = ctx.resolve_type(wanted.clone());
                            if ty != supplied_type {
                                return Err(Error::fatal()); //todo
                            }
                        }
                    }
                    wanted => {
                        if wanted != supplied_type {
                            return Err(Error::fatal()); //todo
                        }
                    }
                }
            }

            bytecode.append(&mut bc);
            bytecode.push(Op::Call as u8);
            bytecode.push(u8::try_from(args.len()).unwrap());

            let return_type = bindings.remove(&*(returns.clone())).unwrap_or(*returns);
            Ok((bytecode, return_type))
        }
        _ => {
            dbg!(called_type);
            panic!("attempted to call a non function???");
        }
    }
}

/// Declares a new function.
fn function(
    ctx: &mut Context,
    is_variadic: &bool,
    args: &Vec<(String, Type)>,
    returns: &Type,
    body: &Box<Ast>,
    mut generics: HashMap<String, Type>,
) -> Result<(Vec<u8>, Type)> {
    // If there is a surrounding function type context (like if this function is an argument to
    // a surrounding function), we inherit its generics definitions where we dont make our own
    if let Some(Type::Function {
        generics: ref surrounding_generics,
        ..
    }) = ctx.current_function_type
    // TODO this can likely be put in the Scopes
    {
        for (k, v) in surrounding_generics.iter() {
            generics.entry(k.to_string()).or_insert(v.clone());
        }
    }

    let ty = Type::Function {
        args: args.iter().map(|a| a.1.clone()).collect(),
        returns: Box::new(returns.clone()),
        is_variadic: *is_variadic,
        generics,
    };

    let current_function_type_copy = ctx.current_function_type.clone();
    ctx.current_function_type = Some(ty.clone());

    let mut scope = Scope::default();
    // TODO add implicit self arg as local variable to this scope
    for (arg_name, arg_type) in args.iter() {
        scope.locals.push(LocalVariable {
            name: arg_name.clone(),
            is_mutable: false,
            is_captured: false,
            r#type: arg_type.clone(),
        })
    }
    ctx.scopes.push(scope);

    let (func_bytecode, _actually_returns) = compile(ctx, body)?;

    let mut upvalues_bytecode = vec![];
    ctx.scope()?.upvalues.clone().iter().for_each(|u| {
        if u.is_local {
            upvalues_bytecode.push(1_u8);
        } else {
            upvalues_bytecode.push(0_u8);
        }
        upvalues_bytecode.push(u.index as u8); // todo allow largers size here?
    });
    let upvalue_count = ctx.scope()?.upvalues.len() as u8;

    ctx.scopes.pop();

    // TODO typecheck returns
    // if the provided return type is more loose than the actually_returns type, overwrite it

    // If the function ends with a return statement from a Block expression [..., Return, 0_u8, _],
    // we can safely remove that since we're providing our own Return opcode from the function.
    //let last_three = &func_bytecode[func_bytecode.len() - 3..func_bytecode.len() - 1];
    //if last_three == &[OpCode::Return as u8, 0_u8] {
    //    func_bytecode.truncate(func_bytecode.len() - 3);
    //}

    ctx.current_function_type = current_function_type_copy;

    let mut is_method = false;
    for parent in ctx.lineage.iter().rev() {
        if matches!(
            parent,
            &Ast::Type {
                decl: TypeDecl::Struct { .. },
                ..
            }
        ) {
            is_method = true;
            break;
        }
    }

    let function = Value::Function(Function {
        is_variadic: *is_variadic,
        is_method,
        arity: args.len(),
        bytecode: func_bytecode,
        upvalue_count,
        upvalues: vec![],
        upvalues_bytecode: upvalues_bytecode,
    });

    let (function_bytecode, _) = compile(ctx, &Ast::Value(function))?;

    Ok((function_bytecode, ty))
}

/// If is implemented by utilizing the Jump and JumpIfFalse opcodes. We evaluate the two expressions and
/// get the length of their bytecode instructions. If the Predicate expression is false, we jump over the
/// Then expression length straight to the Else statement. If the Predicate is true, we fall through to the
/// Then expression and subsequently Jump over the Else expression.
fn if_(
    ctx: &mut Context,
    predicate: &Box<Ast>,
    then_: &Box<Ast>,
    else_: &Box<Ast>,
) -> Result<(Vec<u8>, Type)> {
    let (mut predicate_bytecode, predicate_ty) = compile(ctx, predicate)?;
    if !matches!(predicate_ty, Type::Bool) {
        panic!("predicate expression must return a boolean value");
    }
    let (mut then_bytecode, _then_ty) = compile(ctx, then_)?;
    let (mut else_bytecode, _else_ty) = compile(ctx, else_)?;

    let mut bytecode = vec![];

    bytecode.append(&mut predicate_bytecode);
    bytecode.push(Op::JumpIfFalse as u8);

    // Jump the size of the Then statement + this len value + one more instruction
    let mut then_len = (then_bytecode.len() + 8 + 1).to_ne_bytes().to_vec();
    bytecode.append(&mut then_len);

    bytecode.append(&mut then_bytecode);
    bytecode.push(Op::Jump as u8);
    let mut else_len = (else_bytecode.len() + 8).to_ne_bytes().to_vec();

    bytecode.append(&mut else_len);
    bytecode.append(&mut else_bytecode);

    Ok((bytecode, Type::Void))
}

/// Expresses a binary operation such as 1 + 1, a == b, 9 > 8, etc.
/// Requres the two expressions to be of the same type.
fn infix(ctx: &mut Context, a: &Box<Ast>, op: &Operator, b: &Box<Ast>) -> Result<(Vec<u8>, Type)> {
    let (mut bytecode, a_type) = compile(ctx, a)?;
    let (mut bytecode_b, b_type) = compile(ctx, b)?;

    if !a_type.allowed_infix_operators(&b_type).contains(op) {
        dbg!(&ctx.lineage);
        panic!(
            "infix op {:?} not allowed between {:?} and {:?}",
            op, a_type, b_type
        );
    }

    // if !a_type.is_numeric() || !b_type.is_numeric() {
    //     // Handle string concat or mul,
    //     // list concats etc by mapping against builtin functions for dealing with that stuff
    //     todo!();
    // }

    let return_type = op.resulting_type(a_type);

    bytecode.append(&mut bytecode_b);
    bytecode.push(Op::Binary as u8);
    bytecode.push(op.to_owned() as u8);

    Ok((bytecode, return_type))
}

/// Accesses a member within an Object or Vector, by first putting the backing Object
/// on the stack and subsequently the Key as an Access Opcode.
fn member(_ctx: &mut Context, _container: &Box<Ast>, _key: &Box<Ast>) -> Result<(Vec<u8>, Type)> {
    Ok((vec![], Type::Unknown))
}

/// Pops the current call frame
fn return_(ctx: &mut Context, expr: &Box<Ast>) -> Result<(Vec<u8>, Type)> {
    let (mut bytecode, ty) = compile(ctx, expr)?;
    bytecode.push(Op::Return as u8);
    Ok((bytecode, ty))
}

/// Compiles a statement and optionally the rest of the program.
fn statement(
    ctx: &mut Context,
    stmt: Box<Ast>,
    rest: Box<Ast>,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    if let Ast::Nop = *rest {
        let (bytecode, ty) = compile(ctx, &stmt)?;
        let return_type = if let Ast::Return { .. } = *stmt {
            ty
        } else {
            Type::Void
        };
        return Ok((bytecode, return_type));
    }

    let (mut bytecode, _) = compile(ctx, &stmt)?;
    let (mut rest_bytecode, ty) = compile(ctx, &rest)?;
    bytecode.append(&mut rest_bytecode);
    Ok((bytecode, ty))
}

fn struct_literal(
    ctx: &mut Context,
    ident: &Option<String>,
    fields: &HashMap<String, Ast>,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    let mut return_type = Type::Unknown;
    let mut field_types = None;

    // if named, look up the type and get vtable entries for the methods
    if let Some(name) = ident {
        // This is a named struct. make sure the type is defined
        if let ref struct_type @ Type::Struct { ref fields, .. } = ctx
            .resolve_type(Type::TypeVar {
                name: name.to_string(),
                inner: vec![],
            })
            .clone()
        {
            return_type = struct_type.clone();
            field_types = Some(fields);
        } else {
            panic!(
                "struct of name {} is not defined at this point or is of the wrong type",
                name
            );
        }
    }
    // put fields on the stack
    // put method pointers on the stack
    // emit collect list bytecode

    Ok((vec![], Type::Unknown))
}

/// Declares a new type for the current scope. The type itself does not emit any bytecode,
/// but struct methods get inserted into our function vtable, which get emitted post compilation.
fn decl_type(
    ctx: &mut Context,
    name: &str,
    decl: &TypeDecl,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    let ty = match decl {
        TypeDecl::Enum {
            variants,
            generics: _,
        } => Type::Enum(variants.clone()),

        TypeDecl::Struct {
            generics,
            fields,
            methods,
        } => {
            let mut methods_lookup = HashMap::default();
            for (name, ast) in methods.iter() {
                let (fn_bytecode, ty) = compile(ctx, ast)?;
                let idx = ctx.cache_fn(fn_bytecode);
                methods_lookup.insert(name.to_owned(), (ty, idx));
            }

            Type::Struct {
                name: Some(name.to_string()),
                fields: fields.clone(),
                methods: methods_lookup,
                generics: generics.clone(),
            }
        }
        // TypeDecl::Interface { properties } => Type::Interface {
        //     name: name.to_string(),
        //     properties: properties
        //         .to_owned()
        //         .into_iter()
        //         .map(|(s, t)| {
        //             if let Type::Function {
        //                 args,
        //                 returns,
        //                 is_variadic,
        //                 generics,
        //                 ..
        //             } = t
        //             {
        //                 (
        //                     s,
        //                     Type::Function {
        //                         is_method: true,
        //                         args,
        //                         returns,
        //                         is_variadic,
        //                         generics,
        //                     },
        //                 )
        //             } else {
        //                 (s, t)
        //             }
        //         })
        //         .collect::<hashbrown::HashMap<String, Type>>(),
        // },
        // TypeDecl::Struct {
        //     fields,
        //     methods: methods_expr,
        // } => {
        //     let mut methods = hashbrown::HashMap::default();

        //     program.compiler_stack[compiler].types.insert(
        //         name.to_string(),
        //         Type::Struct {
        //             name: name.to_string(),
        //             fields: fields
        //                 .to_owned()
        //                 .into_iter()
        //                 .collect::<hashbrown::HashMap<String, Type>>(),
        //             methods: methods.clone(),
        //         },
        //     );

        //     for (method_name, expr) in methods_expr {
        //         program.compiler(compiler).self_ctx = Some(name.clone());
        //         let expr = if let Expr::Function {
        //             is_variadic,
        //             args,
        //             body,
        //             r#type,
        //             ..
        //         } = expr.clone()
        //         {
        //             Expr::Function {
        //                 is_variadic,
        //                 is_method: true,
        //                 args,
        //                 body,
        //                 r#type,
        //             }
        //         } else {
        //             expr.to_owned()
        //         };
        //         let (method_bc, mut method_signature) = walk(program, compiler, &expr);
        //         program.compiler(compiler).self_ctx = None;
        //         if let Type::Function {
        //             args,
        //             returns,
        //             is_variadic,
        //             generics,
        //             ..
        //         } = method_signature
        //         {
        //             method_signature = Type::Function {
        //                 is_method: true,
        //                 args,
        //                 returns,
        //                 is_variadic,
        //                 generics,
        //             };
        //         }
        //         methods.insert(method_name.to_string(), (method_signature, method_bc));
        //         program.compiler_stack[compiler].types.insert(
        //             name.to_string(),
        //             Type::Struct {
        //                 name: name.to_string(),
        //                 fields: fields.to_owned().into_iter().collect::<hashbrown::HashMap<
        //                     String,
        //                     Type,
        //                 >>(
        //                 ),
        //                 methods: methods.clone(),
        //             },
        //         );
        //     }
        //     Type::Struct {
        //         name: name.to_string(),
        //         fields: fields
        //             .to_owned()
        //             .into_iter()
        //             .collect::<hashbrown::HashMap<String, Type>>(),
        //         methods,
        //     }
        // }
        _ => Type::Unknown,
    };
    ctx.scope()
        .unwrap()
        .types
        .insert(name.to_string(), ty.clone());
    Ok((vec![], ty))
}

/// Creates a new local variable.
fn let_(
    ctx: &mut Context,
    ident: &str,
    annotated_type: &Option<Type>,
    expr: &Box<Ast>,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    // Create a new local var of type `ty`
    let ty = annotated_type.clone().unwrap_or(Type::Unknown);
    let old_assignment_ctx = ctx.assignment_ctx.clone();
    ctx.assignment_ctx = Some((ident.to_owned(), ty.clone()));
    let local_variable = LocalVariable {
        name: ident.to_string(),
        is_mutable: false,
        is_captured: false,
        r#type: ty.clone(),
    };
    ctx.scope()?.locals.push(local_variable);

    let (assignment_bytes, mut assignment_type) = compile(ctx, expr)?;

    // If the original type is Unknown, update the local var with the assignment type
    // If it isn't, conduct a type check.
    match ty {
        Type::Unknown => {
            let locals = &mut ctx.scope()?.locals;
            locals.last_mut().unwrap().r#type = assignment_type.clone();
        }
        _ if ty != assignment_type => {
            panic!(
                "attempted to assign value of type {:?} to a variable of type {:?}",
                assignment_type, ty
            );
        }
        _ => {
            assignment_type = ty;
        }
    }

    ctx.assignment_ctx = old_assignment_ctx;
    Ok((assignment_bytes, assignment_type))
}

/// Raw values get emitted into the bytecode as <usize length><bytes>.
fn value(ctx: &mut Context, value: &Value) -> Result<(Vec<u8>, Type)> {
    let mut value_bytecode = vec![];
    let mut bytes = util::serialize_value(value.clone())?;
    let length: [u8; std::mem::size_of::<usize>()] = bytes.len().to_ne_bytes();
    value_bytecode.push(Op::Push as u8);
    value_bytecode.append(&mut length.to_vec());
    value_bytecode.append(&mut bytes);

    let return_type = Type::from(value);
    Ok((value_bytecode, return_type))
}

/// Resolves a local variable. Since scope.locals will match the runtime stack,
/// we grab the index by the provided name and emit <Get><stack-index>.
fn var(ctx: &mut Context, ident: &String) -> Result<(Vec<u8>, Type)> {
    // Find a local var with the right name, get its index
    let mut local_variable = None;

    // First check all variables local to us
    for (i, local) in ctx.scope()?.locals.iter().enumerate() {
        if local.name == *ident {
            local_variable = Some((i, local));
            break;
        }
    }

    if let Some(local_variable) = local_variable {
        if let Ok(idx) = u8::try_from(local_variable.0) {
            let return_type = local_variable.1.r#type.clone();
            return Ok((vec![Op::Get as u8, idx], return_type));
        }
        unreachable!()
    } else if let Some((idx, return_type)) = ctx.resolve_upvalue(ident, ctx.scopes.len() - 1)? {
        return Ok((vec![Op::GetUpvalue as u8, idx as u8], return_type));
    } else {
        dbg!(&ident, &ctx.scope()?.locals);
        todo!();
        // If we dont find a variable with this name, it may be an Enum type
        // used in an Entry/Call expression.
        // if let e @ Type::Enum(_) = resolve_type_alias(
        //     &mut program.compiler_stack,
        //     compiler,
        //     &Type::Custom(ident.clone()),
        // ) {
        //     return_type = e;
        //     // We dont emit any bytecode for this, its only used by the type checker.
        // } else {
        //     dbg!("unresolved var with ident", ident);
        //     todo!();
        // }
    }
}
