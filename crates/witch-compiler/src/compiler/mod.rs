//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
mod type_system;
mod util;
use std::collections::HashMap;
use std::ops::Range;

use crate::error::{Error, Result};
use witch_parser::ast::{Ast, Key, Operator};
use witch_parser::types::{Type, TypeDecl};
use witch_runtime::value::{Function, Value};
use witch_runtime::vm::Op;

use self::type_system::TypeSystem;

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

    /// A stack of bindings of concrete types to type variable names
    pub bindings: Vec<HashMap<Type, Type>>,
    /// Generic constraints for this scope, if any
    pub generics: HashMap<String, Type>,
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

    /// Convenience function to get the index number and data of a local variable
    pub fn get_local(&self, ident: &str) -> Option<(usize, LocalVariable)> {
        for (i, local) in self.locals.iter().enumerate() {
            if local.name == ident {
                return Some((i, local.clone()));
            }
        }
        None
    }
}

#[derive(Debug, Default, Clone)]
pub struct Cached {
    pub bytecode: Vec<u8>,
    pub flushed: bool,
}

#[derive(Debug, Clone)]
pub struct Context {
    /// Holds the type system
    pub ts: TypeSystem,

    /// Scopes map to callframes at runtime
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
            ts: TypeSystem::new(),
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

    pub fn get_type(&self, name: &str) -> Type {
        self.ts.get_type(name).unwrap()
    }

    /// Adds a type to the type system. May only be done in the root/global scope!
    pub fn add_type(&mut self, name: String, typ: Type) -> Result<()> {
        if self.scopes.len() > 1 {
            dbg!("attempted to add type in non-root scope");
            return Err(Error::fatal());
        }
        self.ts.add_type(name, typ)
    }

    pub fn push_type_scope(&mut self, subs: &Vec<(String, Type)>) {
        self.ts.push_scope(subs.clone().into_iter().collect())
    }

    pub fn pop_type_scope(&mut self) {
        self.ts.pop_scope()
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
        let mut bc = self.prelude.take().unwrap_or_default();

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

    /// Replaces the bytecode at index `idx` within the functions cache.
    pub fn cache_fn_at(&mut self, idx: usize, fn_bytecode: Vec<u8>) {
        self.functions_cache[idx] = Cached {
            bytecode: fn_bytecode,
            flushed: false,
        };
    }
}

/// Turns an AST into bytecode
pub fn compile<'a>(ctx: &mut Context, ast: &Ast) -> Result<(Vec<u8>, Type)> {
    ctx.lineage.push(ast.clone());

    let (bytecode, return_type) = match &ast {
        Ast::Assignment { lhs, rhs, span } => assignment(ctx, lhs, rhs, span)?,
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
        } => function(ctx, is_variadic, args, returns, body, generics)?,
        Ast::If {
            predicate,
            then_,
            else_,
            span: _,
        } => if_(ctx, predicate, then_, else_)?,
        Ast::Infix { lhs, op, rhs, .. } => infix(ctx, lhs, op, rhs)?,
        Ast::Let {
            ident,
            annotated_type,
            expr,
            span,
        } => let_(ctx, ident, annotated_type, expr, span)?,
        Ast::List { items, span } => list(ctx, items, span)?,
        Ast::Member {
            container,
            key,
            span,
        } => member(ctx, container, key, span)?,
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

    // For each compilation step, we try to resolve the return type to be as concrete as possible
    Ok((bytecode, ctx.ts.resolve(return_type)?))
}

/// Assigns a local variable by setting its new value without initializing it.
/// It needs to either be mutable or the parent expr needs to be `Ast::Let`.
fn assignment(
    ctx: &mut Context,
    lhs: &Ast,
    rhs: &Ast,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    let mut member_key = None;
    let ident = match lhs.clone() {
        Ast::Var(ident) => ident,
        Ast::Member { container, key, .. } => match *container {
            Ast::Var(ident) => {
                member_key = Some(key);
                ident
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let (mut expr_bytes, expr_type) = compile(ctx, rhs)?;

    if let Some((local_variable, local)) = ctx.scope()?.get_local(&ident) {
        if let Ok(local_variable) = u8::try_from(local_variable) {
            let var_type = ctx.ts.resolve(local.r#type.clone())?;

            match (member_key, &var_type) {
                (None, _) => {
                    if var_type != Type::Unknown && var_type != expr_type {
                        panic!(
                            "cant coerce between types!!! var ({:?}): {:?}, expr_type: {:?}",
                            local.name, var_type, &expr_type
                        );
                    }
                    expr_bytes.push(Op::Set as u8);
                    expr_bytes.push(local_variable);
                    return Ok((expr_bytes, expr_type));
                }

                (Some(Key::String(key)), Type::Struct { fields, .. }) => {
                    let idx = fields.iter().position(|f| f.0 == key).unwrap();
                    expr_bytes.push(Op::SetProperty as u8);
                    expr_bytes.push(local_variable);
                    expr_bytes.push(idx as u8);
                    return Ok((expr_bytes, expr_type));
                }

                (Some(Key::Usize(idx)), Type::List(_)) => {
                    expr_bytes.push(Op::SetProperty as u8);
                    expr_bytes.push(local_variable);
                    expr_bytes.push(idx as u8);
                    return Ok((expr_bytes, expr_type));
                }

                _ => unreachable!(),
            }
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
    let mut arity = args.len();

    // If this is being called on a Member expression, that means a method call.
    // Method call means we have an implicit `self` variable as a first argument.
    // Let's stick the Entry object on the stack before the arguments then.
    if let Ast::Member { container, .. } = *expr.clone() {
        let (mut bc, _) = compile(ctx, &container)?;
        bytecode.append(&mut bc);
        arity += 1;
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
            generics,
        } => {
            // {
            //     for (k, v) in ctx.scope()?.generics.iter() {
            //         generics.entry(k.to_string()).or_insert(v.clone());
            //     }
            // }

            ctx.push_type_scope(&generics);

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

            //ctx.scope()?.bindings.push(HashMap::default());
            // Type check arguments and update the `bindings` map with what our generics correspond to for this call
            for (idx, wanted_type) in arg_types.into_iter().enumerate() {
                let supplied_type = args_with_types[idx].1.clone();

                let resolved_wanted_type = ctx.ts.resolve(wanted_type.clone())?; // ctx.ts.resolve_recursive(wanted_type.clone(), vec![generics.clone()]);

                if resolved_wanted_type != supplied_type {
                    panic!(
                        "type error in function call, wanted: {:?}, got: {:?}",
                        wanted_type, supplied_type
                    );
                }

                if resolved_wanted_type.requires_binding() {
                    ctx.scope()?
                        .bindings
                        .last_mut()
                        .map(|m| m.insert(resolved_wanted_type, supplied_type.clone()));
                }
            }

            bytecode.append(&mut bc);
            bytecode.push(Op::Call as u8);

            let return_type = ctx.ts.resolve(*returns)?;
            ctx.pop_type_scope();

            // +9 to account for the length itself (8 bytes) and advance the IP to the byte after
            let length: [u8; std::mem::size_of::<usize>()] = (bytecode.len() + 9).to_ne_bytes();
            Ok((
                vec![Op::SetReturn as u8]
                    .into_iter()
                    .chain(length)
                    .chain(bytecode)
                    .collect(),
                return_type.clone(),
            ))
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
    generics: &Vec<(String, Type)>,
) -> Result<(Vec<u8>, Type)> {
    // If there is a surrounding function type context (like if this function is an argument to
    // a surrounding function), we inherit its generics definitions where we dont make our own
    // TODO this can likely be put in the Scopes
    // {
    //     for (k, v) in ctx.scope()?.generics.iter() {
    //         generics.entry(k.to_string()).or_insert(v.clone());
    //     }
    // }
    ctx.push_type_scope(generics);

    let ty = Type::Function {
        args: args.iter().map(|a| a.1.clone()).collect(),
        returns: Box::new(returns.clone()),
        is_variadic: *is_variadic,
        generics: generics.clone(),
    };

    let mut arity = args.len();

    let current_function_type_copy = ctx.current_function_type.clone();
    ctx.current_function_type = Some(ty.clone());

    let mut scope = Scope::default();
    // scope.generics = generics.clone();

    // If the parent expression is a struct declaration, that means this is a method and so should have
    // an implicit `self` variable injected
    if let Ast::Type {
        name,
        decl: TypeDecl::Struct { .. },
        ..
    } = &ctx.lineage[ctx.lineage.len() - 2]
    {
        arity += 2;

        scope.locals.push(LocalVariable {
            name: "self".to_string(),
            is_captured: false,
            is_mutable: false,
            r#type: Type::TypeVar(name.clone()),
        })
    }

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
        arity,
        bytecode: func_bytecode,
        upvalue_count,
        upvalues: vec![],
        upvalues_bytecode,
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
fn infix(ctx: &mut Context, a: &Ast, op: &Operator, b: &Ast) -> Result<(Vec<u8>, Type)> {
    let (mut bytecode, a_type) = compile(ctx, a)?;
    let (mut bytecode_b, b_type) = compile(ctx, b)?;

    if !a_type.allowed_infix_operators(&b_type).contains(op) {
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

/// Accesses a member within a Struct, Module or Enum.
/// E.g. foo.bar
fn member(
    ctx: &mut Context,
    container: &Box<Ast>,
    key: &Key,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    // Put the containing object on the stack
    // NOTE: In the case of Enums, `bc` will be empty and we just care about `expr_type`.
    let (mut bytecode, container_type) = compile(ctx, container)?;

    match ctx.ts.resolve(container_type.clone())? {
        Type::Struct {
            fields, methods, ..
        } => {
            if let Key::String(key) = key {
                for (idx, (name, ty)) in fields.iter().enumerate() {
                    if name == key {
                        bytecode.push(Op::GetMember as u8);
                        bytecode.push(1_u8);
                        bytecode.push(idx as u8);
                        return Ok((bytecode, ty.clone()));
                    }
                }

                for (name, (ty, idx)) in methods.iter() {
                    if name == key {
                        bytecode.push(Op::GetFunction as u8);
                        bytecode.push(*idx as u8);
                        return Ok((bytecode, ty.clone()));
                    }
                }

                panic!("invalid struct field or method: {}", key)
            } else {
                panic!("cant use non string key for struct access");
            }
        }

        Type::List(ty) => match key {
            Key::Usize(idx) => {
                bytecode.push(Op::GetMember as u8);
                bytecode.push(1_u8);
                bytecode.push(*idx as u8);
                return Ok((bytecode, *ty.clone()));
            }
            Key::Expression(expr) => {
                let (mut key_bytecode, key_type) = compile(ctx, expr)?;
                if key_type != Type::Usize {
                    panic!("lists can only be indexed by usize");
                }
                bytecode.append(&mut key_bytecode);
                bytecode.push(Op::GetMember as u8);
                bytecode.push(0_u8);
                return Ok((bytecode, Type::Unknown));
            }
            x => todo!("{:?}", x),
        },

        // TODO this can probably be handled in a nicer way than being hardcoded here...
        Type::Interface { name, .. } if name == "Index" => match key {
            Key::Expression(expr) => {
                let (mut key_bytecode, _key_type) = compile(ctx, expr)?;
                bytecode.push(Op::GetMember as u8);
                bytecode.append(&mut key_bytecode);
                return Ok((bytecode, Type::Unknown));
            }
            x => todo!("{:?}", x),
        },
        x => todo!("{:?}", x),
    }

    panic!("what");

    Ok((bytecode, Type::Unknown))
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
    let mut bytecode = vec![];
    let mut field_types = vec![];
    let mut methods: HashMap<String, (Type, usize)> = HashMap::default();
    let mut generics = vec![];

    // if named, look up the type and field types
    if let Some(name) = ident {
        // This is a named struct. make sure the type is defined
        if let Type::Struct {
            ref fields,
            generics: ref g,
            methods: ref m,
            ..
        } = ctx.get_type(name)
        {
            field_types = fields.clone();
            methods = m.clone();
            generics = g.clone();
        } else {
            panic!(
                "struct of name {} is not defined at this point or is of the wrong type",
                name
            );
        }
    }

    ctx.push_type_scope(&generics);

    let mut substitutions = vec![];
    for (name, field_ty) in field_types.iter_mut() {
        let (mut bc, actual_field_type) = compile(ctx, fields.get(name).unwrap())?;

        if ctx.ts.resolve(field_ty.clone())? != actual_field_type {
            panic!(
                "type error: field {} expected type {:?}, got {:?}",
                name, field_ty, actual_field_type
            );
        }
        if let Type::TypeVar(ident) = field_ty.clone() {
            substitutions.push((ident, actual_field_type.clone()));
        }

        bytecode.append(&mut bc);
    }

    ctx.push_type_scope(&substitutions);

    let field_types = field_types
        .into_iter()
        .map(|(n, t)| (n, ctx.ts.resolve(t).unwrap()))
        .collect();
    let methods = methods
        .into_iter()
        .map(|(n, (t, idx))| (n, (ctx.ts.resolve(t).unwrap(), idx)))
        .collect();

    ctx.pop_type_scope();
    ctx.pop_type_scope();

    let return_type = Type::Struct {
        name: ident.clone(),
        fields: field_types,
        methods,
        generics,
    };

    bytecode.push(Op::Collect as u8);
    let length: [u8; std::mem::size_of::<usize>()] = fields.len().to_ne_bytes();
    bytecode.append(&mut length.to_vec());
    Ok((bytecode, return_type))
}

/// Declares a new type. The type itself does not emit any bytecode,
/// but struct methods get inserted into our function vtable, which get emitted post compilation.
fn decl_type(
    ctx: &mut Context,
    name: &str,
    decl: &TypeDecl,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    match decl {
        TypeDecl::Enum {
            variants,
            generics: _,
        } => {
            let typ = Type::Enum(variants.clone());
            ctx.add_type(name.to_string(), typ.clone())?;
            ctx.scope()?.types.insert(name.to_string(), typ.clone());
            Ok((vec![], typ))
        }

        TypeDecl::Struct {
            generics,
            fields,
            methods,
        } => {
            ctx.push_type_scope(generics);

            let mut method_vtable_idxs: HashMap<String, usize> = HashMap::default();
            let typ = Type::Struct {
                name: Some(name.to_string()),
                fields: fields.clone(),
                methods: methods
                    .iter()
                    .map(|(name, ast)| {
                        // Reserve an index in the vtable with empty bytecode
                        let vtable_idx = ctx.cache_fn(vec![]);
                        method_vtable_idxs.insert(name.clone(), vtable_idx);

                        (name.clone(), (ast.into(), vtable_idx))
                    })
                    .collect(),
                generics: generics.clone(),
            };

            ctx.add_type(name.to_string(), typ.clone())?;

            for (method_name, ast) in methods.iter() {
                let (fn_bytecode, ty) = compile(ctx, ast)?;

                if Type::from(ast) != ty {
                    panic!("compiled method to different type than ast???");
                }

                ctx.cache_fn_at(*method_vtable_idxs.get(method_name).unwrap(), fn_bytecode);
            }

            ctx.pop_type_scope();

            Ok((vec![], typ))
        }

        TypeDecl::Interface {
            generics,
            properties,
        } => {
            let typ = Type::Interface {
                name: name.to_string(),
                properties: properties.clone(),
                generics: generics.clone(),
            };
            ctx.add_type(name.to_string(), typ.clone())?;
            ctx.scope()?.types.insert(name.to_string(), typ.clone());
            Ok((vec![], typ))
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
        _ => todo!(),
    }
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
    match ctx.ts.resolve(ty)? {
        Type::Unknown => {
            let locals = &mut ctx.scope()?.locals;
            locals.last_mut().unwrap().r#type = assignment_type.clone();
        }
        ty if ty != assignment_type => {
            panic!(
                "attempted to assign value of type {:?} to a variable of type {:?}",
                assignment_type, ty
            );
        }
        ty => {
            assignment_type = ty;
        }
    }

    ctx.assignment_ctx = old_assignment_ctx;
    Ok((assignment_bytes, assignment_type))
}

/// Evaluates a list literal
fn list(ctx: &mut Context, items: &Vec<Ast>, _span: &Range<usize>) -> Result<(Vec<u8>, Type)> {
    let mut bytecode = vec![];
    let length: [u8; std::mem::size_of::<usize>()] = items.len().to_ne_bytes();

    let mut list_type = Type::Unknown;
    for ast in items {
        let (mut bc, item_type) = compile(ctx, ast)?;
        bytecode.append(&mut bc);
        match list_type {
            Type::Unknown => list_type = item_type,
            ty if ty != item_type => {
                panic!(
                    "attempted to use different types in vec! {:?}, {:?}",
                    &ty, &item_type
                );
            }
            _ => {}
        }
    }

    bytecode.push(Op::Collect as u8);
    bytecode.append(&mut length.to_vec());

    Ok((bytecode, Type::List(Box::new(list_type))))
}

/// Raw values get emitted into the bytecode as <usize length><bytes>.
fn value(_ctx: &mut Context, value: &Value) -> Result<(Vec<u8>, Type)> {
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
        dbg!(ident);
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
