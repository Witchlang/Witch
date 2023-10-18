//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
mod util;
use std::collections::HashMap;
use std::ops::Range;

use crate::error::{Error, Result};
use crate::parser::ast::{Ast, Operator};
use crate::types::{Type, TypeDecl};
use witch_runtime::value::Function;
use witch_runtime::{value::Value, vm::Op};

/// Contains all compile-time information about a locally scoped
/// variable.
#[derive(Debug, Clone)]
pub struct LocalVariable {
    name: String,
    scope_depth: usize,
    is_captured: bool,
    r#type: Type,
    is_mutable: bool,
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub locals: Vec<LocalVariable>,
    pub types: HashMap<String, Type>,
}

#[derive(Debug, Default, Clone)]
struct Cached {
    bytecode: Vec<u8>,
    flushed: bool,
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
            functions_cache: Default::default(),
            value_cache: Default::default(),
        }
    }
}

impl Context {
    pub fn scope(&mut self) -> Result<&mut Scope> {
        self.scopes.last_mut().ok_or(Error::fatal())
    }

    /// Flushes the current context.
    /// Unflushed cached values get returned as a `prelude` bytecode and marked as such.
    /// Scopes and AST lineage of the context get reset.
    pub fn flush(&mut self) -> Vec<u8> {
        let mut bc = vec![];
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
            self.functions_cache.len()
        }
    }
}

/// Turns an AST into bytecode
pub fn compile<'a>(ctx: &mut Context, ast: &Ast) -> Result<(Vec<u8>, Type)> {
    ctx.lineage.push(ast.clone());

    let (bytecode, return_type) = match &ast {
        Ast::Assignment { ident, expr, span } => assignment(ctx, ident, expr, span)?,
        Ast::Call { expr, args, span: _ } => call(ctx, expr, args)?,
        Ast::Function {
            is_variadic,
            args,
            returns,
            body,
            generics,
        } => function(ctx, is_variadic, args, returns, body, generics.clone())?,
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

            // let mut bindings = hashbrown::HashMap::new();

            // // Typecheck arguments
            // // `r#type` = wanted type, `arg_type` = supplied type
            // for (idx, wanted_type) in arg_types.into_iter().enumerate() {
            //     let supplied_type = args_with_types[idx].1.clone();

            //     // If variable is in generics, match the supplied type against its constraints
            //     // If its not in generics but is in Types map,
            //     match resolve_binding(
            //         &mut generics,
            //         &mut bindings,
            //         &mut program.compiler_stack,
            //         compiler,
            //         wanted_type.clone(),
            //         Some(&supplied_type),
            //     ) {
            //         Err(e) => {
            //             panic!(
            //                 "wrong type in arg nr {}! wants {:?}, got {:?}",
            //                 idx, wanted_type, supplied_type
            //             );
            //         }
            //         Ok(_) => {}
            //     }
            // }

            // match resolve_binding(
            //     &mut generics,
            //     &mut bindings,
            //     &mut program.compiler_stack,
            //     compiler,
            //     *returns.clone(),
            //     None,
            // ) {
            //     Err(e) => {
            //         panic!("fucked up the return type bruv");
            //     }
            //     Ok(ty) => {
            //         return_type = ty.clone();
            //     }
            // }

            bytecode.append(&mut bc);
            bytecode.push(Op::Call as u8);
            bytecode.push(u8::try_from(args.len()).unwrap());

            Ok((bytecode, *returns))
        }
        _ => panic!("attempted to call a non function???"),
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
        dbg!(&surrounding_generics);
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
            scope_depth: ctx.scopes.len() + 1,
            is_captured: false,
            r#type: arg_type.clone(),
        })
    }
    ctx.scopes.push(scope);

    let (func_bytecode, _actually_returns) = compile(ctx, body)?;

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

    // TODO upvalues
    let function = Value::Function(Function {
        is_variadic: *is_variadic,
        is_method,
        arity: args.len(),
        bytecode: func_bytecode,
        upvalue_count: 0_u8,
        upvalues: vec![],
        upvalues_bytecode: vec![],
    });

    let (function_bytecode, _) = compile(ctx, &Ast::Value(function))?;

    Ok((function_bytecode, ty))
}

/// Expresses a binary operation such as 1 + 1, a == b, 9 > 8, etc.
/// Requres the two expressions to be of the same type.
fn infix(ctx: &mut Context, a: &Box<Ast>, op: &Operator, b: &Box<Ast>) -> Result<(Vec<u8>, Type)> {
    let (mut bytecode, a_type) = compile(ctx, a)?;
    let (mut bytecode_b, _b_type) = compile(ctx, b)?;

    // if !a_type.allowed_infix_operators(&b_type).contains(op) {
    //     panic!(
    //         "infix op {:?} not allowed between {:?} and {:?}",
    //         op, a_type, b_type
    //     );
    // }

    // if !a_type.is_numeric() || !b_type.is_numeric() {
    //     // Handle string concat or mul,
    //     // list concats etc by mapping against builtin functions for dealing with that stuff
    //     todo!();
    // }

    bytecode.append(&mut bytecode_b);
    bytecode.push(Op::Binary as u8);
    bytecode.push(op.to_owned() as u8);

    Ok((bytecode, a_type))
}

/// Accesses a member within an Object or Vector, by first putting the backing Object
/// on the stack and subsequently the Key as an Access Opcode.
fn member(_ctx: &mut Context, _container: &Box<Ast>, _key: &Box<Ast>) -> Result<(Vec<u8>, Type)> {
    Ok((vec![], Type::Unknown))
}

///
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

/// Declares a new type for the current scope. Does not emit any bytecode.
fn decl_type(
    ctx: &mut Context,
    name: &str,
    decl: &TypeDecl,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    let ty = match decl {
        TypeDecl::Enum { variants, generics: _ } => Type::Enum(variants.clone()),
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

/// Assigns a local variable by setting its new value without initializing it.
/// It needs to either be mutable or the parent expr needs to be `Ast::Let`.
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
        scope_depth: ctx.scopes.len(),
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
/// The value gets cached within our Context object and prepended to the
/// final payload. That way, we only need to deserialize it once and can
/// keep on referencing its index within the stack instead.
fn value(ctx: &mut Context, value: &Value) -> Result<(Vec<u8>, Type)> {
    let mut value_bytecode = vec![];
    let mut bytes = util::serialize_value(value.clone())?;
    let length: [u8; std::mem::size_of::<usize>()] = bytes.len().to_ne_bytes();
    value_bytecode.push(Op::Push as u8);
    value_bytecode.append(&mut length.to_vec());
    value_bytecode.append(&mut bytes);

    let return_type = Type::from(value);
    let bc = if let &Type::Function { .. } = &return_type {
        let idx = ctx.cache_fn(value_bytecode);
        [
            vec![Op::GetFunction as u8],
            (idx as u8).to_ne_bytes().to_vec(),
        ]
        .concat()
    } else {
        let idx = ctx.cache_value(value_bytecode);
        [vec![Op::GetValue as u8], (idx as u8).to_ne_bytes().to_vec()].concat()
    };

    Ok((bc, return_type))
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
    } else
    /* if let Some((upvalue, ty)) =
        resolve_upvalue(&mut program.compiler_stack, compiler, ident)
    {
        return_type = ty;
        bytecode.push(OpCode::GetUpvalue as u8);
        bytecode.push(upvalue)
    } else */
    {
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
