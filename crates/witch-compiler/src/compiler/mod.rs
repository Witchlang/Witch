//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
pub mod context;
mod type_system;
mod util;
use std::collections::HashMap;
use std::ops::Range;
use std::path::PathBuf;

use crate::error::Result;

use context::{Context, Scope};
use witch_parser::ast::{Ast, Key, Operator};
use witch_parser::pattern::Pattern;
use witch_parser::types::{EnumVariant, Type, TypeDecl};

use witch_runtime::value::{Function, Value};
use witch_runtime::vm::Op;

/// Contains all compile-time information about a locally scoped
/// variable.
#[derive(Debug, Clone, PartialEq)]
pub struct LocalVariable {
    pub name: String,
    pub is_captured: bool,
    pub r#type: Type,
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
        Ast::Case { expr, cases, span } => case(ctx, expr, cases, span)?,
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
        Ast::Import { path, span } => import(ctx, path, span)?,
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
        Ast::Nop => (vec![], Type::Void),
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

    if let Some((local_variable, local)) = ctx.get_local(&ident) {
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

    let (mut bc, mut called_type) = compile(ctx, expr)?;

    // If we're calling an enum constructor, bail early and run that instead
    if let Type::EnumVariant(variant) = called_type {
        return construct_enum(ctx, variant, args);
    }

    let mut args_bytecode = vec![];
    let mut args_with_types = vec![];
    for arg in args.iter() {
        let (mut bc, arg_type) = compile(ctx, arg)?;
        args_bytecode.append(&mut bc);
        args_with_types.push((arg.clone(), arg_type));
    }

    // If we're calling a function stub, it needs to undergo monomorphization
    if let Type::GenericFunctionStub { scope, idx } = called_type {
        let (_, mut function_ast) = ctx.scope_by_index(scope)?.generic_functions[idx].clone();
        if let Ast::Function {
            ref is_variadic,
            ref args,
            ref returns,
            ref body,
            ref mut generics,
        } = function_ast
        {
            ctx.push_type_scope(&generics);
            for (caller_arg_idx, (_, caller_arg_typ)) in args_with_types.iter().enumerate() {
                if let Type::TypeVar(ident) = args[caller_arg_idx].1.clone() {
                    // If the arg is a typevar, check the function generics for it

                    // If theres a match, replace with our caller type (or error if wrong type)
                    if let Some((i, (n, t))) =
                        generics.iter_mut().enumerate().find(|(_, g)| g.0 == ident)
                    {
                        // Caller type matches the generic constraint
                        if caller_arg_typ == &ctx.ts.resolve(t.to_owned())? {
                            generics[i] = (n.to_owned(), caller_arg_typ.clone());
                        } else {
                            panic!(
                                "type error: generic function arg {} expected type {:?}, got {:?}",
                                args[caller_arg_idx].0,
                                ctx.ts.resolve(args[caller_arg_idx].1.clone())?,
                                caller_arg_typ
                            );
                        }
                    }
                }

                ctx.pop_type_scope();
            }

            // Compile the new function AST
            let (impl_bytecode, impl_type) = compile(ctx, &function_ast)?;

            // Cache it
            let impl_idx = ctx.cache_fn(impl_bytecode);
            // Emit a getfunction OP to put it on the stack
            bc = vec![];
            bc.push(Op::GetFunction as u8);
            bc.push((ctx.vtable_offset() + impl_idx) as u8);

            called_type = impl_type;

            // And keep going
        }
    }

    // If this is being called on a Member expression, that means a method call.
    // Method call means we have an implicit `self` variable as a first argument.
    // Let's stick the Entry object on the stack before the arguments then.
    if let Ast::Member { container, .. } = *expr.clone() {
        if let Type::Function { is_method, .. } = &called_type {
            if *is_method {
                let (mut self_bc, ty) = compile(ctx, &container)?;
                bytecode.append(&mut self_bc);
                arity += 1;
            }
        }
    }

    bytecode.append(&mut args_bytecode);

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
            is_method,
        } => {
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

            // Type check arguments and update the `bindings` map with what our generics correspond to for this call
            for (idx, wanted_type) in arg_types.into_iter().enumerate() {
                let supplied_type = args_with_types[idx].1.clone();

                let resolved_wanted_type = ctx.ts.resolve(wanted_type.clone())?;

                dbg!(&resolved_wanted_type, &supplied_type);

                if resolved_wanted_type != supplied_type {
                    panic!(
                        "type error in function call, wanted: {:?}, got: {:?}",
                        wanted_type, supplied_type
                    );
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

fn construct_enum(
    ctx: &mut Context,
    mut variant: EnumVariant,
    args: &Vec<Ast>,
) -> Result<(Vec<u8>, Type)> {
    let mut bytecode = vec![];
    let mut args_with_types = vec![];
    for arg in args.iter() {
        let (mut bc, arg_type) = compile(ctx, arg)?;
        bytecode.append(&mut bc);
        args_with_types.push((arg.clone(), arg_type));
    }

    let generics = if let Some(Type::Enum { generics, .. }) = ctx.get_type(&variant.parent) {
        generics
    } else {
        vec![]
    };
    ctx.push_type_scope(&generics);

    // Compare arguments length against the type.
    if args.len() != variant.types.len() {
        panic!(
            "wrong amount of enum field arguments. want: {:?}, got: {:?}",
            &variant.types.len(),
            &args.len()
        );
    }

    let mut resolved_types = vec![];

    // Type check arguments
    for (idx, wanted_type) in variant.types.clone().into_iter().enumerate() {
        let supplied_type = args_with_types[idx].1.clone();

        dbg!(&supplied_type);

        let resolved_wanted_type = ctx.ts.resolve(wanted_type.clone())?;

        if resolved_wanted_type != supplied_type {
            panic!(
                "type error in enum constructor call, wanted: {:?}, got: {:?}",
                wanted_type, supplied_type
            );
        }

        resolved_types.push(supplied_type);
    }

    dbg!(&resolved_types);

    bytecode.push(Op::ConstructEnum as u8);
    bytecode.push(variant.discriminant as u8);
    bytecode.push(variant.types.len() as u8);

    // Construct return type
    variant.types = resolved_types;
    let mut return_type = ctx.get_type(&variant.parent).expect("oops");
    if let Type::Enum {
        ref mut variants, ..
    } = return_type
    {
        *variants
            .iter_mut()
            .find(|v| v.name == variant.name)
            .expect("oops") = variant.clone();
    }

    ctx.pop_type_scope();

    return Ok((bytecode, return_type));
}

fn case(
    ctx: &mut Context,
    expr: &Ast,
    cases: &Vec<(Pattern, Ast)>,
    span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    dbg!(cases);
    todo!();
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
    let is_method = if let Ast::Type {
        decl: TypeDecl::Struct { .. },
        ..
    } = &ctx.lineage[ctx.lineage.len() - 2]
    {
        true
    } else {
        false
    };

    ctx.push_type_scope(generics);

    // If this is a generic function which is not a method (because methods have their generics handled by the encompassing struct)
    // and not all type variables are concrete, we stash this away for later monomorphization instead of compiling it.
    if !is_method && generics.iter().any(|(_, g)| ctx.ts.is_abstract(g)) {
        ctx.pop_type_scope();
        ctx.scope()?.generic_functions.push((
            "<generic function name placeholder>".to_string(),
            Ast::Function {
                is_variadic: *is_variadic,
                args: args.to_vec(),
                returns: returns.clone(),
                body: body.clone(),
                generics: generics.to_vec(),
            },
        ));
        return Ok((
            vec![],
            Type::GenericFunctionStub {
                scope: ctx.scopes.len() - 1,
                idx: ctx.scope()?.generic_functions.len() - 1,
            },
        ));
    }

    let ty = Type::Function {
        args: args.iter().map(|a| a.1.clone()).collect(),
        returns: Box::new(returns.clone()),
        is_variadic: *is_variadic,
        generics: generics.clone(),
        is_method,
    };

    let mut arity = args.len();

    let current_function_type_copy = ctx.current_function_type.clone();
    ctx.current_function_type = Some(ty.clone());

    let mut scope = Scope::default();

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
            r#type: Type::TypeVar(name.clone()),
        })
    }

    for (arg_name, arg_type) in args.iter() {
        scope.locals.push(LocalVariable {
            name: arg_name.clone(),
            is_captured: false,
            r#type: arg_type.clone(),
        })
    }
    ctx.scopes.push(scope);

    let (func_bytecode, _actually_returns) = compile(ctx, body)?;

    let mut upvalues_bytecode = vec![];
    // If the parent scope is the root scope (i.e. we have a scope len of 2), take the previous modules stacks into account for the
    // location this upvalue is pointing to
    let offset = if ctx.scopes.len() == 2 {
        ctx.stack_offset()
    } else {
        0
    };
    ctx.scope()?.upvalues.clone().iter().for_each(|u| {
        if u.is_local {
            upvalues_bytecode.push(1_u8);
        } else {
            upvalues_bytecode.push(0_u8);
        }
        upvalues_bytecode.push((offset + u.index) as u8); // todo allow largers size here?
    });

    ctx.scopes.pop();

    // TODO typecheck returns
    // if the provided return type is more loose than the actually_returns type, overwrite it

    ctx.current_function_type = current_function_type_copy;

    let function = Value::Function(Function {
        is_variadic: *is_variadic,
        arity,
        upvalues_bytecode,
    });

    let (mut function_bytecode, _) = compile(ctx, &Ast::Value(function))?;

    let length: [u8; std::mem::size_of::<usize>()] = func_bytecode.len().to_ne_bytes();
    function_bytecode.append(&mut length.to_vec());
    function_bytecode.append(&mut func_bytecode.to_vec());

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

fn import(ctx: &mut Context, path: &PathBuf, _span: &Range<usize>) -> Result<(Vec<u8>, Type)> {
    panic!("DEPRECATED??");
    // Make sure the module is available to us
    //let module = ctx.resolve_import(path.clone())?;

    // Add the module as a local variable in the current scope
    // ctx.scope()?.locals.push(LocalVariable {
    //     name: module.name(),
    //     is_captured: false,
    //     r#type: module.r#type(),
    // });

    Ok((vec![], Type::Unknown))
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
                        bytecode.push((ctx.vtable_offset() + *idx) as u8);
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
                Ok((bytecode, *ty.clone()))
            }
            Key::Expression(expr) => {
                let (mut key_bytecode, key_type) = compile(ctx, expr)?;
                if key_type != Type::Usize {
                    panic!("lists can only be indexed by usize");
                }
                bytecode.append(&mut key_bytecode);
                bytecode.push(Op::GetMember as u8);
                bytecode.push(0_u8);
                Ok((bytecode, Type::Unknown))
            }
            x => todo!("{:?}", x),
        },

        Type::Module { path } => match key {
            Key::String(ident) => {
                // Get the module from ctx
                if let Some(module) = ctx.get_module(&path) {
                    for (i, local) in module.locals.iter().enumerate() {
                        if local.name == *ident {
                            if let Ok(idx) = u8::try_from(module.stack_offset + i) {
                                let return_type = local.r#type.clone();
                                return Ok((vec![Op::Get as u8, idx], return_type));
                            }
                            unreachable!()
                        }
                    }
                    panic!("unknown variable in module {:?}", path);
                } else {
                    panic!("unknown module {:?}", path);
                }
            }
            _ => panic!("cant retrieve module symbol by kkey other than string"),
        },

        // TODO this can probably be handled in a nicer way than being hardcoded here...
        Type::Interface { name, .. } if name == "Index" => match key {
            Key::Expression(expr) => {
                let (mut key_bytecode, _key_type) = compile(ctx, expr)?;
                bytecode.push(Op::GetMember as u8);
                panic!();
                bytecode.append(&mut key_bytecode);
                Ok((bytecode, Type::Unknown))
            }
            x => todo!("{:?}", x),
        },
        x => todo!("{:?}", x),
    }
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

    // If the statement is not an assigment or declaration, pop it off the stack afterwards
    if !matches!(
        *stmt,
        Ast::Let { .. }
            | Ast::Assignment { .. }
            | Ast::Type { .. }
            | Ast::Block(_)
            | Ast::If { .. }
            | Ast::Break
            | Ast::Continue
            | Ast::While { .. }
    ) {
        bytecode.push(Op::Pop as u8);
    }

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
        if let Some(Type::Struct {
            ref fields,
            generics: ref g,
            methods: ref m,
            ..
        }) = ctx.get_type(name)
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
        TypeDecl::Enum { variants, generics } => {
            let typ = Type::Enum {
                variants: variants.to_owned(),
                generics: generics.to_owned(),
            };
            ctx.add_type(name.to_string(), typ.clone())?;
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

                        // The AST dont know whether the function is a method or not. Make sure it is.
                        let mut mtype = ast.into();
                        if let Type::Function {
                            ref mut is_method, ..
                        } = mtype
                        {
                            *is_method = true;
                        }

                        (name.clone(), (mtype, vtable_idx))
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
            Ok((vec![], typ))
        }
    }
}

/// Creates a new local variable.
fn let_(
    ctx: &mut Context,
    ident: &str,
    annotated_type: &Option<Type>,
    expr: &Ast,
    _span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    // Create a new local var of type `ty`
    let ty = annotated_type.clone().unwrap_or(Type::Unknown);
    let old_assignment_ctx = ctx.assignment_ctx.clone();
    ctx.assignment_ctx = Some((ident.to_owned(), ty.clone()));
    let local_variable = LocalVariable {
        name: ident.to_string(),
        is_captured: false,
        r#type: ty.clone(),
    };
    ctx.scope()?.locals.push(local_variable);

    let (assignment_bytes, mut assignment_type) = compile(ctx, expr)?;

    if let Type::GenericFunctionStub { scope, idx } = assignment_type.clone() {
        ctx.scope_by_index(scope)?.generic_functions[idx].0 = ident.to_owned();
        ctx.scope()?.locals.pop();
        return Ok((vec![], assignment_type));
    }

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
    let mut local_variable = ctx.get_local(ident);

    // If not found, check the <prelude> module as well. It has no stack offset since its.. the prelude.
    if local_variable.is_none() {
        let prelude = ctx
            .get_module(&PathBuf::from("<prelude>"))
            .expect("what no prelude");
        for (i, local) in prelude.locals.iter().enumerate() {
            if local.name == *ident {
                local_variable = Some((i, local.to_owned()));
                break;
            }
        }
    }

    if let Some(local_variable) = local_variable {
        if let Ok(idx) = u8::try_from(local_variable.0) {
            let return_type = local_variable.1.r#type.clone();
            return Ok((vec![Op::Get as u8, idx], return_type));
        }
        unreachable!()
    } else if let Some((scope, idx)) = ctx.resolve_generic_function(ident) {
        return Ok((vec![], Type::GenericFunctionStub { scope, idx }));
    } else if let Some((idx, return_type)) = ctx.resolve_upvalue(ident, ctx.scopes.len() - 1)? {
        return Ok((vec![Op::GetUpvalue as u8, idx as u8], return_type));
    } else if let Some((idx, return_type)) = ctx.get_builtin(ident) {
        return Ok((vec![Op::GetBuiltin as u8, idx as u8], return_type));
    } else if let Some(typ) = ctx.get_type(ident) {
        // If the "var" is actually a type, it can be a couple of things.
        // 1) An enum variant, which is either a *variant constructor* if it holds other values,
        // or 2) just a value itself if it doesnt

        match typ.clone() {
            // Type constructor
            Type::EnumVariant(EnumVariant { types, .. }) if types.len() > 0 => {
                // Make sure we get called and with the right amount of arguments.
                if !matches!(ctx.lineage[ctx.lineage.len() - 2], Ast::Call { .. }) {
                    panic!("this type can only be used as a constructor in a calling context");
                }

                return Ok((vec![], typ));
            }
            // Enum value, no constructing needed
            Type::EnumVariant(variant) => {
                let (bc, _) = compile(
                    ctx,
                    &Ast::Value(Value::Enum {
                        discriminant: variant.discriminant,
                        values: vec![],
                    }),
                )?;
                return Ok((bc, typ));
            }
            _ => {
                panic!("using types other than enum variants in this placement is not supported!");
            }
        }
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
