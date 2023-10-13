//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
mod util;
use std::collections::HashMap;
use std::ops::Range;

use crate::error::Result;
use crate::parser::ast::{Ast, Operator};
use crate::types::{Type, TypeDecl};
use witch_runtime::{value::Value, vm::Op};

/// Contains all compile-time information about a locally scoped
/// variable.
#[derive(Debug, Clone)]
pub struct LocalVariable {
    name: String,
    scope_depth: usize,
    is_captured: bool,
    r#type: Type,
    mutable: bool,
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

    /// Values get cached in order keep the subsequent programs smaller
    pub functions_cache: Vec<Cached>,
    pub value_cache: Vec<Cached>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            lineage: Default::default(),
            functions_cache: Default::default(),
            value_cache: Default::default(),
        }
    }
}

impl Context {
    pub fn scope(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
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
        dbg!(&len, &bc);
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
        // Ast::Assignment { ident, expr } => assignment(ctx, ident, expr)?,
        Ast::Infix { lhs, op, rhs, .. } => binary_operation(ctx, lhs, op, rhs)?,
        //  Ast::Member { container, key } => member(ctx, container, key)?,
        Ast::Statement { stmt, rest, span } => statement(ctx, stmt.clone(), rest.clone(), span)?,
        Ast::Type { name, decl, span } => decl_type(ctx, name, decl, span)?,
        Ast::Value(v) => value(ctx, v)?,
        x => todo!("{:?}", x),
    };

    Ok((bytecode, return_type))
}

/// Assigns a local variable by setting its new value without initializing it.
/// It needs to either be mutable or the parent expr needs to be `Ast::Let`.
fn assignment(_ctx: &mut Context, _ident: &str, _expr: &Box<Ast>) -> Result<(Vec<u8>, Type)> {
    Ok((vec![], Type::Unknown))
}

/// Expresses a binary operation such as 1 + 1, a == b, 9 > 8, etc.
/// Requres the two expressions to be of the same type.
fn binary_operation(
    ctx: &mut Context,
    a: &Box<Ast>,
    op: &Operator,
    b: &Box<Ast>,
) -> Result<(Vec<u8>, Type)> {
    let (mut bytecode, a_type) = compile(ctx, a)?;
    let (mut bytecode_b, b_type) = compile(ctx, b)?;

    // Type check and set our return type for this expression.
    // If either branch is of Unknown type, we will infer them to be the same.
    let return_type = match (&a_type, &b_type) {
        (x, y) if x != y => {
            panic!(
                "cant perform binary op or comparison on different types: {:?} != {:?}, expr: {:?}, {:?}",
                &a_type, &b_type, &a, &b
            );
        }
        (x, _) => x.to_owned(),
    };

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

/// Compiles a statement and optionally the rest of the program.
fn statement(
    ctx: &mut Context,
    stmt: Box<Ast>,
    rest: Box<Ast>,
    span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    if let Ast::Nop = *rest {
        let (mut bytecode, ty) = compile(ctx, &stmt)?;
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
    span: &Range<usize>,
) -> Result<(Vec<u8>, Type)> {
    let ty = match decl {
        TypeDecl::Enum { variants, generics } => Type::Enum(variants.clone()),
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
