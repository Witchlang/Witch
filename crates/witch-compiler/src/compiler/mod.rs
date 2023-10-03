//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
mod util;
use crate::ast::{Ast, Spanned};
use crate::types::Type;
use witch_runtime::vm::BinaryOp;
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

#[derive(Debug, Clone)]
struct Scope {
    pub locals: Vec<LocalVariable>,
}

#[derive(Debug, Default, Clone)]
struct Cached {
    bytecode: Vec<u8>,
    flushed: bool,
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    pub scopes: Vec<Scope>,

    /// Lineage is a straight line of parent relationships from the current
    /// `compile` iteration up the the root node.
    pub lineage: Vec<Spanned<Ast>>,

    /// Values get cached in order keep the subsequent programs smaller
    pub functions_cache: Vec<Cached>,
    pub value_cache: Vec<Cached>,
}

impl Context {
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
        let len: [u8; std::mem::size_of::<usize>()] = len.to_ne_bytes();
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
        return bc;
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
            return idx;
        } else {
            self.value_cache.push(Cached {
                bytecode: value_bytecode,
                flushed: false,
            });
            return self.value_cache.len();
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
            return idx;
        } else {
            self.functions_cache.push(Cached {
                bytecode: fn_bytecode,
                flushed: false,
            });
            return self.functions_cache.len();
        }
    }
}

/// Turns an AST into bytecode
pub fn compile<'a>(
    ctx: &mut Context,
    ast: &Spanned<Ast>,
) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    ctx.lineage.push(ast.clone());

    let (bytecode, return_type) = match &ast.0 {
        // Ast::Assignment { ident, expr } => assignment(ctx, ident, expr)?,
        Ast::BinaryOperation { a, op, b } => binary_operation(ctx, a, op, b)?,
        //  Ast::Member { container, key } => member(ctx, container, key)?,
        Ast::Value(v) => value(ctx, v)?,
        x => todo!("{:?}", x),
    };

    Ok((bytecode, return_type))
}

/// Assigns a local variable by setting its new value without initializing it.
/// It needs to either be mutable or the parent expr needs to be `Ast::Let`.
fn assignment<'a>(
    ctx: &mut Context,
    ident: &str,
    expr: &Box<Spanned<Ast>>,
) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    Ok((vec![], Type::Unknown))
}

/// Expresses a binary operation such as 1 + 1, a == b, 9 > 8, etc.
/// Requres the two expressions to be of the same type.
fn binary_operation<'a>(
    ctx: &mut Context,
    a: &Box<Spanned<Ast>>,
    op: &BinaryOp,
    b: &Box<Spanned<Ast>>,
) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    let (mut bytecode, a_type) = compile(ctx, &a)?;
    let (mut bytecode_b, b_type) = compile(ctx, &b)?;

    // Type check and set our return type for this expression.
    // If either branch is of Unknown type, we will infer them to be the same.
    let return_type = match (&a_type, &b_type) {
        (x, y) if x != y => {
            panic!(
                "cant perform binary op or comparison on different types: {:?} != {:?}, expr: {:?}, {:?}",
                &a_type, &b_type, &a.0, &b.0
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
fn member<'a>(
    ctx: &mut Context,
    container: &Box<Spanned<Ast>>,
    key: &Box<Spanned<Ast>>,
) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    Ok((vec![], Type::Unknown))
}

/// Raw values get emitted into the bytecode as <usize length><bytes>.
/// The value gets cached within our Context object and prepended to the
/// final payload. That way, we only need to deserialize it once and can
/// keep on referencing its index within the stack instead.
fn value<'a>(ctx: &mut Context, value: &Value) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    let mut value_bytecode = vec![];
    let mut bytes = util::serialize_value(value.clone())?;
    let length: [u8; std::mem::size_of::<usize>()] = bytes.len().to_ne_bytes();
    value_bytecode.push(Op::Push as u8);
    value_bytecode.append(&mut length.to_vec());
    value_bytecode.append(&mut bytes);

    let return_type = Type::from(value);
    let bc = if let &Type::Function { .. } = &return_type {
        let idx = ctx.cache_fn(value_bytecode);
        vec![
            vec![Op::GetFunction as u8],
            (idx as u16).to_ne_bytes().to_vec(),
        ]
        .concat()
    } else {
        let idx = ctx.cache_value(value_bytecode);
        vec![
            vec![Op::GetValue as u8],
            (idx as u16).to_ne_bytes().to_vec(),
        ]
        .concat()
    };

    Ok((bc, return_type))
}
