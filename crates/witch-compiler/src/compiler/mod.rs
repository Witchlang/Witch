//! The compiler module takes an AST representation of our program and
//! emits bytecode from it.
mod util;
use crate::ast::{Ast, BinaryOp, Spanned};
use crate::types::Type;
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
pub struct Scope {
    pub locals: Vec<LocalVariable>,
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    pub scopes: Vec<Scope>,

    /// Lineage is a straight line of parent relationships from the current
    /// `compile` iteration up the the root node.
    pub lineage: Vec<Spanned<Ast>>,

    /// Raw values to prepend to the stack on startup
    pub values: Vec<Vec<u8>>,
}

/// Turns an AST into bytecode
pub fn compile<'a>(
    ctx: &mut Context,
    ast: &Spanned<Ast>,
) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    ctx.lineage.push(ast.clone());

    let (bytecode, return_type) = match &ast.0 {
        //  Ast::Access { container, key } => access(ctx, container, key)?,
        // Ast::Assignment { ident, expr } => assignment(ctx, ident, expr)?,
        Ast::BinaryOperation { a, op, b } => binary_operation(ctx, a, op, b)?,
        Ast::Value(v) => value(ctx, v)?,
        x => todo!("{:?}", x),
    };

    Ok((bytecode, return_type))
}

/// Accesses an entry within an Object or Vector, by first putting the backing Object
/// on the stack and subsequently the Key as an Access Opcode.
fn access<'a>(
    ctx: &mut Context,
    container: &Box<Spanned<Ast>>,
    key: &Box<Spanned<Ast>>,
) -> Result<(Vec<u8>, Type), crate::error::Error<'a>> {
    Ok((vec![], Type::Unknown))
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

    if ctx.values.contains(&value_bytecode) {
        let idx = ctx
            .values
            .iter()
            .position(|v| v == &value_bytecode)
            .unwrap();
        Ok((
            vec![
                vec![Op::GetValue as u8],
                (idx as u16).to_ne_bytes().to_vec(),
            ]
            .concat(),
            return_type,
        ))
    } else {
        ctx.values.push(value_bytecode.clone());
        Ok((value_bytecode, return_type))
    }
}
