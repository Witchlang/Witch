use crate::error::Result;
use witch_runtime::value::Value;

/// Serializes a Value into bytecode
pub fn serialize_value<'a>(value: Value) -> Result<Vec<u8>> {
    Ok(bincode::serde::encode_to_vec(value, bincode::config::legacy()).unwrap())
    // todo
    //
}
