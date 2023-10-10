use crate::error::Error;
use witch_runtime::value::Value;

/// Serializes a Value into bytecode
pub fn serialize_value<'a>(value: Value) -> Result<Vec<u8>, Error<'a>> {
    bincode::serde::encode_to_vec(value, bincode::config::legacy())
        .map_err(|e| Error::Generic(e.into()))
}
