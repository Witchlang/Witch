
#[cfg(feature = "compiler")]
#[test]
fn it_compiles() {
    use std::path::PathBuf;

    let expected: Vec<u8> = vec![];
    let result = witch::compile(PathBuf::from("tests/fixtures/arithmatics.w")).unwrap();
    assert_ne!(expected, result);
}