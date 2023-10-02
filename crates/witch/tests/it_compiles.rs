#[cfg(feature = "compiler")]
#[test]
fn it_compiles() {
    use std::path::PathBuf;
    let expected: Vec<u8> = vec![];
    let bytecode = compile(PathBuf::from("tests/fixtures/arithmatics.w")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode);
    assert_eq!(expected, result);
}
