#[cfg(feature = "compiler")]
#[test]
fn it_runs() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::Usize(14);
    let (bytecode, _) = compile(PathBuf::from("tests/fixtures/basic.witch"), None).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}
