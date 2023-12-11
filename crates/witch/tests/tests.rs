#[cfg(feature = "compiler")]
#[test]
fn basic() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::String("HelloHelloHello".to_string());
    let bytecode = compile(PathBuf::from("tests/fixtures/basic.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn builtins() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let bytecode = compile(PathBuf::from("tests/fixtures/builtins.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    // We run libc's "puts" which returns any positive number on success
    assert!(matches!(result, Value::Usize(x) if x > 0));
}

#[cfg(feature = "compiler")]
#[test]
fn fib() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::Usize(55);
    let bytecode = compile(PathBuf::from("tests/fixtures/fib.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn generic_functions() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::String("123".to_string());
    let bytecode = compile(PathBuf::from("tests/fixtures/generic_functions.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn modules() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::String("Hello from module".into());
    let bytecode = compile(PathBuf::from("tests/fixtures/module.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn lambdas() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::Usize(5);
    let bytecode = compile(PathBuf::from("tests/fixtures/lambda.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn closures() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::Usize(30);
    let bytecode = compile(PathBuf::from("tests/fixtures/closures.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn lists() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::Usize(3);
    let bytecode = compile(PathBuf::from("tests/fixtures/lists.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}

#[cfg(feature = "compiler")]
#[test]
fn types() {
    use std::path::PathBuf;

    use witch::Vm;
    use witch_compiler::compile;
    use witch_runtime::value::Value;

    let expected = Value::Usize(3);
    let bytecode = compile(PathBuf::from("tests/fixtures/types.witch")).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    assert_eq!(expected, result);
}
