use std::env;



use std::path::PathBuf;
use std::process;



pub use witch_compiler::compile;
pub use witch_runtime::vm::Vm;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a file path");
        process::exit(1);
    }

    run(&args[1]);
}

fn run(file_path: &str) {
    let (bytecode, _) = compile(PathBuf::from(file_path), None).unwrap();
    let mut vm = Vm::new();
    let result = vm.run(bytecode).unwrap();
    dbg!(result);
}
