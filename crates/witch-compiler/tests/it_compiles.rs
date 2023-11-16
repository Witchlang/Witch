#[test]
fn complex() {
    use std::path::PathBuf;
    let expected: Vec<u8> = vec![41];
    let result = witch_compiler::compile(PathBuf::from("tests/fixtures/complex.witch")).unwrap();
    assert_eq!(expected, result);
}

// // #[test]
// // fn it_compiles_with_state() {
// //     use std::path::PathBuf;
// //     let expected: Vec<u8> = vec![];
// //     let (result, state) =
// //         witch_compiler::compile(PathBuf::from("tests/fixtures/arithmatics.w"), None).unwrap();
// //     let (result2, state2) =
// //         witch_compiler::compile(PathBuf::from("tests/fixtures/arithmatics.w"), Some(state))
// //             .unwrap();
// //     assert_eq!(expected, result2);
// // }
