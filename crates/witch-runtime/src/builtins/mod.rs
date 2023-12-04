use crate::{stack::Entry, value::Value, vm::Vm};
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::convert::{From, Into};
use paste::paste;
mod sys;
use sys::*;

#[derive(Debug)]
pub struct BuiltinInfo {
    pub name: &'static str,
    pub inputs: &'static str,
    pub output: &'static str,
}

macro_rules! builtins {

    ($($idents:ident),*) => {

        #[cfg(not(feature = "compiler"))]
        pub fn builtins() -> Vec<Builtin> {
            vec![$(Builtin::new(Box::new($idents))),*]
        }

        #[cfg(feature = "compiler")]
        pub fn builtins() -> Vec<Builtin> {
            paste! { vec![$(Builtin::new(Box::new([<$idents _fn>]))),*] }
        }

        #[cfg(feature = "compiler")]
        pub fn builtins_info() -> Vec<BuiltinInfo> {
            vec![$($idents),*]
        }

    };
}

builtins! {
    witch_libc_puts
}

pub struct Builtin(pub Handler);
impl Builtin {
    pub fn new<Args: 'static>(func: Box<dyn Function<Args>>) -> Self {
        Self(Arc::new(move |vm| {
            (*func).fn_call(vm);
        }))
    }
}

type Handler = Arc<dyn Fn(&mut Vm) + Send + Sync>;

pub trait Function<Args>: 'static + Send + Sync {
    fn fn_call(&self, vm: &mut Vm);
}

impl<Func, Return> Function<()> for Func
where
    Func: 'static + Send + Sync + Fn(&mut Vm) -> Return,
    Return: Into<Value>,
{
    fn fn_call(&self, vm: &mut Vm) {
        let ret = self(vm);

        let return_value = Into::<Value>::into(ret);
        vm.push_value(return_value);
    }
}

impl<Func, A, Return> Function<A> for Func
where
    Func: 'static + Send + Sync + Fn(&mut Vm, A) -> Return,
    Return: Into<Value>,
    A: From<Value>,
{
    fn fn_call(&self, vm: &mut Vm) {
        let a = vm.pop_value().unwrap().into();
        let ret = self(vm, a);

        let return_value = Into::<Value>::into(ret);
        vm.push_value(return_value);
    }
}

impl<Func, A, B, Return> Function<(A, B)> for Func
where
    Func: 'static + Send + Sync + Fn(&mut Vm, A, B) -> Return,
    Return: Into<Value>,
    A: From<Entry>,
    B: From<Entry>,
{
    fn fn_call(&self, vm: &mut Vm) {
        let b = vm.stack.pop().unwrap().into();
        let a = vm.stack.pop().unwrap().into();
        let ret = self(vm, a, b);

        let return_value = Into::<Value>::into(ret);
        vm.push_value(return_value);
    }
}

// impl<Func, A, B, C, Return> Function<(A, B, C)> for Func
// where
//     Func: 'static + Send + Sync + Fn(&mut Vm, A, B, C) -> Return,
//     Return: Into<Value>,
//     A: From<Value>,
//     B: From<Value>,
//     C: From<Value>,
// {
//     fn fn_call(&self, vm: &mut Vm) {
//         let c = vm.stack.pop_value().into();
//         let b = vm.stack.pop_value().into();
//         let a = vm.stack.pop_value().into();
//         let ret = self(vm, a, b, c);

//         let return_value = Into::<Value>::into(ret);
//         vm.push_value(return_value);
//     }
// }

// impl<Func, A, B, C, D, Return> Function<(A, B, C, D)> for Func
// where
//     Func: 'static + Send + Sync + Fn(&mut Vm, A, B, C, D) -> Return,
//     Return: Into<Value>,
//     A: From<Entry>,
//     B: From<Entry>,
//     C: From<Entry>,
//     D: From<Entry>,
// {
//     fn fn_call(&self, vm: &mut Vm) {
//         let d = vm.stack.pop().unwrap().into();
//         let c = vm.stack.pop().unwrap().into();
//         let b = vm.stack.pop().unwrap().into();
//         let a = vm.stack.pop().unwrap().into();
//         let ret = self(vm, a, b, c, d);

//         let return_value = Into::<Value>::into(ret);
//         vm.push_value(return_value);
//     }
// }

// impl<Func, A, B, C, D, E, Return> Function<(A, B, C, D, E)> for Func
// where
//     Func: 'static + Send + Sync + Fn(&mut Vm, A, B, C, D, E) -> Return,
//     Return: Into<Value>,
//     A: From<Entry>,
//     B: From<Entry>,
//     C: From<Entry>,
//     D: From<Entry>,
//     E: From<Entry>,
// {
//     fn fn_call(&self, vm: &mut Vm) {
//         let e = vm.stack.pop().unwrap().into();
//         let d = vm.stack.pop().unwrap().into();
//         let c = vm.stack.pop().unwrap().into();
//         let b = vm.stack.pop().unwrap().into();
//         let a = vm.stack.pop().unwrap().into();
//         let ret = self(vm, a, b, c, d, e);

//         let return_value = Into::<Value>::into(ret);
//         vm.push_value(return_value);
//     }
// }

// impl<Func, A, B, C, D, E, F, Return> Function<(A, B, C, D, E, F)> for Func
// where
//     Func: 'static + Send + Sync + Fn(&mut Vm, A, B, C, D, E, F) -> Return,
//     Return: Into<Value>,
//     A: From<Entry>,
//     B: From<Entry>,
//     C: From<Entry>,
//     D: From<Entry>,
//     E: From<Entry>,
//     F: From<Entry>,
// {
//     fn fn_call(&self, vm: &mut Vm) {
//         let f = vm.stack.pop().unwrap().into();
//         let e = vm.stack.pop().unwrap().into();
//         let d = vm.stack.pop().unwrap().into();
//         let c = vm.stack.pop().unwrap().into();
//         let b = vm.stack.pop().unwrap().into();
//         let a = vm.stack.pop().unwrap().into();
//         let ret = self(vm, a, b, c, d, e, f);

//         let return_value = Into::<Value>::into(ret);
//         vm.push_value(return_value);
//     }
// }
