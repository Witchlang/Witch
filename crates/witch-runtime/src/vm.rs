use crate::alloc::borrow::ToOwned;
use core::cell::RefCell;

#[cfg(feature = "profile")]
use std::collections::HashMap;

use crate::{builtins, dbg};

use alloc::rc::Rc;
use alloc::vec;
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};

use crate::builtins::Builtin;
use crate::heap::Heap;
use crate::stack::{Entry, Function as StackFunction, Pointer, Stack};
use crate::value::Value;

#[derive(Debug)]
enum Upvalue {
    /// If its closed over, it contains a pointer to the value on the heap
    Closed(Pointer),

    /// If its not yet closed over, it contains the stack index of where the value resides
    Open(usize),
}

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

impl core::convert::From<u8> for InfixOp {
    fn from(byte: u8) -> Self {
        match byte {
            0 => InfixOp::Add,
            1 => InfixOp::Sub,
            2 => InfixOp::Mul,
            3 => InfixOp::Div,
            4 => InfixOp::Mod,
            5 => InfixOp::Eq,
            6 => InfixOp::NotEq,
            7 => InfixOp::Lt,
            8 => InfixOp::Lte,
            9 => InfixOp::Gt,
            10 => InfixOp::Gte,
            11 => InfixOp::And,
            12 => InfixOp::Or,
            _ => todo!(),
        }
    }
}

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
#[repr(u8)]
pub enum Op {
    SetupModule,
    SetupFunctionCache,
    GetModuleSymbol,
    GetValue,
    GetFunction,
    GetBuiltin,

    Push,
    Pop,
    Get,
    GetUpvalue,
    GetMember,
    Set,
    SetProperty,

    SetReturn,
    Jump,
    JumpIfFalse,

    Binary,
    Return,
    Call,

    Collect,

    Debug,

    Crash,
}

impl core::convert::From<u8> for Op {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Op::SetupModule,
            1 => Op::SetupFunctionCache,
            2 => Op::GetModuleSymbol,
            3 => Op::GetValue,
            4 => Op::GetFunction,
            5 => Op::GetBuiltin,

            6 => Op::Push,
            7 => Op::Pop,
            8 => Op::Get,
            9 => Op::GetUpvalue,
            10 => Op::GetMember,
            11 => Op::Set,
            12 => Op::SetProperty,

            13 => Op::SetReturn,
            14 => Op::Jump,
            15 => Op::JumpIfFalse,

            16 => Op::Binary,
            17 => Op::Return,
            18 => Op::Call,

            19 => Op::Collect,

            20 => Op::Debug,

            _ => Op::Crash,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CallFrame {
    pub ip: usize,
    pub upvalues_refs_idx: usize,
    stack_start: usize,
}

pub struct Vm {
    pub stack: Stack,
    pub heap: Heap,
    frames: Vec<CallFrame>,

    modules: Vec<Stack>,

    /// The bytecode of the current callframe. Only used for lookups (next N bytes, etc..)
    bytecode: Vec<u8>,

    builtins: Vec<Builtin>,

    /// Our function vtable. Struct methods go here.
    functions: Vec<StackFunction>,

    /// A list of references to upvalue slots. Each item in upvalue_refs correspond to a function.
    upvalue_refs: Vec<Vec<usize>>,

    /// A vec of pointers to the Heap, where we store our upvalues for closures
    upvalues: Vec<Upvalue>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

pub fn hej(_vm: &mut Vm) {
    dbg!("hej");
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            heap: Heap::default(),
            modules: vec![],
            frames: vec![],
            bytecode: vec![],
            builtins: builtins::builtins(),
            functions: vec![],
            upvalue_refs: vec![],
            upvalues: vec![],
        }
    }

    /// Retrieves the current (topmost) CallFrame.
    pub fn frame(&self) -> &CallFrame {
        if !self.frames.is_empty() {
            &self.frames[self.frames.len() - 1]
        } else {
            unreachable!()
        }
    }

    /// Retrieves a mutable reference to the current (topmost) CallFrame.
    pub fn frame_mut(&mut self) -> &mut CallFrame {
        let idx = &self.frames.len() - 1;
        &mut self.frames[idx]
    }

    /// Retrieves the byte which the instruction pointer is currently pointing at.
    fn current_byte(&mut self) -> u8 {
        self.bytecode[self.frame().ip]
    }

    /// Retrieves the byte which is after the byte that the instruction pointer is currently pointing at.
    fn next_byte(&mut self) -> u8 {
        self.bytecode[self.frame().ip + 1]
    }

    fn next_two_bytes(&mut self) -> [u8; 2] {
        [
            self.bytecode[self.frame().ip + 1],
            self.bytecode[self.frame().ip + 2],
        ]
    }

    /// Retrieves the next 8 bytes from the current instruction pointer.
    fn next_eight_bytes(&mut self) -> [u8; 8] {
        self.bytecode[self.frame().ip + 1..self.frame().ip + 1 + 8]
            .try_into()
            .unwrap()
    }

    fn entry_to_value_ref(&mut self, entry: Entry) -> Rc<RefCell<Value>> {
        match entry {
            Entry::Pointer(Pointer::Heap(idx)) => self.heap.get(idx),
            Entry::Usize(u) => Rc::new(RefCell::new(Value::Usize(u))),
            x => todo!("{:?}", x),
        }
    }

    pub fn entry_to_value(&mut self, entry: Entry) -> Value {
        self.entry_to_value_ref(entry).borrow().clone()
    }

    pub fn pop_value(&mut self) -> Option<Value> {
        let entry = self.stack.pop();
        if let Some(entry) = entry {
            return Some(self.entry_to_value(entry));
        }
        None
    }

    /// Pushes a Value onto the stack
    pub fn push_value(&mut self, value: Value) {
        let entry = match value {
            Value::Bool(x) => Entry::Bool(x),
            Value::Usize(x) => Entry::Usize(x),
            value => Entry::Pointer(Pointer::Heap(self.heap.insert(value))),
        };
        self.stack.push(entry);
    }

    /// Moves a stack entry to the heap and stashes a copy of the pointer
    /// among our `upvalues` to be referenced by a closure at a later time
    /// TODO Make it less naive so we dont capture upvalues more than once if necessary
    fn capture_upvalue(&mut self, idx: usize) -> usize {
        self.upvalues.push(Upvalue::Open(idx));
        self.upvalues.len() - 1
    }

    /// Moves a stack entry to the heap and stashes a copy of the pointer
    /// among our `upvalues` to be referenced by a closure at a later time
    fn close_upvalues(&mut self, frame: CallFrame) {
        for idx in (0..self.upvalues.len()).rev() {
            if let Upvalue::Open(stack_index) = self.upvalues[idx] {
                if stack_index >= frame.stack_start {
                    let entry = self.stack.get(stack_index);
                    if let Entry::Pointer(ptr) = entry {
                        self.upvalues[idx] = Upvalue::Closed(ptr);
                    } else {
                        let ptr = self.heap.insert(entry.into());
                        self.upvalues[idx] = Upvalue::Closed(Pointer::Heap(ptr));
                    }
                } else {
                    // If we reach open upvalues poiting to stack entries below our stack_start,
                    // we can bail out early
                    break;
                }
            }
        }
    }

    pub fn push_callframe(&mut self, entry: Entry) {
        let f = match entry {
            Entry::Function(f) => f,
            Entry::Pointer(Pointer::Heap(ptr)) => {
                let value = self.heap.get(ptr);
                let tmp = value.borrow();
                if let Value::StackFunction {
                    addr,
                    arity,
                    upvalues_refs_idx,
                } = *tmp
                {
                    StackFunction {
                        addr,
                        arity,
                        upvalues_refs_idx,
                    }
                } else {
                    unreachable!()
                }
            }
            Entry::Pointer(Pointer::Vtable(p)) => self.functions[p],
            x => {
                dbg!(&self.stack);
                dbg!(&self.entry_to_value(x));
                unreachable!()
            }
        };

        let frame = CallFrame {
            ip: f.addr,
            stack_start: self.stack.len() - f.arity,
            upvalues_refs_idx: f.upvalues_refs_idx,
        };

        self.frames.push(frame);
    }

    pub fn run(&mut self, bytecode: Vec<u8>) -> Result<Value, Value> {
        // Set up some profiling data
        #[cfg(feature = "profile")]
        let mut opcode_stats = HashMap::new();

        if bytecode.is_empty() {
            return Ok(Value::Void);
        }

        self.bytecode = bytecode;

        let frame = CallFrame {
            ip: 0,
            stack_start: 0,
            upvalues_refs_idx: 0,
        };
        self.frames.push(frame);

        #[cfg(feature = "profile")]
        let mut total_time_in_opcodes = 0;
        #[cfg(feature = "profile")]
        println!(
            "{0: <20} | {1: <10} | {2: <10} | {3: <10}",
            "opcode", "avg (ns)", "total (ms)", "execs"
        );
        #[cfg(feature = "profile")]
        println!("{}", "-".repeat(59));
        #[cfg(feature = "profile")]
        for (k, (time, execs)) in opcode_stats.into_iter() {
            total_time_in_opcodes += time;
            println!(
                "{0: <20} | {1: <10} | {2: <10} | {3: <10}",
                format!("{:?}", k),
                time / execs,
                std::time::Duration::from_nanos(time as u64).as_millis(),
                execs
            );
        }

        #[cfg(feature = "profile")]
        println!("{}", "-".repeat(59));
        #[cfg(feature = "profile")]
        println!(
            "{0: <20} | {1: <10}",
            "Total (ms):",
            std::time::Duration::from_nanos(total_time_in_opcodes as u64).as_millis(),
        );

        self.run_frame(0)
    }

    /// Executes a particular call frame and any subsequent frames
    pub fn run_frame(&mut self, bottom_frame: usize) -> Result<Value, Value> {
        while !self.frames.is_empty() && self.frames.len() > bottom_frame {
            // If we advance the instruction pointer to outside of our bytecode,
            // we implicitly return from the current call frame by popping self.frames.
            //
            // If there are no more frames left, we break the loop and move to the return down below.
            if self.frame().ip > self.bytecode.len() - 1 {
                if self.frames.pop().is_none()
                    || (self.frames.len() > 1 && self.frames.len() - 1 < bottom_frame)
                {
                    break;
                } else {
                    continue;
                }
            }

            #[cfg(feature = "profile")]
            let opcode_timer_start = std::time::Instant::now();

            let op = Op::from(self.current_byte());
            let mut offset = 0;
            let forward = true;

            #[cfg(feature = "profile")]
            let opcode_timer_start = std::time::Instant::now();

            // An offset to the instruction pointer, for when ops consume more bytes than 1
            match op {
                Op::Debug => {
                    dbg!(&self.stack.len());
                }

                Op::SetupModule => {
                    // TODO
                    // Get the module stack length and take it off the top of the main stack
                    // Put it into self.modules
                    let entries = self.next_byte();
                    offset = 1;

                    let mut stack = Stack::new();
                    for e in self.stack.take(entries as usize) {
                        stack.push(e);
                    }

                    self.modules.push(stack);
                }

                Op::SetupFunctionCache => {
                    let num_items_bytes = self.next_eight_bytes();
                    let num_items = usize::from_ne_bytes(num_items_bytes);
                    let mut items = vec![];
                    for _ in 0..num_items {
                        items.push(self.stack.pop().unwrap());
                    }
                    for e in items.iter().rev() {
                        if let Entry::Function(f) = e {
                            self.functions.push(*f);
                        }
                    }
                    offset = 8;
                }

                Op::GetModuleSymbol => {
                    let [module_idx, local_idx] = self.next_two_bytes();
                    offset = 2;

                    self.stack
                        .push(self.modules[module_idx as usize].get(local_idx as usize));
                }

                Op::GetFunction => {
                    let idx = self.next_byte();
                    self.stack
                        .push(Entry::Pointer(Pointer::Vtable(idx as usize)));
                    offset = 1;
                }

                Op::GetBuiltin => {
                    let idx = self.next_byte();
                    self.stack
                        .push(Entry::Pointer(Pointer::Builtin(idx as usize)));
                    offset = 1;
                }

                Op::Push => {
                    let value_length_bytes: [u8; 8] = self.next_eight_bytes();
                    let value_length = usize::from_ne_bytes(value_length_bytes);
                    let ip = self.frame().ip;
                    let value_bytes = &self.bytecode[(ip + 9)..(ip + 9 + value_length)];
                    let mut additional_offset = 0;

                    let (value, _): (Value, usize) =
                        bincode::serde::decode_from_slice(value_bytes, bincode::config::legacy())
                            .unwrap();

                    let stackentry = match value {
                        Value::Usize(i) => Entry::Usize(i),
                        Value::Bool(b) => Entry::Bool(b),
                        // Todo all primitive types that get to be stack entries
                        _ => {
                            // For functions, we need to resolve upvalues before putting it on the heap
                            if let Value::Function(f) = value {
                                let mut upvalue_refs = vec![];

                                for (i, x) in f
                                    .upvalues_bytecode
                                    .as_slice()
                                    .to_owned()
                                    .chunks(2)
                                    .enumerate()
                                {
                                    let is_local = x[0];
                                    let idx = x[1];

                                    // If `is_local` is 1, the upvalue refers to an entry
                                    // in our local callframe's stack. We need to capture it to make sure
                                    // it keeps on living after we pop this frame.
                                    if is_local == 1 {
                                        let upv = self.capture_upvalue(
                                            self.frame().stack_start + idx as usize,
                                        );
                                        upvalue_refs.insert(i, upv);

                                    // If it's not local, that means it refers to an upvalue among
                                    // this callframe's upvalues, which in turn refers to something else.
                                    } else {
                                        let upv = self.upvalue_refs[self.frame().upvalues_refs_idx]
                                            [idx as usize];
                                        upvalue_refs.insert(i, upv);
                                    }
                                }

                                self.upvalue_refs.push(upvalue_refs);

                                let func_len_bytes = (&self.bytecode
                                    [(ip + 9 + value_length)..(ip + 9 + value_length + 8)])
                                    .try_into()
                                    .unwrap();
                                additional_offset = usize::from_ne_bytes(func_len_bytes) + 8;

                                Entry::Function(StackFunction {
                                    addr: ip + 9 + value_length + 8,
                                    arity: f.arity,
                                    upvalues_refs_idx: self.upvalue_refs.len() - 1,
                                })
                            } else {
                                Entry::Pointer(Pointer::Heap(self.heap.insert(value)))
                            }
                        }
                    };

                    self.stack.push(stackentry);

                    offset = 8 + value_length + additional_offset;
                }

                Op::Pop => {
                    self.stack.pop();
                }

                Op::Get => {
                    let b = self.next_byte();
                    let entry = self.stack.get(self.frame().stack_start + b as usize);
                    self.stack.push(entry);

                    offset = 1;
                }

                Op::GetUpvalue => {
                    let slot = self.next_byte();
                    let idx = self.upvalue_refs[self.frame().upvalues_refs_idx][slot as usize];

                    let upv = &self.upvalues[idx];

                    let entry = match upv {
                        Upvalue::Closed(ptr) => Entry::Pointer(*ptr),
                        Upvalue::Open(idx) => self.stack.get(*idx),
                    };
                    self.stack.push(entry);

                    offset = 1;
                }

                // Gets a list item by index
                Op::GetMember => {
                    let [first, second] = self.next_two_bytes();
                    let idx_is_next_byte = first == 1;

                    let idx = if idx_is_next_byte {
                        offset = 2;
                        second as usize
                    } else {
                        offset = 1;

                        match self.stack.pop() {
                            Some(Entry::Usize(idx)) => idx,
                            Some(e @ Entry::Pointer(Pointer::Heap(_))) => {
                                match self.entry_to_value(e) {
                                    Value::Usize(idx) => idx,
                                    x => {
                                        dbg!(&x);
                                        unreachable!()
                                    }
                                }
                            }
                            x => {
                                dbg!(&x);
                                unreachable!();
                            }
                        }
                    };
                    if let Some(entry) = self.stack.last_mut() {
                        *entry = match entry {
                            Entry::Pointer(Pointer::Heap(ptr)) => Entry::Pointer(Pointer::Heap(
                                self.heap.get_list_item_ptr(*ptr, idx),
                            )),
                            x => {
                                dbg!(&x);
                                unreachable!()
                            }
                        };
                    }
                }

                Op::Set => {
                    let idx = self.next_byte();
                    let stackentry = self.stack.pop().unwrap();

                    let stack_idx = self.frame().stack_start + idx as usize;
                    if stack_idx == self.stack.len() {
                        self.stack.push(stackentry);
                    } else {
                        // // Edge case: Dylibs need runtime type tracking, and will assume the type
                        // // which is declared in the uninitialized value already on the stack.
                        // if let (Value::Uninitialized(ty), Value::Dylib(dy)) =
                        //     (self.stack[stack_idx].into(), stackentry.into())
                        // {
                        //     dbg!(&ty);
                        //     let mut new_dy = dy.clone();
                        //     new_dy.r#type = ty.clone();
                        //     stackentry =
                        //         StackEntry::Pointer(self.heap.insert(Value::Dylib(new_dy)));
                        // }
                        self.stack.set(stack_idx, stackentry);
                    }
                    offset = 1;
                }

                Op::SetProperty => {
                    let idx = self.next_byte();
                    let property_idx = self.next_byte();
                    let rhs = self.stack.pop().unwrap();

                    let entry = self.stack.get(self.frame().stack_start + idx as usize);
                    match entry {
                        Entry::Pointer(Pointer::Heap(ptr)) => {
                            let item_ptr = self.heap.get_list_item_ptr(ptr, property_idx as usize);
                            let item = self.heap.get(item_ptr);
                            let mut item = item.borrow_mut();
                            *item = self.entry_to_value_ref(rhs).borrow().to_owned();
                        }
                        _ => unreachable!(),
                    }

                    offset = 2;
                }

                // Sets the next byte as the return address on the stack.
                // This gets placed before the arguments for an upcoming Call instruction.
                Op::SetReturn => {
                    let jmp_offset = usize::from_ne_bytes(self.next_eight_bytes());
                    self.stack.push(Entry::Usize(self.frame().ip + jmp_offset));
                    offset = 8;
                }

                Op::Jump => {}

                Op::JumpIfFalse => {
                    let mut jmp_offset = 0;
                    let cond = self.stack.pop().unwrap();
                    if let Entry::Bool(false) = cond {
                        jmp_offset = u64::from_ne_bytes(self.next_eight_bytes()) as usize;
                    }
                    offset = 8 + jmp_offset;
                }

                // Conducts a binary operation between the two top entries on the stack.
                Op::Binary => {
                    let bin_op = InfixOp::from(self.next_byte());
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    let r = match (a, bin_op, b) {
                        (Entry::Usize(a), InfixOp::Add, Entry::Usize(b)) => Entry::Usize(a + b),
                        (Entry::Usize(a), InfixOp::Sub, Entry::Usize(b)) => Entry::Usize(a - b),
                        (Entry::Usize(a), InfixOp::Mul, Entry::Usize(b)) => Entry::Usize(a * b),
                        (Entry::Usize(a), InfixOp::Div, Entry::Usize(b)) => Entry::Usize(a / b),
                        (Entry::Usize(a), InfixOp::Lt, Entry::Usize(b)) => Entry::Bool(a < b),

                        (e1 @ Entry::Pointer(_), op, e2) | (e1, op, e2 @ Entry::Pointer(_)) => {
                            match (
                                &*self.entry_to_value_ref(e1).borrow(),
                                op,
                                &*self.entry_to_value_ref(e2).borrow(),
                            ) {
                                (Value::Usize(a), InfixOp::Add, Value::Usize(b)) => {
                                    Entry::Usize(a + b)
                                }

                                (Value::String(a), InfixOp::Mul, Value::Usize(b)) => {
                                    Entry::Pointer(Pointer::Heap(
                                        self.heap.insert(Value::String(a.repeat(*b))),
                                    ))
                                }

                                x => {
                                    dbg!(&x);
                                    dbg!(&self.stack);
                                    todo!()
                                }
                            }
                        }

                        (x, op, y) => {
                            dbg!(&x, &op, &y);
                            unreachable!()
                        }
                    };
                    self.stack.push(r);
                    offset = 1;
                }

                // Pops the current callframe, truncates the stack to its original size
                // and puts the return value on top of it.
                Op::Return => {
                    let frame = self.frames.pop().unwrap();

                    self.close_upvalues(frame);

                    let result = self.stack.pop().unwrap();

                    self.stack.truncate(frame.stack_start);

                    let ret = &self.stack.pop().unwrap();
                    match ret {
                        Entry::Usize(addr) => {
                            self.frame_mut().ip = *addr;
                        }
                        x => {
                            dbg!(&self.entry_to_value(*x));
                            unreachable!()
                        }
                    }

                    self.stack.push(result);

                    continue;
                }

                Op::Call => {
                    let entry = self.stack.pop().unwrap();
                    match entry {
                        Entry::Pointer(Pointer::Builtin(p)) => {
                            self.builtins[p].0.clone()(self); // TODO get this non-cloneable
                        }
                        entry => {
                            self.push_callframe(entry);
                            continue;
                        }
                    }
                }

                Op::Collect => {
                    let vec_len = usize::from_ne_bytes(self.next_eight_bytes());
                    let mut vec = vec![];
                    for _ in 0..vec_len {
                        let entry = self.stack.pop().unwrap();
                        match entry {
                            Entry::Pointer(Pointer::Heap(ptr)) => {
                                vec.push(ptr);
                            }
                            Entry::Usize(n) => {
                                let ptr = self.heap.insert(Value::Usize(n));
                                vec.push(ptr);
                            }
                            Entry::Function(crate::stack::Function {
                                addr,
                                arity,
                                upvalues_refs_idx,
                            }) => {
                                let ptr = self.heap.insert(Value::StackFunction {
                                    addr,
                                    arity,
                                    upvalues_refs_idx,
                                });
                                vec.push(ptr);
                            }
                            x => {
                                dbg!(&x);
                                unreachable!()
                            }
                        }
                    }
                    vec.reverse();
                    self.stack
                        .push(Entry::Pointer(Pointer::Heap(self.heap.create_list(vec))));
                    offset = 8;
                }

                x => {
                    return Err(Value::Error(crate::value::Error::InvalidOp(x as u8)));
                }
            };

            if forward {
                self.frame_mut().ip = self.frame().ip + 1 + offset;
            } else {
                self.frame_mut().ip = self.frame().ip - offset;
            }

            #[cfg(feature = "profile")]
            opcode_stats
                .entry(op.clone())
                .and_modify(|(time, execs)| {
                    *time += opcode_timer_start.elapsed().as_nanos();
                    *execs += 1;
                })
                .or_insert((opcode_timer_start.elapsed().as_nanos(), 1));
        }

        #[cfg(feature = "debug")]
        println!("program exit with stack len {}", self.stack.len());

        // When the script exits, return whatever is on the top of the stack
        if let Some(entry) = self.stack.pop() {
            let value = self.entry_to_value_ref(entry);
            Ok((*value).clone().borrow().to_owned())
        } else {
            Ok(Value::Void)
        }
    }
}
