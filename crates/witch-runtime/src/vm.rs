use core::cell::{RefCell};

use alloc::rc::Rc;
use alloc::vec;
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};

use crate::heap::Heap;
use crate::stack::{Entry, Pointer, Stack};
use crate::value::{Function, Value};

#[derive(Debug)]
enum Upvalue {
    /// If its closed over, it contains a pointer to the value on the heap
    Closed(Pointer),

    /// If its not yet closed over, it contains the stack index of where the value resides
    Open(usize),

    /// Points to a different upvalue
    Link(usize),
}

#[repr(u8)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Eq, PartialEq, Clone)]
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
    SetupValueCache,
    SetupFunctionCache,
    GetValue,
    GetFunction,

    Push,
    Get,
    GetUpvalue,
    GetMember,
    Set,
    SetProperty,

    Jump,
    JumpIfFalse,

    Binary,
    Return,
    Call,

    Collect,

    Crash,
}

impl core::convert::From<u8> for Op {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Op::SetupValueCache,
            1 => Op::SetupFunctionCache,
            2 => Op::GetValue,
            3 => Op::GetFunction,

            4 => Op::Push,
            5 => Op::Get,
            6 => Op::GetUpvalue,
            7 => Op::GetMember,
            8 => Op::Set,
            9 => Op::SetProperty,

            10 => Op::Jump,
            11 => Op::JumpIfFalse,

            12 => Op::Binary,
            13 => Op::Return,
            14 => Op::Call,

            15 => Op::Collect,

            _ => Op::Crash,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CallFrame {
    pub ptr: Pointer,
    pub ip: usize,
    stack_start: usize,
}

pub struct Vm {
    stack: Stack,
    heap: Heap,
    frames: Vec<CallFrame>,

    /// Our function vtable. Struct methods go here.
    functions: Vec<Function>,

    /// A vec of pointers to the Heap, where we store our upvalues for closures
    upvalues: Vec<Upvalue>,
    cache: Vec<Entry>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            heap: Heap::default(),
            frames: vec![],
            functions: vec![],
            upvalues: vec![],
            cache: vec![],
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
        self.deref_function(self.frame().ptr).bytecode[self.frame().ip]
    }

    /// Retrieves the byte which is after the byte that the instruction pointer is currently pointing at.
    fn next_byte(&mut self) -> u8 {
        self.deref_function(self.frame().ptr).bytecode[self.frame().ip + 1]
    }

    /// Retrieves the next 8 bytes from the current instruction pointer.
    fn next_eight_bytes(&mut self) -> [u8; 8] {
        self.deref_function(self.frame().ptr).bytecode[self.frame().ip + 1..self.frame().ip + 1 + 8]
            .try_into()
            .unwrap()
    }

    fn to_value(&mut self, entry: Entry) -> Rc<RefCell<Value>> {
        match entry {
            Entry::Pointer(Pointer(idx)) => self.heap.get(idx),
            Entry::OffsetPointer(Pointer(ptr), idx) => {
                if let Value::List(ref list) = *self.heap.get(ptr).borrow() {
                    Rc::new(RefCell::new(list[idx].clone()))
                } else {
                    unreachable!()
                }
            }
            Entry::Usize(u) => Rc::new(RefCell::new(Value::Usize(u))),
            x => todo!("{:?}", x),
        }
    }

    // FIXME this shuldnt clone f
    fn deref_function(&mut self, ptr: Pointer) -> Function {
        if let Value::Function(ref f) = *self.to_value(Entry::Pointer(ptr)).clone().borrow() {
            return f.clone();
        }
        unreachable!()
    }

    /// Moves a stack entry to the heap and stashes a copy of the pointer
    /// among our `upvalues` to be referenced by a closure at a later time
    /// TODO Make it less naive so we dont capture upvalues more than once if necessary
    fn capture_upvalue(&mut self, idx: usize) -> usize {
        self.upvalues.push(Upvalue::Open(idx));
        self.upvalues.len() - 1
    }

    fn link_upvalue(&mut self, idx: usize) -> usize {
        self.upvalues.push(Upvalue::Link(idx));
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
                        self.upvalues[idx] = Upvalue::Closed(Pointer(ptr));
                    }
                } else {
                    // If we reach open upvalues poiting to stack entries below our stack_start,
                    // we can bail out early
                    break;
                }
            }
        }
    }

    pub fn push_callframe(&mut self, ptr: Pointer, offset: usize) {
        if let Value::Function(f) = (self.to_value(Entry::Pointer(ptr))).borrow().clone() {
            let frame = CallFrame {
                ip: 0,
                stack_start: self.stack.len() - f.arity,
                ptr,
            };

            self.frame_mut().ip = self.frame().ip + offset; // One to advance the instruction pointer, plus one offset for the arg_len
            self.frames.push(frame);
        }
    }

    pub fn run(&mut self, bytecode: Vec<u8>) -> Result<Value, Value> {
        // Set up some profiling data
        #[cfg(feature = "profile")]
        let mut opcode_stats = HashMap::new();

        if bytecode.is_empty() {
            return Ok(Value::Void);
        }

        let ptr = self.heap.insert(Value::Function(Function {
            is_variadic: false,
            is_method: false,
            arity: 0,
            bytecode,
            upvalue_count: 0,
            upvalues: vec![],
            upvalues_bytecode: vec![],
        }));
        let frame = CallFrame {
            ip: 0,
            stack_start: 0,
            ptr: Pointer(ptr),
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
            if self.frame().ip > self.deref_function(self.frame().ptr).bytecode.len() - 1 {
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
                Op::SetupValueCache => {
                    let num_items_bytes = self.next_eight_bytes();
                    let num_items = usize::from_ne_bytes(num_items_bytes);
                    let mut items = vec![];
                    for _ in 0..num_items {
                        items.push(self.stack.pop().unwrap());
                    }
                    for e in items.iter().rev() {
                        self.cache.push(*e);
                    }

                    offset = 8;
                }
                Op::SetupFunctionCache => {
                    let num_items_bytes = self.next_eight_bytes();
                    let num_items = usize::from_ne_bytes(num_items_bytes);
                    let mut items = vec![];
                    for _ in 0..num_items {
                        items.push(self.stack.pop().unwrap());
                    }
                    for e in items.iter().rev() {
                        if let Value::Function(ref f) = self.to_value(*e).borrow().clone() {
                            self.functions.push(f.clone());
                        }
                    }
                    offset = 8;
                }

                Op::GetValue => {
                    let idx = self.next_byte();
                    let val = self.cache[idx as usize];
                    self.stack.push(val);
                    offset = 1;
                }

                Op::Push => {
                    let value_length_bytes: [u8; 8] = self.next_eight_bytes();
                    let value_length = usize::from_ne_bytes(value_length_bytes);
                    let ip = self.frame().ip;
                    let value_bytes = &self.deref_function(self.frame().ptr).bytecode
                        [(ip + 9)..(ip + 9 + value_length)];

                    let (mut value, _): (Value, usize) =
                        bincode::serde::decode_from_slice(value_bytes, bincode::config::legacy())
                            .unwrap();

                    let stackentry = match value {
                        Value::Usize(i) => Entry::Usize(i),
                        Value::Bool(b) => Entry::Bool(b),
                        // Todo all primitive types that get to be stack entries
                        _ => {
                            // For functions, we need to resolve upvalues before putting it on the heap
                            if let Value::Function(mut f) = value {
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
                                        f.upvalues.insert(
                                            i,
                                            self.capture_upvalue(
                                                self.frame().stack_start + idx as usize,
                                            ),
                                        )
                                    // If it's not local, that means it refers to an upvalue among
                                    // this callframe's upvalues, which in turn refers to something else.
                                    } else {
                                        let func = self.deref_function(self.frame().ptr);
                                        f.upvalues.insert(
                                            i,
                                            self.link_upvalue(func.upvalues[idx as usize]),
                                        )
                                    }
                                }
                                value = Value::Function(f);
                            }
                            Entry::Pointer(Pointer(self.heap.insert(value)))
                        }
                    };

                    self.stack.push(stackentry);

                    offset = 8 + value_length;
                }

                Op::Get => {
                    let b = self.next_byte();
                    let entry = self.stack.get(self.frame().stack_start + b as usize);
                    self.stack.push(entry);

                    offset = 1;
                }

                Op::GetUpvalue => {
                    let slot = self.next_byte();
                    let idx = self.deref_function(self.frame().ptr).upvalues[slot as usize];

                    let mut upv = &self.upvalues[idx];

                    while let Upvalue::Link(idx) = upv {
                        upv = &self.upvalues[*idx];
                    }

                    let entry = match upv {
                        Upvalue::Closed(ptr) => Entry::Pointer(*ptr),
                        Upvalue::Open(idx) => self.stack.get(*idx),
                        _ => unreachable!(),
                    };
                    self.stack.push(entry);

                    offset = 1;
                }

                // Gets a list item by index
                Op::GetMember => {
                    let idx = self.next_byte();
                    let entry = self.stack.last_mut().unwrap();

                    *entry = match entry {
                        Entry::Pointer(ref ptr) => Entry::OffsetPointer(*ptr, idx as usize),
                        _ => unreachable!(),
                    };

                    offset = 1;
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

                    let container_ptr = self.to_value(entry).clone();
                    if let Value::List(ref mut vec) = *(container_ptr.borrow_mut()) {
                        (*vec)[property_idx as usize] = self.to_value(rhs).borrow().to_owned();
                    } else {
                        unreachable!();
                    }

                    offset = 2;
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

                        (e1 @ Entry::Pointer(_), op, e2)
                        | (e1, op, e2 @ Entry::Pointer(_)) => {
                            match (
                                &*self.to_value(e1).borrow(),
                                op,
                                &*self.to_value(e2).borrow(),
                            ) {
                                (Value::Usize(a), InfixOp::Add, Value::Usize(b)) => {
                                    Entry::Usize(a + b)
                                }

                                (Value::String(a), InfixOp::Mul, Value::Usize(b)) => {
                                    Entry::Pointer(Pointer(
                                        self.heap.insert(Value::String(a.repeat(*b))),
                                    ))
                                }

                                _ => todo!(),
                            }
                        }

                        (_x, _op, _y) => {
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
                    self.stack.push(result);
                    continue;
                }

                Op::Call => {
                    let _arg_len = self.next_byte();
                    let entry = self.stack.pop().unwrap();

                    if let Entry::Pointer(ptr) = entry {
                        // todo support variadic funcs using arg_len

                        self.push_callframe(ptr, 2);
                        continue;
                    } else {
                        dbg!(&self.stack);
                        unreachable!()
                    }

                    //todo support native functions

                    offset = 1;
                }

                Op::Collect => {
                    let vec_len = usize::from_ne_bytes(self.next_eight_bytes());
                    let mut vec = vec![];
                    for _ in 0..vec_len {
                        vec.push(self.stack.pop().unwrap().into());
                    }
                    vec.reverse();
                    self.stack
                        .push(Entry::Pointer(Pointer(self.heap.insert(Value::List(vec)))));
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

        // When the script exits, return whatever is on the top of the stack
        if let Some(entry) = self.stack.pop() {
            let value = self.to_value(entry);
            Ok((*value).clone().borrow().to_owned())
        } else {
            Ok(Value::Void)
        }
    }
}
