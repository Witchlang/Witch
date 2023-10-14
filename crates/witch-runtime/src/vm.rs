use alloc::vec;
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};

use crate::heap::Heap;
use crate::stack::{Entry, Pointer, Stack};
use crate::value::{Function, Value};

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
            _ => todo!(),
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Serialize, Deserialize, PartialEq, Clone)]
#[repr(u8)]
pub enum Op {
    SetupValueCache,
    SetupFunctionCache,
    GetValue,
    GetFunction,

    Push,
    Get,
    Set,

    Binary,

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
            6 => Op::Set,

            7 => Op::Binary,
            _ => Op::Crash,
        }
    }
}

#[derive(Clone, Copy)]
pub struct CallFrame {
    pub function_ptr: usize,
    pub ip: usize,
    stack_start: usize,
}

pub struct Vm {
    stack: Stack,
    heap: Heap,
    frames: Vec<CallFrame>,
    functions: Vec<Function>,
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
    fn current_byte(&self) -> u8 {
        self.functions[self.frame().function_ptr].bytecode[self.frame().ip]
    }

    /// Retrieves the byte which is after the byte that the instruction pointer is currently pointing at.
    fn next_byte(&self) -> u8 {
        self.functions[self.frame().function_ptr].bytecode[self.frame().ip + 1]
    }

    /// Retrieves the next 8 bytes from the current instruction pointer.
    fn next_eight_bytes(&self) -> [u8; 8] {
        self.functions[self.frame().function_ptr].bytecode
            [self.frame().ip + 1..self.frame().ip + 1 + 8]
            .try_into()
            .unwrap()
    }

    pub fn run(&mut self, bytecode: Vec<u8>) -> Result<Value, Value> {
        // Set up some profiling data
        #[cfg(feature = "profile")]
        let mut opcode_stats = HashMap::new();

        if bytecode.is_empty() {
            return Ok(Value::Void);
        }

        self.functions.push(crate::value::Function {
            is_variadic: false,
            is_method: false,
            arity: 0,
            bytecode,
            upvalue_count: 0,
            upvalues: vec![],
            upvalues_bytecode: vec![],
        });
        let frame = CallFrame {
            ip: 0,
            stack_start: 0,
            function_ptr: 0,
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
            if self.frame().ip > self.functions[self.frame().function_ptr].bytecode.len() - 1 {
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
            let offset;
            let forward = true;

            dbg!(&op);

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
                    // for _ in 0..num_items {
                    //     let v = self.stack.pop().unwrap();
                    //     self.functions.push(v);
                    // }
                    offset = 8;
                }

                Op::GetValue => {
                    let idx = self.next_byte();
                    dbg!(&idx, &self.cache);
                    let val = self.cache[idx as usize];
                    dbg!(val);
                    self.stack.push(val);
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

                Op::Binary => {
                    let bin_op = InfixOp::from(self.next_byte());
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    dbg!(a, b);
                    let res = match (a, bin_op, b) {
                        (Entry::Usize(a), InfixOp::Add, Entry::Usize(b)) => Entry::Usize(a + b),
                        (Entry::Usize(a), InfixOp::Sub, Entry::Usize(b)) => Entry::Usize(a - b),
                        (Entry::Usize(a), InfixOp::Mul, Entry::Usize(b)) => Entry::Usize(a * b),

                        (_x, _op, _y) => {
                            todo!()
                        }
                    };
                    self.stack.push(res);
                    offset = 1;
                }
                Op::Push => {
                    let value_length_bytes: [u8; 8] = self.next_eight_bytes();
                    let value_length = usize::from_ne_bytes(value_length_bytes);
                    let value_bytes = &self.functions[self.frame().function_ptr].bytecode
                        [(&self.frame().ip + 9)..(&self.frame().ip + 9 + value_length)];

                    let (mut value, _): (Value, usize) =
                        bincode::serde::decode_from_slice(value_bytes, bincode::config::legacy())
                            .unwrap();

                    let stackentry = match value {
                        Value::Usize(i) => Entry::Usize(i),
                        Value::Bool(b) => Entry::Bool(b),
                        // Todo all primitive types that get to be stack entries
                        _ => {
                            // if let Value::Function(mut f) = value {
                            //     for (i, x) in f
                            //         .upvalues_bytecode
                            //         .as_slice()
                            //         .to_owned()
                            //         .chunks(2)
                            //         .enumerate()
                            //     {
                            //         let is_local = x[0];
                            //         let stack_idx = x[1];
                            //         if is_local == 1 {
                            //             f.upvalues.insert(
                            //                 i,
                            //                 self.capture_upvalue(
                            //                     self.frame().stack_start + stack_idx as usize,
                            //                 ),
                            //             )
                            //         } else {
                            //             f.upvalues.insert(
                            //                 i,
                            //                 self.functions[self.frame().function].upvalues
                            //                     [stack_idx as usize],
                            //             )
                            //         }
                            //     }
                            //     value = Value::Function(f);
                            // }
                            Entry::Pointer(Pointer::Heap(self.heap.insert(value)))
                        }
                    };

                    self.stack.push(stackentry);

                    offset = 8 + value_length;
                }
                Op::Get => {
                    let b = self.next_byte();
                    let entry = self.stack.get(self.frame().stack_start + b as usize);
                    dbg!(entry);
                    self.stack
                        .push(entry);

                    offset = 1;
                }
                x => {
                    return Err(Value::Error(crate::value::Error::InvalidOp(x)));
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

            dbg!(&self.stack);

        // When the script exits, return whatever is on the top of the stack
        if let Some(entry) = self.stack.pop() {
            Ok(entry.into())
        } else {
            Ok(Value::Void)
        }
    }
}
