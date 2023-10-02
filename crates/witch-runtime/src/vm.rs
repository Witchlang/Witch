use alloc::vec;
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};

use crate::stack::{Entry, Stack};
use crate::value::{Function, Value};

#[repr(u8)]
#[derive(Eq, PartialEq, Clone)]
pub enum BinaryOp {
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
}

impl core::convert::From<u8> for BinaryOp {
    fn from(byte: u8) -> Self {
        match byte {
            0 => BinaryOp::Add,
            _ => todo!(),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Clone)]
#[repr(u8)]
pub enum Op {
    Binary,
    Push,
    GetValue,

    Crash,
}

impl core::convert::From<u8> for Op {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Op::Binary,
            1 => Op::Push,
            2 => Op::GetValue,
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
    frames: Vec<CallFrame>,
    functions: Vec<Function>,
    cache: Stack,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            frames: vec![],
            functions: vec![],
            cache: Stack::new(),
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
            let mut offset = 0;
            let mut forward = true;

            #[cfg(feature = "profile")]
            let opcode_timer_start = std::time::Instant::now();

            // An offset to the instruction pointer, for when ops consume more bytes than 1
            match op {
                Op::Binary => {
                    let bin_op = BinaryOp::from(self.next_byte());
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let res = match (a, bin_op, b) {
                        (Entry::Usize(a), BinaryOp::Add, Entry::Usize(b)) => Entry::Usize(a + b),

                        (_x, _op, _y) => {
                            todo!()
                        }
                    };
                    self.stack.push(res);
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

        // When the script exits, return whatever is on the top of the stack
        if let Some(entry) = self.stack.pop() {
            Ok(entry.into())
        } else {
            Ok(Value::Void)
        }
    }
}
