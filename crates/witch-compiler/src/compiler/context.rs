use super::{type_system::TypeSystem, LocalVariable};
use crate::error::{Error, Result};
use anyhow::anyhow;
use std::{collections::HashMap, path::PathBuf};
use witch_parser::{
    types::{EnumVariant, Type},
    Ast,
};
use witch_runtime::vm::Op;

#[derive(Debug, Clone, PartialEq)]
pub struct Upvalue {
    pub index: usize,

    // Whether the index refers to a local variable, or an upvalue in the parent scope
    pub is_local: bool,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Scope {
    pub locals: Vec<LocalVariable>,
    pub generic_functions: Vec<(String, Ast)>,
    pub upvalues: Vec<Upvalue>,
}

impl Scope {
    /// Adds an upvalue to the current scope. `is_local` refers to whether the index points to
    /// a scope-local variable or the upvalue index of the parent scope.
    pub fn add_upvalue(&mut self, index: usize, is_local: bool) -> Option<usize> {
        // If the upvalue already exists, simply return its index
        for (i, u) in self.upvalues.iter().enumerate() {
            if u.index == index && u.is_local == is_local {
                return Some(i);
            }
        }

        self.upvalues.push(Upvalue { index, is_local });
        Some(self.upvalues.len() - 1)
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Cached {
    pub bytecode: Vec<u8>,
    pub flushed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub stack_offset: usize,
    pub vtable_count: usize,
    pub vtable_offset: usize,
    pub locals: Vec<LocalVariable>,
    pub path: PathBuf,
}

impl Module {
    pub fn name(&self) -> String {
        self.path.file_stem().unwrap().to_string_lossy().to_string()
    }

    pub fn r#type(&self) -> Type {
        Type::Module {
            path: self.path.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context<'a> {
    pub current_module: PathBuf,

    pub modules: &'a Vec<(PathBuf, Module)>,

    /// Holds the type system
    pub ts: TypeSystem,

    /// Scopes map to callframes at runtime
    pub scopes: Vec<Scope>,

    /// Lineage is a straight line of parent relationships from the current
    /// `compile` iteration up the the root node.
    pub lineage: Vec<Ast>,

    /// If we are currently within a function declaration, this is it's type.
    pub current_function_type: Option<Type>,

    /// If we're currently in an assignment, keep track of Ident and the assigned Type (may be Unknown)
    pub assignment_ctx: Option<(String, Type)>,

    /// Values get cached in order keep the subsequent programs smaller
    pub prelude: Option<Vec<u8>>,
    pub functions_cache: Vec<Cached>,
    pub value_cache: Vec<Cached>,
}

impl<'a> Context<'a> {
    pub fn new(current_module: PathBuf, modules: &'a Vec<(PathBuf, Module)>) -> Self {
        Self {
            current_module,
            modules,
            ts: TypeSystem::new(),
            scopes: vec![Scope::default()],
            lineage: Default::default(),
            current_function_type: None,
            assignment_ctx: None,
            prelude: None,
            functions_cache: Default::default(),
            value_cache: Default::default(),
        }
    }

    pub fn stack_offset(&self) -> usize {
        self.modules
            .iter()
            .fold(0, |acc, (_, module)| acc + module.locals.len())
    }

    pub fn vtable_offset(&self) -> usize {
        self.modules
            .iter()
            .fold(0, |acc, (_, module)| acc + module.vtable_count)
    }

    pub fn get_module(&self, path: &PathBuf) -> Option<Module> {
        self.modules
            .iter()
            .find(|(_, m)| &m.path == path)
            .map(|(_, m)| m)
            .cloned()
    }

    pub fn get_local(&mut self, ident: &str) -> Option<(usize, LocalVariable)> {
        let offset = if self.scopes.len() == 1 {
            self.stack_offset()
        } else {
            0
        };
        for (i, local) in self.scope().unwrap().locals.iter().enumerate() {
            if local.name == *ident {
                return Some(((offset + i), local.to_owned()));
            }
        }
        None
    }

    /// Retrieves the builtin index and its type signature, if it exists
    pub fn get_builtin(&self, ident: &str) -> Option<(usize, Type)> {
        witch_runtime::builtins::builtins_info()
            .iter()
            .enumerate()
            .find(|(idx, b)| b.name == ident)
            .map(|(idx, b)| (idx, Type::from(b)))
    }

    pub fn scope(&mut self) -> Result<&mut Scope> {
        self.scopes.last_mut().ok_or(anyhow!(Error::fatal()))
    }

    pub fn scope_by_index(&mut self, scope_idx: usize) -> Result<&mut Scope> {
        self.scopes
            .get_mut(scope_idx)
            .ok_or(anyhow!(Error::fatal()))
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        self.ts.get_type(name)
    }

    /// Adds a type to the type system. May only be done in the root/global scope!
    pub fn add_type(&mut self, name: String, typ: Type) -> Result<()> {
        if self.scopes.len() > 1 {
            dbg!("attempted to add type in non-root scope");
            return Err(anyhow!(Error::fatal()));
        }
        self.ts.add_type(name, typ)
    }

    pub fn push_type_scope(&mut self, subs: &Vec<(String, Type)>) {
        self.ts.push_scope(subs.clone().into_iter().collect())
    }

    pub fn pop_type_scope(&mut self) {
        self.ts.pop_scope()
    }

    pub fn resolve_generic_function(&mut self, ident: &str) -> Option<(usize, usize)> {
        for (scope_idx, scope) in self.scopes.iter().enumerate() {
            for idx in 0..scope.generic_functions.len() {
                if scope.generic_functions[idx].0 == ident {
                    return Some((scope_idx, idx));
                }
            }
        }
        None
    }

    /// For non-local variables, we recursively walk upwards from our current scope until we find it.
    /// When it is found, it is marked as `captured`. We then add the upvalue to each scope on the way back,
    /// giving us a linked list of sorts pointing to the original variable.
    pub fn resolve_upvalue(
        &mut self,
        ident: &str,
        scope_idx: usize,
    ) -> Result<Option<(usize, Type)>> {
        // If we are in the outermost scope, there are no upvalues to resolve
        if scope_idx == 0 {
            return Ok(None);
        }

        // Check the immediate parent scope for the variable
        let mut parent_local = None;
        for (local_index, variable) in self
            .scope_by_index(scope_idx - 1)?
            .locals
            .iter()
            .enumerate()
        {
            if variable.name == *ident {
                parent_local = Some((local_index, variable.clone()));
                break;
            }
        }

        // If its found, we mark it as captured and return its index and type
        if let Some((local_index, variable)) = parent_local {
            self.scope_by_index(scope_idx - 1)?.locals[local_index].is_captured = true;

            let ty = variable.r#type.clone();
            return Ok(self
                .scope_by_index(scope_idx)?
                .add_upvalue(local_index, true)
                .map(|idx| (idx, ty)));
        }

        // If its not found in the parent scope, we recurse up the stack of scopes
        // until it is found (or not).
        if let Some((idx, ty)) = self.resolve_upvalue(ident, scope_idx - 1)? {
            return Ok(self
                .scope_by_index(scope_idx)?
                .add_upvalue(idx, false)
                .map(|idx| (idx, ty)));
        }

        Ok(None)
    }

    /// Flushes the current context.
    /// Unflushed cached values get returned as a `prelude` bytecode and marked as such.
    /// Scopes and AST lineage of the context get reset.
    pub fn flush(&mut self) -> Vec<u8> {
        let mut bc = vec![];

        // Setup imported modules using the following strategy:
        // - Create a DAG of imports in order to deduce a working order
        // - For each module, emit the bytecode and then the SetupModule OP, and then the mode stack size
        // -
        // - The generated stack on top of the normal one will then be moved into the approprate module stack within the vm
        // for mut module in self.imports.clone().into_iter() {
        //     bc.append(&mut module.bytecode);
        //     bc.push(Op::SetupModule as u8);
        //     bc.push(module.context.scope().unwrap().locals.len() as u8);
        // }

        let mut len: usize = 0;
        for cached in self.functions_cache.iter_mut() {
            if !cached.flushed {
                cached.flushed = true;
                bc.append(&mut cached.bytecode.clone());
                len += 1;
            }
        }
        bc.push(Op::SetupFunctionCache as u8);
        let len: [u8; std::mem::size_of::<usize>()] = len.to_ne_bytes();
        bc.append(&mut len.to_vec());
        bc
    }

    /// Adds a fn bytecode to the cache, unless it has already been cached
    /// before. Whether the cached value has been flushed in a previous script
    /// has no bearing on the cached entry index.
    pub fn cache_fn(&mut self, fn_bytecode: Vec<u8>) -> usize {
        if let Some(idx) = self
            .functions_cache
            .iter()
            .position(|cached| &cached.bytecode == &fn_bytecode)
        {
            idx
        } else {
            self.functions_cache.push(Cached {
                bytecode: fn_bytecode,
                flushed: false,
            });
            self.functions_cache.len() - 1
        }
    }

    /// Replaces the bytecode at index `idx` within the functions cache.
    pub fn cache_fn_at(&mut self, idx: usize, fn_bytecode: Vec<u8>) {
        self.functions_cache[idx] = Cached {
            bytecode: fn_bytecode,
            flushed: false,
        };
    }
}
