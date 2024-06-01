//! Handles type resolution and variable binding.
//!
//! Generics are treated as a stack of maps.
//! { T: Foo, I: Baz
//!     { T: Bar
//!         T = Bar, I = Baz
//!     }
//! }
//!
//!
use crate::error::{Error, Result};
use anyhow::anyhow;
use std::collections::HashMap;
use witch_parser::types::{EnumVariant, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeSystem {
    /// Types are global for a module context
    pub types: HashMap<String, Type>,

    /// Variants are global for a module context. Tracked by variant name, and (parent_type, variant data)
    pub variants: HashMap<String, (String, EnumVariant)>,

    /// Variable mappings. These are used to enhance types from abstract
    /// into more concrete.
    pub substitutions: Vec<HashMap<String, Type>>,
}

impl Default for TypeSystem {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeSystem {
    pub fn new() -> Self {
        Self {
            types: vec![(
                "List".to_string(),
                Type::List(Box::new(Type::TypeVar("T".to_string()))),
            )]
            .into_iter()
            .collect(),
            variants: HashMap::default(),
            substitutions: vec![HashMap::default()],
        }
    }

    /// Adds a type to the type library, unless it already exists
    pub fn add_type(&mut self, name: String, typ: Type) -> Result<()> {
        self.types
            .try_insert(name.clone(), typ.clone())
            .map_err(|_| Error::fatal())?;

        // For Enums, we also reserve their variants to keep track of them separately, and avoid name collisions
        if let Type::Enum { ref variants, .. } = typ {
            for variant in variants.iter() {
                self.variants
                    .try_insert(variant.name.clone(), (name.clone(), variant.clone()))
                    .map_err(|_| Error::fatal())?;
            }
        }

        Ok(())
    }

    /// Merges the current substitution table with the provided one as a new "scope".
    pub fn push_scope(&mut self, subs: HashMap<String, Type>) {
        let mut scope = self.substitutions.last().unwrap().clone();
        scope.extend(subs);
        self.substitutions.push(scope);
    }

    /// Pops the topmost scope
    pub fn pop_scope(&mut self) {
        self.substitutions.pop();
    }

    /// A type is considered abstract if it is an interface, an intersection, or if it contains any such thingss
    pub fn is_abstract(&self, typ: &Type) -> bool {
        if matches!(typ, Type::Interface { .. } | Type::Intersection(_)) {
            return true;
        }

        match typ {
            Type::Struct { fields, .. } => {
                for f in fields.iter() {
                    if self.is_abstract(&f.1) {
                        return true;
                    }
                }
                false
            }
            Type::Function { args, returns, .. } => {
                for a in args.iter() {
                    if self.is_abstract(a) {
                        return true;
                    }
                }
                self.is_abstract(returns)
            }
            Type::TypeVar(name) => self.is_abstract(self.types.get(name).unwrap_or_else(|| {
                dbg!(name);
                panic!();
            })),
            _ => false,
        }
    }

    /// Takes any type and returns its most concrete form given our current knowledge
    pub fn resolve(&mut self, typ: Type) -> Result<Type> {
        match typ {
            Type::Function {
                args,
                returns,
                is_variadic,
                generics,
                is_method,
            } => {
                self.push_scope(generics.clone().into_iter().collect());
                let args: Vec<Type> = args
                    .into_iter()
                    .map(|t| self.resolve(t))
                    .collect::<Result<Vec<Type>>>()?;
                let returns = Box::new(self.resolve(*returns)?);

                self.pop_scope();

                Ok(Type::Function {
                    args,
                    returns,
                    is_variadic,
                    generics,
                    is_method,
                })
            }

            Type::Struct {
                name,
                fields,
                methods,
                generics,
            } => {
                self.push_scope(generics.clone().into_iter().collect());
                let fields = fields
                    .into_iter()
                    .map(|(n, t)| self.resolve(t).map(|t| (n, t)))
                    .collect::<Result<Vec<(String, Type)>>>()?;
                let methods = methods
                    .into_iter()
                    .map(|(n, (t, i))| self.resolve(t).map(|t| (n, (t, i))))
                    .collect::<Result<HashMap<String, (Type, usize)>>>()?;
                self.pop_scope();
                Ok(Type::Struct {
                    name,
                    fields,
                    methods,
                    generics,
                })
            }

            Type::Enum { variants, generics } => {
                self.push_scope(generics.clone().into_iter().collect());
                let variants = variants
                    .into_iter()
                    .map(|mut v| {
                        for (i, t) in v.types.iter_mut().enumerate() {
                            *t = self.resolve(t.clone())?;
                        }
                        Ok(v)
                    })
                    .collect::<Result<Vec<EnumVariant>>>()?;
                self.pop_scope();

                Ok(Type::Enum { variants, generics })
            }

            Type::TypeVar(name) => {
                // Look through substitution table first, then check our types library
                if let Some(typ) = self.substitutions.last().unwrap().get(&name) {
                    return Ok(typ.clone());
                }

                return self.types.get(&name).cloned().ok_or_else(|| {
                    dbg!(name);
                    anyhow!(Error::fatal())
                });
            }

            Type::WithSubstitutions(typ, subs) => {
                match self.resolve(*typ.clone())? {
                    Type::Interface {
                        name,
                        properties,
                        generics,
                    } => {
                        if subs.len() != generics.len() {
                            panic!("generic params must be of same length as available generics");
                        }

                        let generics = generics
                            .into_iter()
                            .enumerate()
                            .map(|(idx, (n, _))| {
                                let t = self.resolve(subs[idx].clone())?;
                                Ok((n.clone(), t))
                            })
                            .collect::<Result<Vec<(String, Type)>>>()?;

                        self.resolve(Type::Interface {
                            name,
                            properties,
                            generics,
                        })
                    }

                    Type::Struct {
                        name,
                        fields,
                        methods,
                        generics,
                    } => {
                        // Todo how to handle nested generics???? Foo[A, B, C, D] where A: B[C, D] { thing: A }
                        if subs.len() != generics.len() {
                            dbg!(&subs, &generics);
                            panic!("generic params must be of same length as available generics");
                        }

                        let generics = generics
                            .into_iter()
                            .enumerate()
                            .map(|(idx, (n, _))| (n.clone(), subs[idx].clone()))
                            .collect();

                        self.resolve(Type::Struct {
                            name,
                            fields,
                            methods,
                            generics,
                        })
                    }

                    Type::Enum { variants, generics } => {
                        if subs.len() != generics.len() {
                            panic!("generic params must be of same length as available generics");
                        }

                        let generics = generics
                            .into_iter()
                            .enumerate()
                            .map(|(idx, (n, _))| {
                                let t = self.resolve(subs[idx].clone())?;
                                Ok((n.clone(), t))
                            })
                            .collect::<Result<Vec<(String, Type)>>>()?;

                        self.resolve(Type::Enum { variants, generics })
                    }
                    Type::List(_) => {
                        if subs.len() != 1 {
                            panic!("list type only allows one single generic param");
                        }
                        Ok(Type::List(Box::new(self.resolve(subs[0].clone())?)))
                    }
                    x => panic!("this type dont allow generics: {:?}", x),
                }
            }

            x => Ok(x),
        }
    }

    /// Gets a defined type if it exists
    pub fn get_type(&self, name: &str) -> Option<Type> {
        self.types.get(name).cloned().or_else(|| {
            self.variants
                .get(name)
                .map(|(_, v)| Type::EnumVariant(v.clone()))
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn it_resolves_generics() {
        let mut ts = TypeSystem::new();
        ts.add_type("MyType".to_string(), Type::String).unwrap();
    }
}
