//! The pattern matching module generates a decision tree, which can be passed onto the runtime
use crate::compiler::Context;
use witch_parser::{
    pattern::{Case, Pattern},
    types::Type,
    Ast,
};

/// An AST-based decision tree, which will turn into a runtime-compatible tree
#[derive(Debug)]
pub enum Decision {
    Success(Ast),
    Failure,
    Guard {
        clause: Ast,
        body: Ast,
        subtree: Box<Self>,
    },
    Switch(Vec<SwitchCase>)
}

#[derive(Debug)]
pub struct SwitchCase {
    pattern: Pattern,

    arguments: Vec<(String, usize)>, // binding name, field index

    body: Decision,    
}

/// The result of compiling a pattern match expression.
#[derive(Debug)]
pub struct Match {
    // pub types: Vec<Type>,
    pub tree: Decision,
    pub diagnostics: Diagnostics,
}

/// A type for storing diagnostics produced by the decision tree compiler.
#[derive(PartialEq, Debug)]
pub struct Diagnostics {
    /// A flag indicating the match is missing one or more pattern.
    missing: bool,

    /// The right-hand sides that are reachable.
    ///
    /// If a right-hand side isn't in this list it means its pattern is
    /// redundant.
    reachable: Vec<Ast>,
}

/// The match compiler
#[derive(PartialEq, Debug)]
pub struct Compiler<'ctx, 'program>
where
    'program: 'ctx,
{
    ctx: &'ctx mut Context<'program>,
    tested_type: Type,
    variables: Vec<(String, Type)>, 
    diagnostics: Diagnostics,
}

impl<'ctx, 'program> Compiler<'ctx, 'program> {
    pub fn new(ctx: &'ctx mut Context<'program>, tested_type: Type) -> Self {
        Self {
            ctx,
            tested_type,
            variables: vec![],
            diagnostics: Diagnostics {
                missing: false,
                reachable: Vec::new(),
            },
        }
    }

    pub fn compile(mut self, cases: &Vec<Case>) -> Match {
        Match {
            tree: self.compile_cases(cases),
            diagnostics: self.diagnostics,
            // types: self.types,
        }
    }

    fn compile_cases(&mut self, cases: &Vec<Case>) -> Decision {
        if cases.is_empty() {
            self.diagnostics.missing = true;

            return Decision::Failure;
        }

        // If the case pattern is only a variable, mark it as such and set the pattern to Empty
        let mut cases: Vec<Case> = cases.into_iter().map(move_variable_patterns).collect();

        // If the first case is Pattern::Empty, we can just return a success and discard everything else
        if cases
            .first()
            .map_or(false, |c| matches!(c.pattern, Pattern::Empty))
        {
            let case = cases.remove(0);
            self.diagnostics.reachable.push(case.body.clone());
            return if let Some(guard) = case.guard {
                Decision::Guard {
                    clause: guard,
                    body: case.body,
                    subtree: Box::new(self.compile_cases(&cases)),
                }
            } else {
                Decision::Success(case.body)
            };
        }

        
        match &self.tested_type {
            Type::Enum { variants, generics } => {
                
                // Verify that cases' patterns are all variants of this enum
                let switch_cases = cases.iter().map(|case: &Case| {
                    dbg!(&case.pattern);
                    let args = vec![];
                    let switch_cases = match &case.pattern {
                        Pattern::Variant(name, fields) => {
                            // find variant if exists, or throw
                            // iterate all fields of the pattern:
                                // type check against variant types
                                self.compile_switch_cases()
                        }
                        Pattern::Variable(name) => {
                            self.compile_switch_cases()
                        }
                        Pattern::Empty => {
                            self.compile_switch_cases()
                        }
                        _ => {
                            panic!("invalid pattern in case");
                        }
                    };
                    SwitchCase {
                        pattern: case.pattern.clone(),
                        arguments: args,
                        body: Decision::Switch(switch_cases)
                    }  
                }).collect();

                Decision::Switch(switch_cases)
            }
            _ => unimplemented!()
        }
    }

    fn compile_switch_cases(&mut self) -> Vec<SwitchCase> {
        vec![]
    }


        // let branch_var = self.branch_variable(&rows);

        // match self.variable_type(branch_var).clone() {
        //     Type::Int => {
        //         let (cases, fallback) =
        //             self.compile_int_cases(rows, branch_var);

        //         Decision::Switch(branch_var, cases, Some(fallback))
        //     }
        //     Type::Boolean => {
        //         let cases = vec![
        //             (Constructor::False, Vec::new(), Vec::new()),
        //             (Constructor::True, Vec::new(), Vec::new()),
        //         ];

        //         Decision::Switch(
        //             branch_var,
        //             self.compile_constructor_cases(rows, branch_var, cases),
        //             None,
        //         )
        //     }
        //     Type::Pair(typ1, typ2) => {
        //         let cases = vec![(
        //             Constructor::Pair(typ1, typ2),
        //             self.new_variables(&[typ1, typ2]),
        //             Vec::new(),
        //         )];

        //         Decision::Switch(
        //             branch_var,
        //             self.compile_constructor_cases(rows, branch_var, cases),
        //             None,
        //         )
        //     }
        //     Type::Enum(variants) => {
        //         let cases = variants
        //             .iter()
        //             .enumerate()
        //             .map(|(idx, (_, args))| {
        //                 (
        //                     Constructor::Variant(branch_var.type_id, idx),
        //                     self.new_variables(args),
        //                     Vec::new(),
        //                 )
        //             })
        //             .collect();

        //         Decision::Switch(
        //             branch_var,
        //             self.compile_constructor_cases(rows, branch_var, cases),
        //             None,
        //         )
        //     }
        // }
}

fn move_variable_patterns(case: &Case) -> Case {
    let mut case = case.clone();

    if let Pattern::Variable(name) = case.pattern {
        case.bindings.push(name);
        case.pattern = Pattern::Empty;
    }

    case
}
