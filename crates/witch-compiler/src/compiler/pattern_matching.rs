//! The pattern matching module generates a decision tree, which can be passed onto the runtime
use crate::compiler::Context;
use std::collections::HashMap;
use witch_parser::{
    pattern::{Case, Pattern},
    types::Type,
    Ast,
};

/// An AST-based decision tree, which will turn into a runtime-compatible tree
#[derive(Debug)]
pub enum Decision {
    Success(Body),
    Failure,
    Guard {
        clause: Ast,
        body: Body,
        subtree: Box<Self>,
    },
    Switch(Variable, Vec<SwitchCase>),
}

#[derive(Debug)]
pub struct SwitchCase {
    pub constructor: Constructor,

    pub arguments: Vec<Variable>,

    pub body: Decision,
}
impl SwitchCase {
    fn new(constructor: Constructor, arguments: Vec<Variable>, body: Decision) -> Self {
        Self {
            constructor,
            arguments,
            body,
        }
    }
}

/// The result of compiling a pattern match expression.
#[derive(Debug)]
pub struct Match {
    // pub types: Vec<Type>,
    pub tree: Decision,
    pub diagnostics: Diagnostics,
}

/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    True,
    False,
    Int(i64),
    Variant(Type, usize),
}

impl Constructor {
    /// Returns the index of this constructor relative to its type.
    fn index(&self) -> usize {
        match self {
            Constructor::False | Constructor::Int(_) => 0,
            Constructor::True => 1,
            Constructor::Variant(_, discriminant) => *discriminant,
        }
    }
}

/// The body of code to evaluate in case of a match.
#[derive(Clone, PartialEq, Debug)]
pub struct Body {
    /// Any variables to bind before running the code.
    ///
    /// The tuples are in the form `(name, source)` (i.e `bla = source`).
    pub bindings: Vec<(String, Variable)>,

    /// The "code" to run in case of a match.
    ///
    /// We just use an integer for the sake of simplicity, but normally this
    /// would be an AST node, or perhaps an index to an array of AST nodes.
    pub value: Ast,
}

/// A variable used in a match expression.
///
/// In a real compiler these would probably be registers or some other kind of
/// variable/temporary generated by your compiler.
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Variable {
    id: usize,
    r#type: Type,
}

/// A type for storing diagnostics produced by the decision tree compiler.
#[derive(PartialEq, Debug)]
pub struct Diagnostics {
    /// A flag indicating the match is missing one or more pattern.
    pub missing: bool,

    /// The right-hand sides that are reachable.
    ///
    /// If a right-hand side isn't in this list it means its pattern is
    /// redundant.
    reachable: Vec<Body>,
}

/// A column in a pattern matching table.
///
/// A column contains a single variable to test, and a pattern to test against
/// that variable. A row may contain multiple columns, though this wouldn't be
/// exposed to the source language (= it's an implementation detail).
#[derive(Clone, PartialEq, Debug)]
pub struct Column {
    variable: Variable,
    pattern: Pattern,
}
impl Column {
    fn new(variable: Variable, pattern: Pattern) -> Self {
        Self { variable, pattern }
    }
}

/// A single case (or row) in a match expression/table.
#[derive(Clone, PartialEq, Debug)]
pub struct Row {
    columns: Vec<Column>,
    guard: Option<Ast>,
    body: Body,
}
impl Row {
    fn new(columns: Vec<Column>, guard: Option<Ast>, body: Body) -> Self {
        Self {
            columns,
            guard,
            body,
        }
    }

    fn remove_column(&mut self, variable: &Variable) -> Option<Column> {
        self.columns
            .iter()
            .position(|c| &c.variable == variable)
            .map(|idx| self.columns.remove(idx))
    }
}

/// A type constructor.
/*#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    True,
    False,
    Int(i64),
    Pair(TypeId, TypeId),
    Variant(TypeId, usize),
}*/

/// The match compiler
#[derive(PartialEq, Debug)]
pub struct Compiler<'ctx, 'program>
where
    'program: 'ctx,
{
    ctx: &'ctx mut Context<'program>,
    tested_type: Type,
    variable_id: usize,
    diagnostics: Diagnostics,
}

impl<'ctx, 'program> Compiler<'ctx, 'program> {
    pub fn new(ctx: &'ctx mut Context<'program>, tested_type: Type) -> Self {
        Self {
            ctx,
            tested_type,
            variable_id: 0,
            diagnostics: Diagnostics {
                missing: false,
                reachable: Vec::new(),
            },
        }
    }

    pub fn compile(mut self, cases: &Vec<Case>) -> Match {
        let variable = self.new_variable(self.tested_type.clone());

        let mut rows = vec![];
        for case in cases {
            rows.push(Row {
                columns: vec![Column {
                    variable: variable.clone(),
                    pattern: case.pattern.clone(),
                }],
                guard: case.guard.clone(),
                body: Body {
                    bindings: vec![],
                    value: case.body.clone(),
                },
            })
        }

        Match {
            tree: self.compile_rows(rows),
            diagnostics: self.diagnostics,
        }
    }

    /// Returns a new variable to use in the decision tree.
    ///
    /// In a real compiler you'd have to ensure these variables don't conflict
    /// with other variables.
    fn new_variable(&mut self, r#type: Type) -> Variable {
        let var = Variable {
            id: self.variable_id,
            r#type,
        };

        self.variable_id += 1;
        var
    }

    fn new_variables(&mut self, types: Vec<Type>) -> Vec<Variable> {
        types.iter().map(|t| self.new_variable(t.clone())).collect()
    }

    /// Given a row, returns the variable in that row that's referred to the
    /// most across all rows.
    fn branch_variable(&self, rows: &[Row]) -> Variable {
        let mut counts = HashMap::new();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable).or_insert(0_usize) += 1
            }
        }

        rows[0]
            .columns
            .iter()
            .map(|col| col.variable.clone())
            .max_by_key(|var| counts[var])
            .unwrap()
    }

    fn compile_rows(&mut self, mut rows: Vec<Row>) -> Decision {
        if rows.is_empty() {
            self.diagnostics.missing = true;
            return Decision::Failure;
        }

        // If the case pattern is only a variable, mark it as such and set the pattern to Empty (_)
        for row in rows.iter_mut() {
            move_variable_patterns(row);
        }

        // If the first row is Pattern::Empty, we can just return a success (or guard if provided) and discard everything else
        if rows.first().map_or(false, |c| c.columns.is_empty()) {
            let row = &mut rows.remove(0);
            self.diagnostics.reachable.push(row.body.clone());
            return if let Some(guard) = &row.guard {
                Decision::Guard {
                    clause: guard.clone(),
                    body: row.body.clone(),
                    subtree: Box::new(self.compile_rows(rows)),
                }
            } else {
                Decision::Success(row.body.clone())
            };
        }

        let branch_var = self.branch_variable(&rows);

        match branch_var.r#type.clone() {
            Type::Bool => Decision::Failure, //TODO should be a switch

            Type::Enum { variants, .. } => {
                let cases = variants
                    .iter()
                    .map(|variant| {
                        (
                            Constructor::Variant(branch_var.r#type.clone(), variant.discriminant),
                            self.new_variables(variant.types.clone()),
                            Vec::new(),
                        )
                    })
                    .collect();

                Decision::Switch(
                    branch_var.clone(),
                    self.compile_constructor_cases(rows, branch_var.clone(), cases),
                )
            }
            x => unimplemented!("{:?}", x),
        }
    }

    fn compile_constructor_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: Variable,
        mut cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)>,
    ) -> Vec<SwitchCase> {
        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_var) {
                match (col.pattern, &branch_var.r#type) {
                    (Pattern::Variant(name, args), Type::Enum { variants, .. }) => {
                        let idx = variants
                            .iter()
                            .find(|v| v.name == name)
                            .map(|v| v.discriminant)
                            .expect("variant does not exist");

                        let mut cols = row.columns;

                        for (var, pat) in cases[idx].1.iter().zip(args.into_iter()) {
                            cols.push(Column::new(var.clone(), pat));
                        }

                        cases[idx].2.push(Row::new(cols, row.guard, row.body));
                    }
                    x => todo!("{:?}", x),
                }

                /*if let Pattern::Constructor(cons, args) = col.pattern {
                    let idx = cons.index();
                    let mut cols = row.columns;

                    for (var, pat) in cases[idx].1.iter().zip(args.into_iter()) {
                        cols.push(Column::new(*var, pat));
                    }

                    cases[idx].2.push(Row::new(cols, row.guard, row.body));
                }*/
            } else {
                for (_, _, rows) in &mut cases {
                    rows.push(row.clone());
                }
            }
        }

        cases
            .into_iter()
            .map(|(cons, vars, rows)| SwitchCase::new(cons, vars, self.compile_rows(rows)))
            .collect()
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

fn move_variable_patterns(row: &mut Row) {
    row.columns.retain(|col| {
        if let Pattern::Variable(bind) = &col.pattern {
            row.body.bindings.push((bind.clone(), col.variable.clone()));
            false
        } else {
            true
        }
    });
}
