use crate::types::*;
use lasso::{Rodeo, Spur};
use lrpar::Span;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::{cmp, ops};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("invalid type. expected {expected_type} got {found_type}")]
    InvalidType {
        expected_type: Type,
        found_type: Type,
        location: Span,
    },
    #[error("undeclared variable `{name}`")]
    UndeclaredVariable { name: String },
    #[error("Variable `{name}` declared twice")]
    DoubleDeclaration { name: String },
    #[error("Not enough arguments")]
    NotEnoughArguments { location: Span },
}

impl SemanticError {
    fn invalid_type(expected: Type, found: Type, span: Span) -> SemanticError {
        SemanticError::InvalidType {
            expected_type: expected,
            found_type: found,
            location: span,
        }
    }
}

pub struct HashRef<'live, T>(pub &'live T);

impl<T> cmp::PartialEq for HashRef<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> cmp::Eq for HashRef<'_, T> {}

impl<T> Hash for HashRef<'_, T> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        std::ptr::hash(self.0, h);
    }
}

impl<T> ops::Deref for HashRef<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        return self.0;
    }
}

pub struct Analyzer<'ast> {
    var_interner: lasso::Rodeo,
    var_types: HashMap<Spur, Type>,
    type_cache: HashMap<HashRef<'ast, LocExpression>, Type>,
    program_data: String,
}

struct TextPosition {
    line_number: i32,
    col_number: i32,
    line_start: usize,
    line_end: usize,
    length: usize,
}

// impl Analyzer {
//     fn new(program: &String) -> Analyzer {
//         Analyzer {
//             var_interner: Rodeo::default(),
//             var_types: HashMap::new(),
//             program_data: program.clone(),
// }

impl<'ast> Analyzer<'ast> {
    fn new(program: &String) -> Analyzer<'ast> {
        Analyzer {
            var_interner: Rodeo::default(),
            var_types: HashMap::new(),
            type_cache: HashMap::new(),
            program_data: program.clone(),
        }
    }

    fn lookup_variable(&self, ident: &str) -> Result<Type, SemanticError> {
        let ident_key =
            self.var_interner
                .get(ident)
                .ok_or_else(|| SemanticError::UndeclaredVariable {
                    name: ident.to_owned(),
                })?;
        Ok(self
            .var_types
            .get(&ident_key)
            .expect("variable must exist")
            .clone())
    }
    fn get_pos(&self, location: Span) -> TextPosition {
        let mut n = 1;
        let mut pos = 0;
        let mut temp = 0;
        for c in self.program_data.chars().take(location.start()) {
            if c == '\n' {
                n += 1;
                pos += temp;
                temp = 0;
            }
            temp += 1;
        }
        let mut i = location.end();
        let mut c = self.program_data.chars().nth(i).unwrap();
        while c != '\n' {
            i += 1;
            c = self.program_data.chars().nth(i).unwrap();
        }
        return TextPosition {
            line_number: n,
            col_number: location.start() as i32 - pos,
            line_start: (pos + 1) as usize,
            line_end: i,
            length: location.end() - location.start(),
        };
    }

    fn print_error(&self, location: Span) {
        let error_pos = self.get_pos(location);

        let mut line = error_pos.line_number.to_string();
        line.push_str("| ");
        line.push_str(
            self.program_data
                .get(error_pos.line_start..error_pos.line_end)
                .unwrap(),
        );
        let mut error = String::from("^");
        error.push_str(&"~".repeat(error_pos.length - 1));
        println!("{}", line);
        println!(
            "{:indent$}{}",
            "",
            error,
            indent = (error_pos.col_number + 2) as usize
        );
    }
    pub fn typecheck_expression(
        &mut self,
        expression: &'ast LocExpression,
    ) -> Result<Type, SemanticError> {
        let expression_type = match &expression.data {
            Expression::Int { .. } => Type::Integer,
            Expression::Fraction { .. } => Type::Fraction,
            Expression::Add { lhs, rhs }
            | Expression::Sub { lhs, rhs }
            | Expression::Mul { lhs, rhs } => {
                let lhs_type = self.typecheck_expression(&lhs)?;
                if lhs_type != Type::Integer {
                    return Err(SemanticError::invalid_type(
                        Type::Integer,
                        lhs_type,
                        lhs.location,
                    ));
                }

                let rhs_type = self.typecheck_expression(&rhs)?;
                if rhs_type != Type::Integer {
                    return Err(SemanticError::invalid_type(
                        Type::Integer,
                        rhs_type,
                        rhs.location,
                    ));
                }

                Type::Integer
            }
            Expression::FAdd { lhs, rhs }
            | Expression::FSub { lhs, rhs }
            | Expression::FMul { lhs, rhs } => {
                // These are allowed to be whatever because they will be casted to a fraction.
                let _lhs_type = self.typecheck_expression(&lhs)?;
                let _rhs_type = self.typecheck_expression(&rhs)?;

                Type::Fraction
            }
            Expression::Max { args } => match args.data.as_slice() {
                &[] => unreachable!("arg list must have at least one argument"),
                &[_] => {
                    return Err(SemanticError::NotEnoughArguments {
                        location: expression.location,
                    })
                }
                &[ref first, ref rest @ ..] => {
                    let first_type = self.typecheck_expression(first)?;

                    for arg in rest {
                        let arg_type = self.typecheck_expression(arg)?;
                        if arg_type != first_type {
                            return Err(SemanticError::invalid_type(
                                first_type,
                                arg_type,
                                arg.location,
                            ));
                        }
                    }

                    first_type
                }
            },
            Expression::Variable { ident } => self.lookup_variable(ident)?,
        };

        self.type_cache.insert(HashRef(expression), expression_type);

        return Ok(expression_type);
    }

    fn typecheck_condition(&mut self, cond: &'ast Locatable<Cond>) -> Result<(), SemanticError> {
        match &cond.data {
            Cond::Greater { lhs, rhs } | Cond::Equal { lhs, rhs } | Cond::Less { lhs, rhs } => {
                let lhs_type = self.typecheck_expression(lhs)?;
                let rhs_type = self.typecheck_expression(rhs)?;

                if lhs_type != rhs_type {
                    Err(SemanticError::invalid_type(
                        lhs_type,
                        rhs_type,
                        rhs.location,
                    ))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn typecheck_statement(&mut self, statement: &'ast LocStmt) -> Result<(), SemanticError> {
        match &statement.data {
            Stmt::Assignment { identifier, value } => {
                let var_type = self.lookup_variable(identifier)?;
                let value_type = self.typecheck_expression(value)?;

                if var_type != value_type {
                    Err(SemanticError::invalid_type(
                        var_type,
                        value_type,
                        value.location,
                    ))
                } else {
                    Ok(())
                }
            }
            Stmt::Definition {
                variable_type,
                value,
                ..
            } => {
                let var_type = *variable_type;
                if let Some(value) = value {
                    let value_type = self.typecheck_expression(value)?;

                    if var_type != value_type {
                        Err(SemanticError::invalid_type(
                            var_type,
                            value_type,
                            value.location,
                        ))
                    } else {
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Stmt::PrintInteger { value } => {
                let value_type = self.typecheck_expression(value)?;
                if value_type != Type::Integer {
                    self.print_error(value.location);
                    Err(SemanticError::invalid_type(
                        Type::Integer,
                        value_type,
                        value.location,
                    ))
                } else {
                    Ok(())
                }
            }
            Stmt::PrintFraction { value } => {
                let value_type = self.typecheck_expression(value)?;
                if value_type != Type::Fraction {
                    Err(SemanticError::invalid_type(
                        Type::Fraction,
                        value_type,
                        value.location,
                    ))
                } else {
                    Ok(())
                }
            }
            Stmt::While { condition, block } | Stmt::If { condition, block } => {
                self.typecheck_condition(condition)?;
                self.typecheck_block(block)?;
                Ok(())
            }
            _ => {
                // Don't do anything for unimplemented statements
                // TODO: Implement the rest of the statements
                Ok(())
            }
        }
    }

    fn define_variable(&mut self, identifier: &str, var_type: &Type) -> Result<(), SemanticError> {
        let ident_key = self.var_interner.get_or_intern(identifier);
        if let Some(_) = self.var_types.insert(ident_key, *var_type) {
            Err(SemanticError::DoubleDeclaration {
                name: identifier.to_owned(),
            })
        } else {
            Ok(())
        }
    }

    fn typecheck_block(
        &mut self,
        statement_list: &'ast Locatable<StmtList>,
    ) -> Result<(), SemanticError> {
        for stmt in &statement_list.data.stmts {
            match &stmt.data {
                Stmt::Definition {
                    variable_type,
                    identifier,
                    value: _,
                } => {
                    self.define_variable(identifier, variable_type)?;
                }
                _ => {}
            }

            self.typecheck_statement(stmt)?;
        }
        Ok(())
    }

    pub fn typecheck_program(
        statement_list: &'ast Locatable<StmtList>,
        buffer: &String,
    ) -> Result<HashMap<HashRef<'ast, LocExpression>, Type>, SemanticError> {
        let mut analyzer = Analyzer::new(buffer);
        analyzer.typecheck_block(&statement_list)?;
        Ok(analyzer.type_cache)
    }
}
