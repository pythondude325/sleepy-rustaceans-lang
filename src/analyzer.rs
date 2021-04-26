use crate::types::*;
use lasso::{Rodeo, Spur};
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
    },
    #[error("undeclared variable `{name}`")]
    UndeclaredVariable { name: String },
    #[error("Variable `{name}` declared twice")]
    DoubleDeclaration { name: String },
    #[error("Not enough arguments")]
    NotEnoughArguments,
}

impl SemanticError {
    fn invalid_type(expected: Type, found: Type) -> SemanticError {
        SemanticError::InvalidType {
            expected_type: expected,
            found_type: found,
        }
    }
}

pub type LocSemanticError = Locatable<SemanticError>;

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
    errors: Vec<LocSemanticError>,
}

impl<'ast> Analyzer<'ast> {
    fn new() -> Analyzer<'ast> {
        Analyzer {
            var_interner: Rodeo::default(),
            var_types: HashMap::new(),
            type_cache: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn lookup_variable(&self, ident: Locatable<&str>) -> Result<Type, LocSemanticError> {
        let ident_key = self.var_interner.get(ident.data).ok_or_else(|| {
            ident.location.with(SemanticError::UndeclaredVariable {
                name: ident.data.to_owned(),
            })
        })?;
        Ok(self
            .var_types
            .get(&ident_key)
            .expect("variable must exist")
            .clone())
    }

    pub fn typecheck_expression(
        &mut self,
        expression: &'ast LocExpression,
    ) -> Result<Type, LocSemanticError> {
        let expression_type = match &expression.data {
            Expression::Int { .. } => Type::Integer,
            Expression::Fraction { .. } => Type::Fraction,
            Expression::Add { lhs, rhs }
            | Expression::Sub { lhs, rhs }
            | Expression::Mul { lhs, rhs } => {
                let lhs_type = self.typecheck_expression(&lhs)?;
                if lhs_type != Type::Integer {
                    return Err(lhs
                        .location
                        .with(SemanticError::invalid_type(Type::Integer, lhs_type)));
                }

                let rhs_type = self.typecheck_expression(&rhs)?;
                if rhs_type != Type::Integer {
                    return Err(rhs
                        .location
                        .with(SemanticError::invalid_type(Type::Integer, rhs_type)));
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
                &[_] => return Err(expression.location.with(SemanticError::NotEnoughArguments)),
                &[ref first, ref rest @ ..] => {
                    let first_type = self.typecheck_expression(first)?;

                    for arg in rest {
                        let arg_type = self.typecheck_expression(arg)?;
                        if arg_type != first_type {
                            return Err(arg
                                .location
                                .with(SemanticError::invalid_type(first_type, arg_type)));
                        }
                    }

                    first_type
                }
            },
            Expression::Variable { ident } => {
                self.lookup_variable(expression.location.with(ident))?
            }
        };

        self.type_cache.insert(HashRef(expression), expression_type);

        return Ok(expression_type);
    }

    fn typecheck_condition(&mut self, cond: &'ast Locatable<Cond>) -> Result<(), LocSemanticError> {
        match &cond.data {
            Cond::Greater { lhs, rhs } | Cond::Equal { lhs, rhs } | Cond::Less { lhs, rhs } => {
                let lhs_type = self.typecheck_expression(lhs)?;
                let rhs_type = self.typecheck_expression(rhs)?;

                if lhs_type != rhs_type {
                    Err(rhs
                        .location
                        .with(SemanticError::invalid_type(lhs_type, rhs_type)))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn typecheck_statement(&mut self, statement: &'ast LocStmt) -> Result<(), LocSemanticError> {
        match &statement.data {
            Stmt::Assignment { identifier, value } => {
                let var_type = self.lookup_variable(identifier.as_ref().map(String::as_ref))?;
                let value_type = self.typecheck_expression(value)?;

                if var_type != value_type {
                    Err(value
                        .location
                        .with(SemanticError::invalid_type(var_type, value_type)))
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
                        Err(value
                            .location
                            .with(SemanticError::invalid_type(var_type, value_type)))
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
                    Err(value
                        .location
                        .with(SemanticError::invalid_type(Type::Integer, value_type)))
                } else {
                    Ok(())
                }
            }
            Stmt::PrintFraction { value } => {
                let value_type = self.typecheck_expression(value)?;
                if value_type != Type::Fraction {
                    Err(value
                        .location
                        .with(SemanticError::invalid_type(Type::Fraction, value_type)))
                } else {
                    Ok(())
                }
            }
            Stmt::While { condition, block } | Stmt::If { condition, block } => {
                self.typecheck_condition(condition)?;
                self.typecheck_block(block);
                Ok(())
            }
            _ => {
                // Don't do anything for unimplemented statements
                // TODO: Implement the rest of the statements
                Ok(())
            }
        }
    }

    fn typecheck_block(&mut self, statement_list: &'ast Locatable<StmtList>) {
        for stmt in &statement_list.data.stmts {
            match &stmt.data {
                Stmt::Definition {
                    variable_type,
                    identifier,
                    value: _,
                } => {
                    let result = {
                        let ident_key = self.var_interner.get_or_intern(&identifier.data);
                        if let Some(_) = self.var_types.insert(ident_key, *variable_type) {
                            Err(stmt.location.with(SemanticError::DoubleDeclaration {
                                name: identifier.data.clone(),
                            }))
                        } else {
                            Ok(())
                        }
                    };

                    if let Err(err) = result {
                        self.errors.push(err);
                    }
                }
                _ => {}
            }

            let result = self.typecheck_statement(stmt);

            if let Err(err) = result {
                self.errors.push(err);
            }
        }
    }

    pub fn typecheck_program(
        statement_list: &'ast Locatable<StmtList>,
    ) -> Result<HashMap<HashRef<'ast, LocExpression>, Type>, Vec<LocSemanticError>> {
        let mut analyzer = Analyzer::new();
        analyzer.typecheck_block(&statement_list);

        if analyzer.errors.is_empty() {
            Ok(analyzer.type_cache)
        } else {
            Err(analyzer.errors)
        }
    }
}
