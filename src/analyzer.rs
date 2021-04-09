use thiserror::Error;
use crate::types::*;
use lasso::{Rodeo, Spur};
use std::collections::HashMap;

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
}

impl SemanticError {
    fn invalid_type(expected: Type, found: Type) -> SemanticError {
        SemanticError::InvalidType {
            expected_type: expected,
            found_type: found,
        }
    }
}

pub struct Analyzer {
    var_interner: lasso::Rodeo,
    var_types: HashMap<Spur, Type>,
}

impl Analyzer {
    fn new() -> Analyzer {
        Analyzer {
            var_interner: Rodeo::default(),
            var_types: HashMap::new(),
        }
    }

    fn lookup_variable(&self, ident: &str) -> Result<Type, SemanticError> {
        let ident_key = self.var_interner
            .get(ident)
            .ok_or_else(|| SemanticError::UndeclaredVariable { name: ident.to_owned() })?;
        Ok(self.var_types.get(&ident_key).expect("variable must exist").clone())
    }

    pub fn typecheck_expression(&self, expression: &LocExpression) -> Result<Type, SemanticError> {
        Ok(match &expression.data {
            Expression::Int { .. } => Type::Integer,
            Expression::Fraction { .. } => Type::Fraction,
            Expression::Add { lhs, rhs}
            | Expression::Sub { lhs, rhs}
            | Expression::Mul { lhs, rhs } => {
                let lhs_type = self.typecheck_expression(&lhs)?;
                if lhs_type != Type::Integer {
                    return Err(SemanticError::invalid_type(Type::Integer, lhs_type));
                }
    
                let rhs_type = self.typecheck_expression(&rhs)?;
                if rhs_type != Type::Integer {
                    return Err(SemanticError::invalid_type(Type::Integer, rhs_type));
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
            Expression::Variable { ident } => { self.lookup_variable(ident)? }
        })
    }    

    fn typecheck_statement(&mut self, statement: &LocStmt) -> Result<(), SemanticError> {
        match &statement.data {
            Stmt::Assignment {identifier, value} => {
                let var_type = self.lookup_variable(identifier)?;
                let value_type = self.typecheck_expression(value)?;

                if var_type != value_type {
                    Err(SemanticError::invalid_type(var_type, value_type))
                } else {
                    Ok(())
                }
            },
            Stmt::Definition { variable_type, value, .. } => {
                let var_type = *variable_type;
                let value_type = self.typecheck_expression(value)?;
                if var_type != value_type {
                    Err(SemanticError::invalid_type(var_type, value_type))
                } else {
                    Ok(())
                }
            }
            _ => {
                unimplemented!()
            }
        }
    }

    fn define_variable(&mut self, identifier: &str, var_type: &Type) -> Result<(), SemanticError> {
        let ident_key = self.var_interner.get_or_intern(identifier);
        if let Some(_) = self.var_types.insert(ident_key, *var_type) {
            Err(SemanticError::DoubleDeclaration { name: identifier.to_owned() })
        } else {
            Ok(())
        }
    }

    pub fn typecheck_program(statment_list: &Locatable<StmtList>) -> Result<(), SemanticError> {
        let mut analyzer = Analyzer::new();
        
        for stmt in &statment_list.data.stmts {
            match &stmt.data {
                Stmt::Definition { variable_type, identifier, value: _} => {
                    analyzer.define_variable(identifier, variable_type)?;
                }
                _ => {}
            }

            analyzer.typecheck_statement(stmt)?;
        }

        Ok(())
    }
}