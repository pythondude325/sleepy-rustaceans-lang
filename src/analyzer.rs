use thiserror::Error;
use crate::types::{Type, Expression, LocExpression};

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("invalid type. expected {expected_type} got {found_type}")]
    InvalidType {
        expected_type: Type,
        found_type: Type,
    }
}

impl TypeError {
    fn invalid_type(expected: Type, found: Type) -> TypeError {
        TypeError::InvalidType {
            expected_type: expected,
            found_type: found,
        }
    }
}

pub fn typecheck_expression(expression: &LocExpression) -> Result<Type, TypeError> {
    Ok(match expression.data {
        Expression::Int { .. } => Type::Integer,
        Expression::Fraction { .. } => Type::Fraction,
        Expression::Add { ref lhs, ref rhs}
        | Expression::Sub { ref lhs, ref rhs}
        | Expression::Mul { ref lhs, ref rhs } => {
            let lhs_type = typecheck_expression(&lhs)?;
            if lhs_type != Type::Integer {
                return Err(TypeError::invalid_type(Type::Integer, lhs_type));
            }

            let rhs_type = typecheck_expression(&rhs)?;
            if rhs_type != Type::Integer {
                return Err(TypeError::invalid_type(Type::Integer, rhs_type));
            }

            Type::Integer
        }
        Expression::FAdd { ref lhs, ref rhs }
        | Expression::FSub { ref lhs, ref rhs }
        | Expression::FMul { ref lhs, ref rhs } => {
            // These are allowed to be whatever because they will be casted to a fraction.
            let _lhs_type = typecheck_expression(&lhs)?;
            let _rhs_type = typecheck_expression(&rhs)?;

            Type::Fraction
        }
        Expression::Variable { .. } => {
            unimplemented!()
        }
    })
}