use lrpar::Span;
use std::fmt;

#[derive(Debug)]
pub struct Locatable<T> {
    pub data: T,
    pub location: Span,
}

pub trait LocatableExt {
    fn with<T>(self, data: T) -> Locatable<T>;
}

impl LocatableExt for Span {
    fn with<T>(self, data: T) -> Locatable<T> {
        Locatable {
            data,
            location: self,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Int {
        val: i32,
    },
    Fraction {
        num: i32,
        den: i32,
    },
    Variable {
        ident: String,
    },
    Add {
        lhs: Box<LocExpression>,
        rhs: Box<LocExpression>,
    },
    Sub {
        lhs: Box<LocExpression>,
        rhs: Box<LocExpression>,
    },
    Mul {
        lhs: Box<LocExpression>,
        rhs: Box<LocExpression>,
    },
    FAdd {
        lhs: Box<LocExpression>,
        rhs: Box<LocExpression>,
    },
    FSub {
        lhs: Box<LocExpression>,
        rhs: Box<LocExpression>,
    },
    FMul {
        lhs: Box<LocExpression>,
        rhs: Box<LocExpression>,
    },
    Max {
        args: Locatable<Vec<LocExpression>>,
    }
}

pub type LocExpression = Locatable<Expression>;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Type {
    Integer,
    Fraction,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer { .. } => {
                write!(f, "integer")?;
            }
            Type::Fraction { .. } => {
                write!(f, "fraction")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Stmt {
    Definition {
        variable_type: Type,
        identifier: String,
        value: Option<LocExpression>,
    },
    Assignment {
        identifier: String,
        value: LocExpression,
    },
    PrintInteger {
        value: LocExpression,
    },
    PrintFraction {
        value: LocExpression,
    },
    PrintString {
        value: LocExpression,
    },
    PrintNewline,
    If {
        condition: Locatable<Cond>,
        block: Locatable<StmtList>,
    },
    While {
        condition: Locatable<Cond>,
        block: Locatable<StmtList>,
    },
}

pub type LocStmt = Locatable<Stmt>;

#[derive(Debug)]
pub struct StmtList {
    pub stmts: Vec<Locatable<Stmt>>,
}

#[derive(Debug)]
pub enum Cond {
    Greater {
        lhs: LocExpression,
        rhs: LocExpression,
    },
    Equal {
        lhs: LocExpression,
        rhs: LocExpression,
    },
    Less {
        lhs: LocExpression,
        rhs: LocExpression,
    },
}
