%start Statement

%%

Statement -> Result<LocStmt, ()>
    : 'DEFINE' Type Identifier 'AS' Operand {
        Ok($span.with(Stmt::Definition {
            variable_type: $2?,
            identifier: $3?,
            value: $5?,
        }))
    }
    | Expr 'INTO' Identifier {
        Ok($span.with(Stmt::Assignment {
            value: $1?,
            identifier: $3?,
        }))
    }
    | 'PRINTI' Operand { Ok($span.with(Stmt::PrintInteger { value: $2? })) }
    | 'PRINTF' Operand { Ok($span.with(Stmt::PrintFraction { value: $2? })) }
    | 'PRINTS' Operand { Ok($span.with(Stmt::PrintString { value: $2? })) }
    | 'PRINTNL' { Ok($span.with(Stmt::PrintNewline)) }
    ;

Type -> Result<Type, ()>
    : 'INTEGER' { Ok(Type::Integer) }
    | 'FRACTION' { Ok(Type::Fraction) }
    ;

Identifier -> Result<String, ()>
    : 'IDENTIFIER' {
        Ok($lexer.span_str($1.map_err(|_| ())?.span()).to_owned())
    };

Expr -> Result<LocExpression, ()>
    : 'ADD' Operand 'AND' Operand { Ok($span.with(Expression::Add{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'SUBTRACT' Operand 'FROM' Operand { Ok($span.with(Expression::Sub{ lhs: Box::new($4?), rhs: Box::new($2?) })) }
    | 'MULTIPLY' Operand 'BY' Operand { Ok($span.with(Expression::Mul{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'FADD' Operand 'AND' Operand { Ok($span.with(Expression::FAdd{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'FSUBTRACT' Operand 'FROM' Operand { Ok($span.with(Expression::FSub{ lhs: Box::new($4?), rhs: Box::new($2?) })) }
    | 'FMULTIPLY' Operand 'BY' Operand { Ok($span.with(Expression::FMul{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    ;

Operand -> Result<LocExpression, ()>
    :  '[' Expr ']' { $2 }
    | Number { $1 }
    | Fraction { $1 }
    ;

Fraction -> Result<LocExpression, ()>
    : Number 'OVER' Number {
        match ($1, $3) {
            (Ok(Locatable { data: Expression::Int { val: num }, .. }),
             Ok(Locatable { data: Expression::Int { val: den }, .. })) => {
                Ok($span.with(Expression::Fraction { num, den }))
            },
            _ => Err(()),
        }
    };

Number -> Result<LocExpression, ()>
    : 'NEGATIVE' DigitList {
        let digit_list = $2?;
        Ok($span.with(Expression::Int{ val: -digits_to_int(&digit_list).ok_or(())?}))
    }
    | DigitList {
        let digit_list = $1?;
        Ok($span.with(Expression::Int{ val: digits_to_int(&digit_list).ok_or(())?}))
    }
    ;

DigitList -> Result<Vec<u8>, ()>
    : DigitList Digit {
        let mut new_digits = $1?.clone();
        new_digits.push($2?);
        Ok(new_digits)
    }
    | Digit { Ok(vec![$1?]) }
    ;

Digit -> Result<u8, ()> : 'DIGIT'
    {
        let v = $1.map_err(|_| ())?;
        let value = match $lexer.span_str(v.span()) {
            "zero" => 0,
            "one" => 1,
            "two" => 2,
            "three" => 3,
            "four" => 4,
            "five" => 5,
            "six" => 6,
            "seven" => 7,
            "eight" => 8,
            "nine" => 9,
            _ => return Err(()),
        };
        Ok(value)
    };

Unmatched -> (): "UNMATCHED" { };

%%

use lrpar::Span;

#[derive(Debug)]
pub struct Locatable<T>{
    pub data: T,
    pub location: Span,
}

trait LocatableExt {
    fn with<T>(self, data: T) -> Locatable<T>;
}

impl LocatableExt for Span {
    fn with<T>(self, data: T) -> Locatable<T> {
        Locatable { data, location: self }
    }
}

#[derive(Debug)]
pub enum Expression {
    Int{ val: i32 },
    Fraction { num: i32, den: i32 },
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
}

type LocExpression = Locatable<Expression>;

#[derive(Debug)]
pub enum Type {
    Integer,
    Fraction
}

#[derive(Debug)]
pub enum Stmt {
    Definition { variable_type: Type, identifier: String, value: LocExpression },
    Assignment { identifier: String, value: LocExpression },
    PrintInteger { value: LocExpression },
    PrintFraction { value: LocExpression },
    PrintString { value: LocExpression },
    PrintNewline,
}

type LocStmt = Locatable<Stmt>;

fn digits_to_int(digits: &[u8]) -> Option<i32> {
    let mut integer_value: i32 = 0;
    for digit in digits {
        integer_value = integer_value.checked_mul(10)?.checked_add(*digit as i32)?;
    }
    Some(integer_value)
}