%start Statement

%%

Statement -> Result<Locatable<Stmt>, ()>
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

Expr -> Result<Locatable<Token>, ()>
    : 'ADD' Operand 'AND' Operand { Ok($span.with(Token::Add{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'SUBTRACT' Operand 'FROM' Operand { Ok($span.with(Token::Sub{ lhs: Box::new($4?), rhs: Box::new($2?) })) }
    | 'MULTIPLY' Operand 'BY' Operand { Ok($span.with(Token::Mul{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'FADD' Operand 'AND' Operand { Ok($span.with(Token::FAdd{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'FSUBTRACT' Operand 'FROM' Operand { Ok($span.with(Token::FSub{ lhs: Box::new($4?), rhs: Box::new($2?) })) }
    | 'FMULTIPLY' Operand 'BY' Operand { Ok($span.with(Token::FMul{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    ;

Operand -> Result<Locatable<Token>, ()>
    :  '[' Expr ']' { $2 }
    | Number { $1 }
    | Fraction { $1 }
    ;

Fraction -> Result<Locatable<Token>, ()>
    : Number 'OVER' Number {
        match ($1, $3) {
            (Ok(Locatable { data: Token::Int { val: num }, .. }),
             Ok(Locatable { data: Token::Int { val: den }, .. })) => {
                Ok($span.with(Token::Fraction { num, den }))
            },
            _ => Err(()),
        }
    };

Number -> Result<Locatable<Token>, ()>
    : 'NEGATIVE' DigitList {
        let digit_list = $2?;
        Ok($span.with(Token::Int{ val: -digits_to_int(&digit_list)}))
    }
    | DigitList {
        let digit_list = $1?;
        Ok($span.with(Token::Int{ val: digits_to_int(&digit_list)}))
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
pub enum Token {
    Int{ val: i32 },
    Fraction { num: i32, den: i32 },
    Add {
        lhs: Box<Locatable<Token>>,
        rhs: Box<Locatable<Token>>,
    },
    Sub {
        lhs: Box<Locatable<Token>>,
        rhs: Box<Locatable<Token>>,
    },
    Mul {
        lhs: Box<Locatable<Token>>,
        rhs: Box<Locatable<Token>>,
    },
    FAdd {
        lhs: Box<Locatable<Token>>,
        rhs: Box<Locatable<Token>>,
    },
    FSub {
        lhs: Box<Locatable<Token>>,
        rhs: Box<Locatable<Token>>,
    },
    FMul {
        lhs: Box<Locatable<Token>>,
        rhs: Box<Locatable<Token>>,
    },
}

#[derive(Debug)]
pub enum Type {
    Integer,
    Fraction
}

#[derive(Debug)]
pub enum Stmt {
    Definition { variable_type: Type, identifier: String, value: Locatable<Token> },
    Assignment { identifier: String, value: Locatable<Token> },
    PrintInteger { value: Locatable<Token> },
    PrintFraction { value: Locatable<Token> },
    PrintString { value: Locatable<Token> },
    PrintNewline,
}

fn digits_to_int(digits: &[u8]) -> i32 {
    let mut integer_value: i32 = 0;
    for digit in digits {
        integer_value = integer_value * 10 + (*digit as i32);
    }
    integer_value
}