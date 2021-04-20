%start Program

%%

Program -> Result<Locatable<StmtList>, ()>
    : 'BEGIN' StatementList 'END' { $2 };

Statement -> Result<LocStmt, ()>
    : 'DEFINE' Type Identifier 'AS' Operand {
        Ok($span.with(Stmt::Definition {
            variable_type: $2?,
            identifier: $3?,
            value: Some($5?),
        }))
    }
    | 'DEFINE' Type Identifier {
        Ok($span.with(Stmt::Definition {
            variable_type: $2?,
            identifier: $3?,
            value: None,
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
    | 'PRINTS' StringLiteral { Ok($span.with(Stmt::PrintString { value: $2? })) }
    | 'PRINTNL' { Ok($span.with(Stmt::PrintNewline)) }
    | If  { $1 }
    | While { $1 }
    ;

StatementList -> Result<Locatable<StmtList>, ()>
    : StatementList Statement { 
        let mut stmt_list = $1?.data;
        stmt_list.stmts.push($2?);
        Ok($span.with(stmt_list))
    }
    | Statement { Ok($span.with(StmtList { stmts: vec![$1?] })) }
    ;

Conditional -> Result<Locatable<Cond>, ()>
    : Operand 'GREATER' Operand { Ok($span.with(Cond::Greater { lhs: $1?, rhs: $3? })) }
    | Operand 'EQUAL' Operand   { Ok($span.with(Cond::Equal { lhs: $1?, rhs: $3? })) }
    | Operand 'LESS' Operand    { Ok($span.with(Cond::Less { lhs: $1?, rhs: $3? })) }
    ;

If -> Result<Locatable<Stmt>, ()>
    : 'IF' '(' Conditional ')' StatementList 'BLOCK'  { Ok($span.with(Stmt::If { condition: $3?, block: $5? })) }
    ;

While -> Result<Locatable<Stmt>, ()>
    : 'WHILE' '(' Conditional ')' StatementList 'BLOCK' { Ok($span.with(Stmt::While { condition: $3?, block: $5? })) }
    ;

Type -> Result<Type, ()>
    : 'INTEGER' { Ok(Type::Integer) }
    | 'FRACTION' { Ok(Type::Fraction) }
    ;

Identifier -> Result<String, ()>
    : 'IDENTIFIER' {
        Ok($lexer.span_str($1.map_err(|_| ())?.span()).to_owned())
    }
    ;

StringLiteral -> Result<String, ()>
    : 'STRING_LITERAL' {
        Ok($lexer.span_str(string_literal_span($1.map_err(|_| ())?.span())).to_owned())
    }
    ;

Expr -> Result<LocExpression, ()>
    : 'PUT' Operand { $2 }
    | 'ADD' Operand 'AND' Operand { Ok($span.with(Expression::Add{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'SUBTRACT' Operand 'FROM' Operand { Ok($span.with(Expression::Sub{ lhs: Box::new($4?), rhs: Box::new($2?) })) }
    | 'MULTIPLY' Operand 'BY' Operand { Ok($span.with(Expression::Mul{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'FADD' Operand 'AND' Operand { Ok($span.with(Expression::FAdd{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'FSUBTRACT' Operand 'FROM' Operand { Ok($span.with(Expression::FSub{ lhs: Box::new($4?), rhs: Box::new($2?) })) }
    | 'FMULTIPLY' Operand 'BY' Operand { Ok($span.with(Expression::FMul{ lhs: Box::new($2?), rhs: Box::new($4?) })) }
    | 'MAX' 'OF' ArgumentList { Ok($span.with(Expression::Max { args: $3? })) }
    ;

Operand -> Result<LocExpression, ()>
    :  '[' Expr ']' { $2 }
    | Number { $1 }
    | Fraction { $1 }
    | Identifier { Ok($span.with(Expression::Variable { ident: $1? })) }
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
        let mut new_digits = $1?;
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

ArgumentList -> Result<Locatable<Vec<LocExpression>>, ()>
    : Operand { Ok($span.with(vec![$1?])) }
    | ArgumentList "AND" Operand {
        let mut list: Vec<LocExpression> = $1?.data;
        list.push($3?);
        Ok($span.with(list))
    };

Unmatched -> (): "UNMATCHED" { };

%%

use crate::types::*;
use lrpar::Span;

fn digits_to_int(digits: &[u8]) -> Option<i32> {
    let mut integer_value: i32 = 0;
    for digit in digits {
        integer_value = integer_value.checked_mul(10)?.checked_add(*digit as i32)?;
    }
    Some(integer_value)
}

fn string_literal_span(s: Span) -> Span {
    Span::new(s.start() + 1, s.end() - 1) 
}
