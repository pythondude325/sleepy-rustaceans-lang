%start Expr
%avoid_insert "INT"

%%

Expr -> Result<Token, ()>
    :  Expr '+' Term { Ok(Token::Add{span: $span, lhs: Box::new($1?), rhs: Box::new($3?)}) }
    |  Expr '-' Term { Ok(Token::Sub{span: $span, lhs: Box::new($1?), rhs: Box::new($3?)}) }
    | Term { $1 }
    ;

Term -> Result<Token, ()>
    :  Term '*' Factor { Ok(Token::Mul{span: $span, lhs: Box::new($1?), rhs: Box::new($3?)}) }
    |  Term '/' Factor { Ok(Token::Div{span: $span, lhs: Box::new($1?), rhs: Box::new($3?)}) }
    | Factor { $1 }
    ;

Factor -> Result<Token, ()>
    :  '[' Expr ']' { $2 }
    | Number { $1 }
    | 'INT' {
        let v = $1.map_err(|_| ())?;
        Ok(Token::Int{val: $lexer.span_str(v.span()).parse::<i32>().unwrap(), span: $span})
      }
    | 'FLOAT' {
        let v = $1.map_err(|_| ())?;
        Ok(Token::Float{val: $lexer.span_str(v.span()).parse::<f32>().unwrap(), span: $span})
      }
    ;

Number -> Result<Token, ()>
    : DigitList {
        let digit_list = $1?;
        let mut integer_value: i32 = 0;
        for digit in &digit_list {
            integer_value = integer_value * 10 + (*digit as i32);
        }
        Ok(Token::Int{ val: integer_value, span: $span })
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
pub enum Token {
    Int{ val: i32, span: Span },
    Float{ val: f32, span: Span },
    Add {
        span: Span,
        lhs: Box<Token>,
        rhs: Box<Token>,
    },
    Sub {
        span: Span,
        lhs: Box<Token>,
        rhs: Box<Token>,
    },
    Mul {
        span: Span,
        lhs: Box<Token>,
        rhs: Box<Token>,
    },
    Div {
        span: Span,
        lhs: Box<Token>,
        rhs: Box<Token>,
    },
}