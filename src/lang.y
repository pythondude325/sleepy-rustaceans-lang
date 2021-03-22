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
    :  '(' Expr ')' { $2 }
    | 'INT' {
        let v = $1.map_err(|_| ())?;
        Ok(Token::Int{val: $lexer.span_str(v.span()).parse::<i32>().unwrap(), span: $span})
      }
    | 'FLOAT' {
        let v = $1.map_err(|_| ())?;
        Ok(Token::Float{val: $lexer.span_str(v.span()).parse::<f32>().unwrap(), span: $span})
      }
    ;

Unmatched -> ()
    :  "UNMATCHED" { }
    ;

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