use std::collections::HashMap;
use reduce::Reduce;

use crate::types::*;
use crate::analyzer::HashRef;

pub struct Compiler<'ast> {
    //main_body: String,
    vars: HashMap<String, (String, Type)>,
    curr_int: i32,
    curr_frac: i32,
    type_cache: HashMap<HashRef<'ast, LocExpression>, Type>,
}

impl<'ast> Compiler<'ast> {
    pub fn new(type_cache: HashMap<HashRef<'ast, LocExpression>, Type>) -> Self {
        Compiler {
            vars: HashMap::new(),
            curr_int: 0,
            curr_frac: 0,
            type_cache
        }
    }

    pub fn compile_program(&mut self, program: &Locatable<StmtList>) -> Result<String, &'static str> {
        Ok(template_program(&self.compile_stmt_list(&program.data)?))
    }

    fn compile_stmt_list(&mut self, stmt_list: &StmtList) -> Result<String, &'static str> {
        let mut body = String::new();

        for stmt in &stmt_list.stmts {
            body += self.compile_stmt(stmt)?.as_str();
        }

        Ok(body)
    }

    fn compile_stmt(&mut self, stmt: &LocStmt) -> Result<String, &'static str> {
        Ok(match stmt.data {
            Stmt::Definition { ref variable_type, ref identifier, ref value } => self.build_definition(variable_type, identifier, value)?,
            Stmt::Assignment { ref identifier, ref value } => self.build_assignment(identifier, value)?,
            Stmt::PrintString { ref value } => self.build_print_string(value)?,
            Stmt::PrintInteger { ref value } => self.build_print_integer(value)?,
            Stmt::PrintFraction { ref value } => self.build_print_fraction(value)?,
            Stmt::PrintNewline { .. } => self.build_print_newline()?,
            Stmt::If { ref condition, ref block } => self.build_if(condition, block)?,
            Stmt::While { ref condition, ref block } => self.build_while(condition, block)?,
        })
    }

    fn check_type(&self, expr: &LocExpression) -> Type {
        match expr.data {
            Expression::Int { .. } | Expression::Add { .. } | Expression::Sub { .. } | Expression::Mul { .. } => Type::Integer,
            Expression::Fraction { .. } | Expression::FAdd { .. } | Expression::FSub { .. } | Expression::FMul { .. } => Type::Fraction,
            Expression::Variable { ref ident } => self.vars.get(ident).unwrap().1,
            _ => unimplemented!(),
        }
    }

    fn build_definition(&mut self, variable_type: &Type, identifier: &String, value: &Option<LocExpression>) -> Result<String, &'static str> {
        Ok(match variable_type {
            Type::Integer => {
                let expression_text = match value {
                    Some(expr) => self.build_expression(expr, Some(Type::Integer))?,
                    None => String::from("0"),
                };

                let name = format!("__int{}", self.curr_int);
                self.vars.insert(identifier.clone(), (name.clone(), Type::Integer));

                self.curr_int += 1;
                format!("int {} = {};\n", name, expression_text)
            }
            Type::Fraction => {
                let expression_text = match value {
                    Some(expr) => self.build_expression(expr, Some(Type::Fraction))?,
                    None => String::from("new_frac(0, 0)"),
                };

                let name = format!("__frac{}", self.curr_frac);
                self.vars.insert(identifier.clone(), (name.clone(), Type::Fraction));

                self.curr_frac += 1;
                format!("frac {} = {};\n", name, expression_text)
            }
        })
    }

    fn build_assignment(&mut self, identifier: &String, value: &LocExpression) -> Result<String, &'static str> {
        let expression_text = self.build_expression(value, None)?;
        Ok(format!("{} = {};\n", self.vars.get(identifier).unwrap().0, expression_text))
    }

    fn build_print_string(&mut self, s: &String) -> Result<String, &'static str> {
        Ok(format!("printf(\"%s\", \"{}\");\n", s))
    }

    fn build_print_integer(&mut self, val: &LocExpression) -> Result<String, &'static str> {
        Ok(format!("printf(\"%d\", {});\n", self.build_expression(val, Some(Type::Integer))?))
    }

    fn build_print_fraction(&mut self, val: &LocExpression) -> Result<String, &'static str> {
        Ok(format!("print_fraction({});\n", self.build_expression(val, Some(Type::Fraction))?))
    }

    fn build_print_newline(&mut self) -> Result<String, &'static str> {
        Ok(String::from("printf(\"\\n\");\n"))
    }

    fn build_expression(&mut self, val: &LocExpression, target_type: Option<Type>) -> Result<String, &'static str> {
        Ok(match val.data {
            Expression::Int { ref val } => match target_type {
                Some(t) => match t { Type::Integer => format!("{}", *val), Type::Fraction => format!("new_frac({},1)", *val) },
                None => format!("{}", *val)
            },
            Expression::Add { ref lhs, ref rhs } => match target_type {
                Some(t) => match t {
                    Type::Integer => format!("{}+{}", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?),
                    Type::Fraction => format!("frac_new({}+{}, 1)", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?)
                },
                None => format!("{}+{}", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?)
            },
            Expression::Sub { ref lhs, ref rhs } => match target_type {
                Some(t) => match t {
                    Type::Integer => format!("{}-{}", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?),
                    Type::Fraction => format!("frac_new({}-{}, 1)", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?)
                },
                None => format!("{}-{}", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?)
            },
            Expression::Mul { ref lhs, ref rhs } => match target_type {
                Some(t) => match t {
                    Type::Integer => format!("{}*{}", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?),
                    Type::Fraction => format!("frac_new({}*{}, 1)", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?)
                },
                None => format!("{}*{}", self.build_expression(lhs, None)?, self.build_expression(rhs, None)?)
            },

            Expression::Fraction { ref num, ref den } => format!("new_frac({}, {})", num, den),
            Expression::FAdd { ref lhs, ref rhs } => format!("frac_add({}, {})", self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?),
            Expression::FSub { ref lhs, ref rhs } => format!("frac_sub({}, {})", self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?),
            Expression::FMul { ref lhs, ref rhs } => format!("frac_mul({}, {})", self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?),

            Expression::Max { ref args } => {
                let frac = self.type_cache.get(&HashRef(val)).unwrap() == &Type::Fraction;
                args.data
                    .iter()
                    .map(|e| self.build_expression(e, Some(if frac { Type::Fraction } else { Type::Integer })).unwrap())
                    .reduce(|acc, x| format!("max{}({},{})", if frac { "_f" } else { "" }, x, acc))
                    .unwrap()
            },
            Expression::Variable { ref ident } => {
                match target_type {
                    Some(t) => match (self.vars.get(ident).unwrap().1, t) {
                        (Type::Integer, Type::Integer) | (Type::Fraction, Type::Fraction) => self.vars.get(ident).unwrap().0.clone(),
                        (Type::Integer, Type::Fraction) => format!("new_frac({}, 1)", self.vars.get(ident).unwrap().0.clone()),
                        _ => unreachable!(),
                    }
                    None => self.vars.get(ident).unwrap().0.clone()
                }
            },
        })
    }

    fn build_if(&mut self, condition: &Locatable<Cond>, block: &Locatable<StmtList>) -> Result<String, &'static str> {
        let body_text = self.compile_stmt_list(&block.data)?;
        let cond_text = self.build_condition(condition)?;

        Ok(format!("if ({}) {{\n{}}}\n", cond_text, body_text))
    }

    fn build_while(&mut self, condition: &Locatable<Cond>, block: &Locatable<StmtList>) -> Result<String, &'static str> {
        let body_text = self.compile_stmt_list(&block.data)?;
        let cond_text = self.build_condition(condition)?;

        Ok(format!("while ({}) {{\n{}}}\n", cond_text, body_text))
    }

    fn build_condition(&mut self, condition: &Locatable<Cond>) -> Result<String, &'static str> {
        Ok(match condition.data {
            Cond::Greater { ref lhs, ref rhs } => {
                let (lhs_expression_text, rhs_expression_text, ty) = match (self.check_type(rhs), self.check_type(lhs)) {
                    (Type::Integer, Type::Integer) => (self.build_expression(lhs, Some(Type::Integer))?, self.build_expression(rhs, Some(Type::Integer))?, Type::Integer),
                    (Type::Fraction, Type::Integer) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                    (Type::Integer, Type::Fraction) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                    (Type::Fraction, Type::Fraction) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                };

                match ty {
                    Type::Integer => format!("{} > {}", lhs_expression_text, rhs_expression_text),
                    Type::Fraction => format!("frac_gt({},{})", lhs_expression_text, rhs_expression_text),
                }
            }
            Cond::Equal { ref lhs, ref rhs } => {
                let (lhs_expression_text, rhs_expression_text, ty) = match (self.check_type(rhs), self.check_type(lhs)) {
                    (Type::Integer, Type::Integer) => (self.build_expression(lhs, Some(Type::Integer))?, self.build_expression(rhs, Some(Type::Integer))?, Type::Integer),
                    (Type::Fraction, Type::Integer) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                    (Type::Integer, Type::Fraction) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                    (Type::Fraction, Type::Fraction) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                };

                match ty {
                    Type::Integer => format!("{} == {}", lhs_expression_text, rhs_expression_text),
                    Type::Fraction => format!("frac_eq({},{})", lhs_expression_text, rhs_expression_text),
                }
            }
            Cond::Less { ref lhs, ref rhs } => {
                let (lhs_expression_text, rhs_expression_text, ty) = match (self.check_type(rhs), self.check_type(lhs)) {
                    (Type::Integer, Type::Integer) => (self.build_expression(lhs, Some(Type::Integer))?, self.build_expression(rhs, Some(Type::Integer))?, Type::Integer),
                    (Type::Fraction, Type::Integer) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                    (Type::Integer, Type::Fraction) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                    (Type::Fraction, Type::Fraction) => (self.build_expression(lhs, Some(Type::Fraction))?, self.build_expression(rhs, Some(Type::Fraction))?, Type::Fraction),
                };

                match ty {
                    Type::Integer => format!("{} < {}", lhs_expression_text, rhs_expression_text),
                    Type::Fraction => format!("frac_lt({},{})", lhs_expression_text, rhs_expression_text),
                }
            }
        })
    }
}

fn template_program(program: &String) -> String {
    let mut prog = String::from(r#"#include <stdio.h>
#include <stdlib.h>

typedef struct {
	int num, den;
} frac;

frac frac_add(frac a, frac b) {
	frac c;
	c.num = a.num * b.den + b.num * a.den;
	c.den = a.den * b.den;
	return c;
}

frac frac_sub(frac a, frac b) {
	frac c;
	c.num = a.num * b.den - b.num * a.den;
	c.den = a.den * b.den;
	return c;
}

frac frac_mul(frac a, frac b) {
	frac c;
	c.num = a.num * b.num;
	c.den = a.den * b.den;
	return c;
}

int frac_gt(frac a, frac b) {
	return (a.num * b.den) > (b.num * a.den); 
}

int frac_eq(frac a, frac b) {
	return (a.num * b.den) == (b.num * a.den); 
}

int frac_lt(frac a, frac b) {
	return (a.num * b.den) < (b.num * a.den); 
}

frac max_f(frac a, frac b) {
    if (frac_gt(a, b)) return a;
    else return b;
}

frac new_frac(int num, int den) {
	frac f;
	f.num = num;
	f.den = den;
	return f;
}

void print_fraction(frac f) {
	printf("%d/%d", f.num, f.den);
}

int max(int a, int b) {
    return a > b ? a : b;
}

int main() {
"#);
  prog += program;
  prog += "\treturn 0;\n}";

  prog
}
