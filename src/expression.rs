use std::rc::Rc;
use crate::ratio::Ratio;
use crate::token::Token;
use crate::{Environment, ErrReport};

#[derive(Clone)]
pub enum Expression {
    /// All numbers are fractions
    Number(Ratio),
    // The below are the necessary "meta-types" to have a lisp
    /// Also called "atoms"
    Symbol(Token),
    Builtin(
        &'static str,
        fn(&Expression, &Environment) -> Result<Expression, ErrReport>,
    ),
    /// The empty list
    Nil,
    /// The successor list
    Cons(Rc<Expression>, Rc<Expression>),
    /// A closure contains a (cons) list of parameter names, an expression to evaluate and the
    /// environment in which to do it.
    Closure(Rc<Expression>, Rc<Expression>, Environment),
    Define(Token, Rc<Expression>),
}

impl Expression {
    fn pretty_print(&self) -> String {
        match self {
            Expression::Number(v) => v.to_string(),
            Expression::Symbol(t) => t.chars().collect(),
            Expression::Builtin(name, _) => format!("\x1B[1;32m{}\x1B[0m", name),
            Expression::Cons(car, cdr) => {
                format!("( {} )", Expression::cons_print(car, cdr))
            }
            Expression::Nil => "\x1B[1;90m⊥\x1B[0m".to_string(),
            //NOTE(robert) This is very cool but gets out of hand easily
            //Expression::Closure(a, v, e) => format!(
            //    "\x1B[1;33mλ\x1B[0m {} \x1B[1;33m→\x1B[0m {} \x1B[1;33mwith\x1B[0m {}",
            //    a, v, e
            //),
            Expression::Closure(a, v, _) => {
                format!("\x1B[1;33mλ\x1B[0m {} \x1B[1;33m→\x1B[0m {}", a, v)
            }
            Expression::Define(t, v) => {
                format!("\x1B[1;34mdef\x1B[0m {} \x1B[1;34mas\x1B[0m {}", t, v)
            }
        }
    }

    fn cons_print(car: &Expression, cdr: &Expression) -> String {
        let car_str = car.pretty_print();
        let cdr_str = match cdr {
            Expression::Cons(a, b) => Expression::cons_print(a, b),
            e => e.pretty_print(),
        };
        if matches!(cdr, Expression::Nil | Expression::Cons(_, _)) {
            format!("{} {}", car_str, cdr_str)
        } else {
            format!("{} \x1B[1;90m·\x1B[0m {}", car_str, cdr_str)
        }
    }

    pub fn as_number(&self) -> Option<Ratio> {
        match *self {
            Expression::Number(f) => Some(f),
            _ => None,
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print())
    }
}

pub struct ExpressionConsIterator {
    source: Option<Rc<Expression>>,
}

impl Iterator for ExpressionConsIterator {
    type Item = Rc<Expression>;
    fn next(&mut self) -> Option<Self::Item> {
        match &self.source.clone() {
            Some(expr) => match expr.as_ref() {
                Expression::Cons(head, tail) => {
                    self.source = Some(tail.clone());
                    Some(head.clone())
                }
                Expression::Nil => {
                    self.source = None;
                    None
                }
                _ => self.source.take(),
            },
            None => None,
        }
    }
}

pub fn expression_iter(expr: Rc<Expression>) -> ExpressionConsIterator {
    ExpressionConsIterator { source: Some(expr) }
}

pub fn cons_from_iter_of_result<T, E>(mut iter: T) -> Result<Expression, E>
where
    T: Iterator<Item = Result<Expression, E>>,
{
    match iter.next() {
        None => {
            Ok(Expression::Nil)
        },
        Some(e) => {
            Ok(Expression::Cons(Rc::new(e?), Rc::new(cons_from_iter_of_result(iter)?)))
        }
    }
}
