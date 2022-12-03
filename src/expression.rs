use std::rc::Rc;

use crate::*;

#[derive(Clone)]
pub enum Expression {
    /// All numbers are fractions
    Number(Number),
    // The below are the necessary "meta-types" to have a lisp
    /// Also called "atoms"
    Symbol(Token),
    Builtin(
        &'static str,
        fn(&Expression, &Environment, &Environment) -> Result<Expression, ErrReport>,
    ),
    /// The empty list
    Nil,
    /// The successor list
    Cons(Rc<Expression>, Rc<Expression>),
    /// A closure contains a (cons) list of parameter names, an expression to evaluate and the
    /// environment in which to do it.
    Closure(Rc<Expression>, Rc<Expression>, Rc<Environment>),
    Macro(Rc<Expression>, Rc<Expression>),
    Define(Token, Rc<Expression>),
}

impl Expression {
    fn pretty_print(&self) -> String {
        if std::env::var("TINYLISP_NO_COLOUR")
            .map(|s| !s.is_empty())
            .unwrap_or(false)
        {
            match self {
                Expression::Number(v) => v.to_string(),
                Expression::Symbol(t) => t.chars().collect(),
                Expression::Builtin(name, _) => name.to_string(),
                Expression::Cons(car, cdr) => {
                    format!("( {} )", Expression::cons_print(car, cdr))
                }
                Expression::Nil => "⊥".to_string(),
                Expression::Closure(a, v, _) => {
                    format!("λ {} → {}", a, v)
                }
                Expression::Macro(a, v) => {
                    format!("macro {} → {}", a, v)
                }
                Expression::Define(t, v) => {
                    format!("def {} as {}", t, v)
                }
            }
        } else {
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
                Expression::Macro(a, v) => {
                    format!("\x1B[1;33mmacro\x1B[0m {} \x1B[1;33m→\x1B[0m {}", a, v)
                }
                Expression::Define(t, v) => {
                    format!("\x1B[1;34mdef\x1B[0m {} \x1B[1;34mas\x1B[0m {}", t, v)
                }
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

    pub fn as_number(&self) -> Option<Number> {
        match *self {
            Expression::Number(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_token(&self) -> Option<Token> {
        match self {
            Expression::Symbol(t) => Some(t.clone()),
            _ => None,
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Expression::Nil)
    }

    /// Match two expression lists
    ///
    /// Returns the vector of matched elements, and the remainder of a
    pub fn mtch(
        a: Rc<Expression>,
        b: Rc<Expression>,
    ) -> (
        Vec<(Rc<Expression>, Rc<Expression>)>,
        Option<Rc<Expression>>,
    ) {
        match (a.as_ref(), b.as_ref()) {
            (Expression::Nil, _) => (vec![], None),
            (Expression::Cons(a_head, a_tail), Expression::Cons(b_head, b_tail)) => {
                let mut ans = Expression::mtch(a_tail.clone(), b_tail.clone());
                ans.0.push((a_head.clone(), b_head.clone()));
                ans
            }
            (Expression::Cons(_, _), bval) if bval.is_nil() => {
                (vec![], Some(a))
            }
            (Expression::Cons(a_head, a_tail), _) => {
                (vec![(a_head.clone(), b.clone())], Some(a_tail.clone()))
            }
            (_, _) => (vec![(a, b)], None),
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
        None => Ok(Expression::Nil),
        Some(e) => Ok(Expression::Cons(
            Rc::new(e?),
            Rc::new(cons_from_iter_of_result(iter)?),
        )),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn expression_cons_iter() {
        let n = || Rc::new(Expression::Nil);
        let num = |i: i64| Rc::new(Expression::Number(Number::from(i)));
        let c = |a, b| Rc::new(Expression::Cons(a, b));
        let iter = expression_iter(c(n(), n()))
            .map(|x| (*x).clone())
            .collect::<Vec<_>>();
        assert_eq!(iter.len(), 1);
        assert!(matches!(iter[0], Expression::Nil));
        let iter = expression_iter(c(num(1), c(n(), n())))
            .map(|x| (*x).clone())
            .collect::<Vec<_>>();
        assert_eq!(iter.len(), 2);
        assert!(matches!(iter[0], Expression::Number(_)));
        assert!(matches!(iter[1], Expression::Nil));
    }
}
