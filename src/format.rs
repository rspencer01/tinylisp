use crate::{expression::expression_iter, Expression};
use std::{collections::VecDeque, rc::Rc};

pub fn pretty_format(expression: &Expression) -> String {
    pretty_print(expression, 0, 0)
}

fn pretty_print(expression: &Expression, first_line_offset: usize, indent: usize) -> String {
    match expression {
        Expression::Number(v) => v.to_string(),
        Expression::Symbol(t) => t.chars().collect(),
        Expression::Builtin(name, _) => name.to_string(),
        Expression::Nil => "()".to_string(),
        Expression::Cons(_, _) => {
            let oneline = pretty_print_oneline(expression);
            if oneline.chars().count() < 20 {
                return oneline;
            }
            if is_two_level_list(expression) && first_line_offset + oneline.chars().count() < 80 {
                return oneline;
            }

            let subindent = indent + 2;

            let mut list: VecDeque<String> = expression_iter(Rc::new(expression.clone()))
                .map(|x| pretty_print(x.as_ref(), subindent, subindent))
                .collect();
            if null_terminated(expression) {
                list.pop_back();
            } else {
                list.insert(list.len() - 1, ".".to_string())
            }
            if list.len() >= 3 && ["define", "λ", "defun"].contains(&list[0].as_str()) {
                let first = list.pop_front().unwrap();
                let second = list.pop_front().unwrap();
                let third = list.pop_front().unwrap();
                list.push_front(String::from("(") + &first + " " + &second + " " + &third);
            } else {
                list.push_front(String::from("("));
            }
            list.iter()
                .intersperse(&format!("\n{:subindent$}", ""))
                .fold(String::from(""), |a, b| a + b)
                + &format!("\n{:>indent$})", "")
        }
        _ => {
            unimplemented!("Expression contains unformattable objects")
        }
    }
}

fn pretty_print_oneline(expression: &Expression) -> String {
    match expression {
        Expression::Number(v) => v.to_string(),
        Expression::Symbol(t) => t.chars().collect(),
        Expression::Builtin(name, _) => name.to_string(),
        Expression::Cons(_, _) => {
            let mut list: Vec<String> = expression_iter(Rc::new(expression.clone()))
                .map(|x| pretty_print_oneline(x.as_ref()))
                .collect();
            if null_terminated(expression) {
                list.pop();
            } else{
                list.insert(list.len() - 1, ".".to_string())
            }
            list.iter()
                .intersperse(&" ".to_string())
                .fold(String::from("("), |a, b| a + b)
                + ")"
        }
        Expression::Nil => "()".to_string(),
        _ => {
            unimplemented!("Expression contains unformattable objects")
        }
    }
}

fn is_two_level_list(expression: &Expression) -> bool {
    match expression {
        Expression::Cons(_, _) => {
            expression_iter(Rc::new(expression.clone())).all(|x| is_simple_list(x.as_ref()))
        }
        _ => true,
    }
}

fn is_simple_list(expression: &Expression) -> bool {
    match expression {
        Expression::Cons(_, _) => expression_iter(Rc::new(expression.clone()))
            .all(|x| !matches!(x.as_ref(), Expression::Cons(_, _))),
        _ => true,
    }
}

fn null_terminated(expression: &Expression) -> bool {
    match expression {
        Expression::Nil => true,
        Expression::Cons(_, e) => null_terminated(e.as_ref()),
        _ => false,
    }
}
