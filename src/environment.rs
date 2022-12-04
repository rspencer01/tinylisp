use std::rc::Rc;

use crate::expression::Expression;
use crate::builtins::*;
use crate::token::Token;

#[derive(Clone)]
pub struct Environment {
    variables: Vec<(String, Rc<Expression>)>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            variables: vec![]
        }
    }
    pub fn new_base() -> Self {
        Environment {
            variables: vec![
                ("λ".to_string(), Rc::new(BUILTIN_LAMBDA)),
                ("macro".to_string(), Rc::new(BUILTIN_MACRO)),
                ("'".to_string(), Rc::new(BUILTIN_QUOTE)),
                ("eval".to_string(), Rc::new(BUILTIN_EVAL)),
                ("define".to_string(), Rc::new(BUILTIN_DEFINE)),
                ("#env".to_string(), Rc::new(BUILTIN_PRINT_ENVIRONEMNT)),
                ("+".to_string(), Rc::new(BUILTIN_ADD)),
                ("×".to_string(), Rc::new(BUILTIN_MUL)),
                ("neg".to_string(), Rc::new(BUILTIN_NEG)),
                ("inv".to_string(), Rc::new(BUILTIN_INV)),
                ("<".to_string(), Rc::new(BUILTIN_LT)),
                ("floor".to_string(), Rc::new(BUILTIN_FLOOR)),
                ("cond".to_string(), Rc::new(BUILTIN_COND)),
            ],
        }
    }
    pub fn associate(&self, symbol: Token) -> Option<&Expression> {
        let symbol_name: String = symbol.chars().collect();
        for (name, value) in self.variables.iter().rev() {
            if name == &symbol_name {
                return Some(value);
            }
        }
        None
    }
    pub fn bind(&mut self, symbol: Token, value: Rc<Expression>) {
        let symbol_name: String = symbol.chars().collect();
        self.variables.push((symbol_name, value));
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{..")?;
        for (name, value) in self.variables.iter().skip(14) {
            write!(f, ", {} => {}", name, value)?;
        }
        write!(f, "}}")
    }
}

