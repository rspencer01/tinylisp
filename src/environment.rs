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
                (
                    "#t".to_string(),
                    Rc::new(Expression::Symbol(Token::new("#t", "builtin", 0, 2))),
                ),
                ("λ".to_string(), Rc::new(BUILTIN_LAMBDA)),
                ("+".to_string(), Rc::new(BUILTIN_ADD)),
                ("*".to_string(), Rc::new(BUILTIN_MUL)),
                ("neg".to_string(), Rc::new(BUILTIN_NEG)),
                ("inv".to_string(), Rc::new(BUILTIN_INV)),
                ("<".to_string(), Rc::new(BUILTIN_LT)),
                ("cond".to_string(), Rc::new(BUILTIN_COND)),
                ("'".to_string(), Rc::new(BUILTIN_QUOTE)),
                ("eval".to_string(), Rc::new(BUILTIN_EVAL)),
                ("define".to_string(), Rc::new(BUILTIN_DEFINE)),
                ("macro".to_string(), Rc::new(BUILTIN_MACRO)),
                ("#env".to_string(), Rc::new(BUILTIN_PRINT_ENVIRONEMNT)),
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

