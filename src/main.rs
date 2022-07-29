#![feature(result_flattening)]
#![warn(clippy::unwrap_used, clippy::panic)]
use ariadne::{sources, Color, Label, Report, ReportBuilder, ReportKind};
#[macro_use]
mod logging;

use std::rc::Rc;

type ErrReport = ReportBuilder<(&'static str, std::ops::Range<usize>)>;

mod token;
use token::{tokenise, Token};
mod builtins;
use builtins::*;
mod ratio;
use ratio::{ONE, ZERO};
mod expression;
use expression::{expression_iter, Expression};

const SOURCES: [(&str, &str); 3] = [
    ("stdlib", include_str!("../standard_library/slib.tinylisp")),
    ("input", include_str!("../input.tinylisp")),
    ("evaluation", ""),
];

struct Interpreter {
    atoms: Vec<String>,
    global: Environment,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            atoms: vec!["#t".to_owned()],
            global: Environment::new(),
        }
    }
    fn find_atom_by_name(&self, name: &str) -> Option<usize> {
        self.atoms.iter().position(|x| x == name)
    }

    fn find_or_insert_atom_by_name(&mut self, name: &str) -> usize {
        self.find_atom_by_name(name).unwrap_or_else(|| {
            self.atoms.push(name.to_owned());
            self.atoms.len() - 1
        })
    }

    fn execute(&mut self, source: &'static str, source_id: &'static str) -> Result<(), ErrReport> {
        let tokens = tokenise(source, source_id);

        let expression = make_root_expression(&tokens)?;

        if !matches!(expression, Expression::Cons(_, _) | Expression::Nil) {
            return Err(Report::build(ReportKind::Error, source_id, 0)
                .with_label(
                    Label::new((source_id, 0..source.chars().count()))
                        .with_message("This is not a list")
                        .with_color(Color::Red),
                )
                .with_message("Program must be list of statments")
                .with_help("Ensure that the entire program is one list.")
                .with_help("If it consists of a single expression, still wrap it in a list."));
        }

        for statement in expression_iter(Rc::new(expression)) {
            trace!("Statement {}", statement);

            let ans = eval(&statement, &self.global)?;
            match ans {
                Expression::Define(token, value) => {
                    println!("Bound {} as {}", token, value);
                    self.global.bind(token, value);
                }
                x => println!("{}", x),
            }
        }
        Ok(())
    }
}

// TODO(robert) I don't like having split this up - I tried to do an inline loop thing. Give it
// another go.
fn make_list_expression(
    tokens: &[Token],
    first_token: &Token,
) -> Result<(Expression, usize), ErrReport> {
    match tokens.first() {
        Some(token) => {
            if token.chars().next() == Some(')') {
                return Ok((Expression::Nil, 1));
            }
            let (head, head_len) = make_expression(tokens)?;
            let tokens = &tokens[head_len..];
            match tokens.first() {
                Some(token) => {
                    if token.chars().next() == Some('.') {
                        let (tail, tail_len) = make_expression(&tokens[1..])?;
                        Ok((
                            Expression::Cons(Rc::new(head), Rc::new(tail)),
                            //TODO(robert) ensure there is a close brace here
                            head_len + tail_len + 2,
                        ))
                    } else {
                        let (tail, tail_len) = make_list_expression(tokens, first_token)?;
                        Ok((
                            Expression::Cons(Rc::new(head), Rc::new(tail)),
                            head_len + tail_len,
                        ))
                    }
                }
                None => Err(Report::build(ReportKind::Error, first_token.source_id(), 0)
                    .with_message("EOF while scanning list")
                    .with_label(
                        Label::new((
                            first_token.source_id(),
                            first_token.start()..first_token.source().chars().count(),
                        ))
                        .with_message("This list")
                        .with_color(Color::Blue),
                    )),
            }
        }
        None => Err(Report::build(ReportKind::Error, first_token.source_id(), 0)
            .with_message("EOF while scanning list")
            .with_label(
                Label::new((
                    first_token.source_id(),
                    first_token.start()..first_token.source().chars().count(),
                ))
                .with_message("This list")
                .with_color(Color::Blue),
            )),
    }
}

fn make_expression(tokens: &[Token]) -> Result<(Expression, usize), ErrReport> {
    match tokens.split_first() {
        Some((token, rest)) => {
            if token.chars().next() == Some(')') {
                panic!("Unexpected close brace");
                //                return Err(Report::build(ReportKind::Error, (), 0)
                //                    .with_message("Unexpected close brace")
                //                    .with_label(
                //                        Label::new(token.position.0..token.position.1)
                //                            .with_message("This close brace"),
                //                    ));
            }
            if token.chars().next() == Some('(') {
                let (list, list_len) = make_list_expression(rest, token)?;
                Ok((list, list_len + 1))
            } else {
                let string_value = token.chars().collect::<String>();
                if let Ok(num) = string_value.parse() {
                    Ok((Expression::Number(num), 1))
                } else {
                    Ok((Expression::Symbol(*token), 1))
                }
            }
        }
        None => {
            Err(Report::build(ReportKind::Error, "evaluation", 0).with_message("Unexpected EOF"))
        }
    }
}

fn make_root_expression(tokens: &[Token]) -> Result<Expression, ErrReport> {
    let (expression, length) = make_expression(tokens)?;
    println!("{} {}", expression, length);
    if length == tokens.len() {
        Ok(expression)
    } else {
        Err(Report::build(ReportKind::Error, tokens[0].source_id(), 0)
            .with_message("Extra tokens after program")
            .with_help(
                "The program should consist of a single list to be evaluated. \
                Remove the extra tokens after this list",
            )
            .with_label(
                Label::new((
                    tokens[0].source_id(),
                    tokens[0].start()..tokens[length - 1].end(),
                ))
                .with_message("This is the main program")
                .with_color(Color::Green),
            )
            .with_label(
                Label::new((
                    tokens[0].source_id(),
                    tokens[length].start()..tokens.last().unwrap().end(),
                ))
                .with_message("This is extra")
                .with_color(Color::Yellow),
            ))
    }
}

#[derive(Clone)]
pub struct Environment {
    variables: Vec<(String, Rc<Expression>)>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: vec![
                ("#t".to_string(), Rc::new(BUILTIN_TRUE)),
                ("λ".to_string(), Rc::new(BUILTIN_LAMBDA)),
                ("+".to_string(), Rc::new(BUILTIN_ADD)),
                ("*".to_string(), Rc::new(BUILTIN_MUL)),
                ("neg".to_string(), Rc::new(BUILTIN_NEG)),
                ("inv".to_string(), Rc::new(BUILTIN_INV)),
                ("<".to_string(), Rc::new(BUILTIN_LT)),
                ("not".to_string(), Rc::new(BUILTIN_NOT)),
                ("if".to_string(), Rc::new(BUILTIN_IF)),
                ("'".to_string(), Rc::new(BUILTIN_QUOTE)),
                ("eval".to_string(), Rc::new(BUILTIN_EVAL)),
                ("define".to_string(), Rc::new(BUILTIN_DEFINE)),
                ("list".to_string(), Rc::new(BUILTIN_LIST)),
                ("#env".to_string(), Rc::new(BUILTIN_PRINT_ENVIRONEMNT)),
            ],
        }
    }
    fn associate(&self, symbol: Token) -> Option<&Expression> {
        let symbol_name: String = symbol.chars().collect();
        for (name, value) in self.variables.iter().rev() {
            if name == &symbol_name {
                return Some(value);
            }
        }
        None
    }
    fn bind(&mut self, symbol: Token, value: Rc<Expression>) {
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

fn eval(expression: &Expression, environment: &Environment) -> Result<Expression, ErrReport> {
    match expression {
        Expression::Symbol(symbol) => {
            trace!("Eval {} in {}", expression, environment);
            match environment.associate(*symbol) {
                Some(value) => Ok(value.clone()),
                None => Err(Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message("Symbol not found in environment")),
            }
        }
        Expression::Cons(f, v) => apply(eval(f, environment)?, v.as_ref(), environment),
        expression => Ok(expression.clone()),
    }
}

fn eval_list(expression: &Expression, environment: &Environment) -> Result<Expression, ErrReport> {
    trace!("Eval list {}", expression);
    match expression {
        Expression::Cons(h, t) => Ok(Expression::Cons(
            Rc::new(eval(h, environment)?),
            Rc::new(eval_list(t, environment)?),
        )),
        e => eval(e, environment),
    }
}

fn apply(
    function: Expression,
    arguments: &Expression,
    environment: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Apply {} to {}", function, arguments);
    match function {
        Expression::Builtin(_, function) => function(arguments, environment),
        Expression::Closure(parameters, body, closure_environment) => reduce(
            (parameters.as_ref(), body.as_ref(), closure_environment),
            &arguments,
            environment,
        ),
        _ => Err(
            Report::build(ReportKind::Error, "evaluation", 0).with_message(format!(
                "Cannot apply {} as a function or closure",
                function
            )),
        ),
    }
}

fn reduce(
    (mut closure_params, closure_body, closure_env): (&Expression, &Expression, Environment),
    arguments: &Expression,
    environment: &Environment,
) -> Result<Expression, ErrReport> {
    let mut arguments = Rc::new(eval_list(arguments, environment)?);
    let mut new_env = closure_env;
    let mut arg_count = 0;
    loop {
        match (closure_params, arguments.as_ref()) {
            (Expression::Nil, _) => break,
            (Expression::Symbol(symbol), value) => {
                new_env.bind(*symbol, Rc::new(value.clone()));
                break;
            }
            (Expression::Cons(a, b), Expression::Cons(value, d)) => {
                match **a {
                    Expression::Symbol(symbol) => {
                        new_env.bind(symbol, value.clone());
                    }
                    _ => panic!("Bindings must be symbols"),
                }
                arg_count += 1;
                closure_params = b.as_ref();
                arguments = d.clone();
            }
            (Expression::Cons(_, _), _) => {
                return Err(Report::build(ReportKind::Error, "evalutation", 0)
                    .with_message("Not enough parameters to closure")
                    .with_note(format!("Closure wanted more than {} parameters", arg_count)))
            }
            _ => {
                return Err(Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message("Unknown error in bindings"))
            }
        }
    }
    eval(&closure_body, &new_env)
}

fn run() -> Result<(), ErrReport> {
    let mut interpreter = Interpreter::new();
    interpreter.execute(SOURCES[0].1, SOURCES[0].0)?;
    interpreter.execute(SOURCES[1].1, SOURCES[1].0)
}

fn main() -> Result<(), std::io::Error> {
    match run() {
        Ok(_) => Ok(()),
        Err(report) => report.finish().print(sources(SOURCES)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ratio::Ratio;

    fn expr_test_num<T: Into<Ratio>>(source: &'static str, val: T) {
        let environment = Environment::new();
        match make_expression(&tokenise(source, "test")) {
            Ok((expr, _)) => {
                let ans = eval(&expr, &environment).ok().unwrap();
                match ans {
                    Expression::Number(v) if v == val.into() => {}
                    _ => panic!(),
                }
            }
            Err(_) => {
                panic!("Could not make expression");
            }
        }
    }

    #[test]
    fn add() {
        expr_test_num("(+ 1 2)", 3);
        expr_test_num("( + 1)", 1);
        expr_test_num("( + 1 2 3 )", 6);

        expr_test_num("( + (+ 1 2) 4 )", 7);
        expr_test_num("( + 1 (+ 2 5) )", 8);
        println!("foo");
        expr_test_num("( + 1 (+ 2 5) (+ 3 4) )", 15);
    }

    #[test]
    fn mul() {
        expr_test_num("(* (* 3.1415 4) ( * 10 (* 10 10)))", 12566);
    }

    #[test]
    fn lambdas() {
        expr_test_num("((λ (x y) . (+ x y)) 3 9)", 12);
    }

    #[test]
    fn scopes() {
        expr_test_num("(((λ (x) . (λ (y) . (+ x y))) 3) 4)", 7);
        expr_test_num("((λ (f v) . (f (f v))) (λ (x). (* x x) ) 3)", 81);
        expr_test_num(
            "( * 10000
                (λ (sub div x y) . (div (sub y x) x))
                    (λ (x y) . (+ x (neg . y)))
                    (λ (x y) . (* x (inv . y)))
                    32
                    42
                )",
            3125,
        );
    }
}
