#![feature(result_flattening)]
#![warn(clippy::unwrap_used, clippy::panic)]
use ariadne::{Color, Label, Report, ReportBuilder, ReportKind, Source};
#[macro_use]
extern crate log;
extern crate pretty_env_logger;
use std::rc::Rc;

type ErrReport = ReportBuilder<std::ops::Range<usize>>;

mod token;
use token::{tokenise, Token};
mod builtins;
use builtins::*;

#[derive(Clone)]
pub enum Expression {
    /// All numbers are integers
    Number(i32),
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
            Expression::Closure(a, v, _) => format!(
                "\x1B[1;33mλ\x1B[0m {} \x1B[1;33m→\x1B[0m {}",
                a, v
            ),
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

    fn as_number(&self) -> Option<i32> {
        match *self {
            Expression::Number(f) => Some(f),
            _ => None,
        }
    }
}

fn expression_iter(expr : Rc<Expression>) -> ExpressionConsIterator {
    ExpressionConsIterator {
        source: Some(expr)
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print())
    }
}

pub struct ExpressionConsIterator {
    source : Option<Rc<Expression>>
}

impl Iterator for ExpressionConsIterator {
    type Item = Rc<Expression>;
    fn next(&mut self) -> Option<Self::Item> {
        match &self.source.clone() {
            Some(expr) => {
                match expr.as_ref() {
                    Expression::Cons(head, tail) => {
                        self.source = Some(tail.clone());
                        Some(head.clone())
                    }
                    Expression::Nil => {
                        self.source = None;
                        None
                    }
                    _ => {
                        self.source.take()
                    }
                }
            }
            None => None,
        }
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
                None => Err(Report::build(ReportKind::Error, (), 0)
                    .with_message("EOF while scanning list")
                    .with_label(
                        Label::new(first_token.start()..first_token.source().chars().count())
                            .with_message("This list")
                            .with_color(Color::Blue),
                    )),
            }
        }
        None => Err(Report::build(ReportKind::Error, (), 0)
            .with_message("EOF while scanning list")
            .with_label(
                Label::new(first_token.start()..first_token.source().chars().count())
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
        None => Err(Report::build(ReportKind::Error, (), 0).with_message("Unexpected EOF")),
    }
}

fn make_root_expression(tokens: &[Token]) -> Result<Expression, ErrReport> {
    let (expression, length) = make_expression(tokens)?;
    if length == tokens.len() {
        Ok(expression)
    } else {
        Err(Report::build(ReportKind::Error, (), 0)
            .with_message("Extra tokens after program")
            .with_help(
                "The program should consist of a single list to be evaluated. \
                Remove the extra tokens after this list",
            )
            .with_label(
                Label::new(tokens[0].start()..tokens[length - 1].end())
                    .with_message("This is the main program")
                    .with_color(Color::Green),
            )
            .with_label(
                Label::new(tokens[length].start()..tokens.last().unwrap().end())
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
                ("ZERO".to_string(), Rc::new(Expression::Number(0))),
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
        for (name, value) in self.variables.iter().skip(13) {
            write!(f, ", {} => {}", name, value)?;
        }
        write!(f, "}}")
    }
}

fn eval(expression: &Expression, environment: &Environment) -> Result<Expression, ErrReport> {
    trace!("Eval {} in {}", expression, environment);
    match expression {
        Expression::Symbol(symbol) => match environment.associate(*symbol) {
            Some(value) => Ok(value.clone()),
            None => Err(Report::build(ReportKind::Error, (), 0)
                .with_message("Symbol not found in environment")),
        },
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
            Report::build(ReportKind::Error, (), 0).with_message(format!(
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
                return Err(Report::build(ReportKind::Error, (), 0)
                    .with_message("Not enough parameters to closure")
                    .with_note(format!("Closure wanted more than {} parameters", arg_count)))
            }
            _ => {
                return Err(Report::build(ReportKind::Error, (), 0)
                    .with_message("Unknown error in bindings"))
            }
        }
    }
    eval(&closure_body, &new_env)
}

fn split_into_statements(tokens: &[Token]) -> Vec<&[Token]> {
    let mut ans = Vec::new();
    let mut from = 0;
    let mut brackets = 0;
    let mut to = 0;
    for token in tokens {
        to += 1;
        if token.chars().collect::<String>() == "(" {
            brackets += 1;
        } else if token.chars().collect::<String>() == ")" {
            brackets -= 1;
            if brackets == 0 {
                ans.push(&tokens[from..to]);
                from = to;
            }
        }
    }
    if from < tokens.len() {
        ans.push(&tokens[from..]);
    }
    ans
}

fn execute(source: &'static str, environment: &mut Environment) -> Result<(), ErrReport> {
    let tokens = tokenise(source);
    let statement_tokens = split_into_statements(&tokens);

    for tokens in statement_tokens {
        trace!("Tokens {}", Token::list_to_string(tokens));
        let statement = make_root_expression(tokens)?;
        trace!("Statement {}", statement);

        let ans = eval(&statement, environment)?;
        match ans {
            Expression::Define(token, value) => {
                println!("Bound {} as {}", token, value);
                environment.bind(token, value);
            }
            x => println!("{}", x),
        }
    }
    Ok(())
}

fn run(source: &'static str) -> Result<(), ErrReport> {
    let mut environment = Environment::new();
    let standard_library = include_str!("../standard_library/slib.lisp");
    execute(standard_library, &mut environment)?;
    execute(source, &mut environment)
}

fn main() -> Result<(), std::io::Error> {
    pretty_env_logger::init();

    //TODO(robert) this is a good error message to get right
    //                                            v
    let source = "
         (* 3 5)
         ";

    match run(source) {
        Ok(_) => Ok(()),
        Err(report) => report.finish().print(Source::from(source)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn expr_test_num(source: &'static str, val:i32 ) {
        let environment = Environment::new();
        match make_expression(&tokenise(source)) {
            Ok((expr, _)) => {
                let ans = eval(&expr, &environment).ok().unwrap();
                match ans {
                    Expression::Number(v) if v == val => {}
                    _ => panic!()
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
        expr_test_num(
            "(* (* 3.1415 4) ( * 10 (* 10 10)))",
            12566,
        );
    }

    #[test]
    fn lambdas() {
        expr_test_num("((λ (x y) . (+ x y)) 3 9)", 12);
    }

    #[test]
    fn scopes() {
        expr_test_num(
            "(((λ (x) . (λ (y) . (+ x y))) 3) 4)",
            7,
        );
        expr_test_num(
            "((λ (f v) . (f (f v))) (λ (x). (* x x) ) 3)",
            81,
        );
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
