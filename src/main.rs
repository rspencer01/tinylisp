#![warn(clippy::unwrap_used, clippy::panic)]
use ariadne::{Color, Label, Report, ReportBuilder, ReportKind, Source};
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

type ErrReport = ReportBuilder<std::ops::Range<usize>>;

mod token;
use token::{tokenise, Token};
mod builtins;
use builtins::*;

#[derive(Clone, PartialEq)]
pub enum Expression {
    Number(f32),
    Symbol(Token),
    Builtin(
        &'static str,
        fn(Expression, Environment) -> Result<Expression, ErrReport>,
    ),
    /// The empty list
    Nil,
    /// The successor list
    Cons(Box<Expression>, Box<Expression>),
    /// A closure contains a (cons) list of parameter names, an expression to evaluate and the
    /// environment in which to do it.
    Closure(Box<Expression>, Box<Expression>, Environment),
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
            Expression::Closure(a, v, e) => format!(
                "\x1B[1;33mλ\x1B[0m {} \x1B[1;33m=>\x1B[0m {} \x1B[1;33mwith\x1B[0m {}",
                a, v, e
            ),
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

    fn as_number(&self) -> Option<f32> {
        match *self {
            Expression::Number(f) => Some(f),
            _ => None
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print())
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
                            Expression::Cons(Box::new(head), Box::new(tail)),
                            //TODO(robert) ensure there is a close brace here
                            head_len + tail_len + 2,
                        ))
                    } else {
                        let (tail, tail_len) = make_list_expression(tokens, first_token)?;
                        Ok((
                            Expression::Cons(Box::new(head), Box::new(tail)),
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

#[derive(PartialEq, Clone)]
pub struct Environment {
    variables: Vec<(String, Expression)>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: vec![
                ("ZERO".to_string(), Expression::Number(0.)),
                ("#t".to_string(), BUILTIN_TRUE),
                ("λ".to_string(), BUILTIN_LAMBDA),
                ("+".to_string(), BUILTIN_ADD),
                ("*".to_string(), BUILTIN_MUL),
                ("neg".to_string(), BUILTIN_NEG),
                ("inv".to_string(), BUILTIN_INV),
                ("<".to_string(), BUILTIN_LT),
                ("not".to_string(), BUILTIN_NOT),
                ("if".to_string(), BUILTIN_IF),
                ("'".to_string(), BUILTIN_QUOTE),
                ("eval".to_string(), BUILTIN_EVAL),
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
    fn bind(&mut self, symbol: Token, value: Expression) {
        let symbol_name: String = symbol.chars().collect();
        self.variables.push((symbol_name, value));
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{..")?;
        for (name, value) in self.variables.iter().skip(12) {
            write!(f, ", {} => {}", name, value)?;
        }
        write!(f, "}}")
    }
}

fn eval(expression: &Expression, environment: Environment) -> Result<Expression, ErrReport> {
    trace!("Eval {} in {}", expression, environment);
    match expression {
        Expression::Symbol(symbol) => match environment.associate(*symbol) {
            Some(value) => Ok(value.clone()),
            None => Err(Report::build(ReportKind::Error, (), 0)
                .with_message("Symbol not found in environment")),
        },
        Expression::Cons(f, v) => apply(eval(f, environment.clone())?, *v.clone(), environment),
        expression => Ok(expression.clone()),
    }
}

fn eval_list(expression: &Expression, environment: Environment) -> Result<Expression, ErrReport> {
    trace!("Eval list {}", expression);
    match expression {
        Expression::Cons(h, t) => Ok(Expression::Cons(
            Box::new(eval(h, environment.clone())?),
            Box::new(eval_list(t, environment)?),
        )),
        e => eval(e, environment),
    }
}

fn apply(
    function: Expression,
    arguments: Expression,
    environment: Environment,
) -> Result<Expression, ErrReport> {
    trace!("Apply {} to {}", function, arguments);
    match function {
        Expression::Builtin(_, function) => function(arguments, environment),
        Expression::Closure(parameters, body, closure_environment) => reduce(
            (*parameters, *body, closure_environment),
            arguments,
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
    (mut closure_params, closure_body, closure_env): (Expression, Expression, Environment),
    arguments: Expression,
    environment: Environment,
) -> Result<Expression, ErrReport> {
    let mut arguments = eval_list(&arguments, environment)?;
    let mut new_env = closure_env;
    let mut arg_count = 0;
    loop {
        match (closure_params, arguments) {
            (Expression::Nil, _) => break,
            (Expression::Symbol(symbol), value) => {
                new_env.bind(symbol, value);
                break;
            }
            (Expression::Cons(a, b), Expression::Cons(value, d)) => {
                match *a {
                    Expression::Symbol(symbol) => {
                        new_env.bind(symbol, *value);
                    }
                    _ => panic!("Bindings must be symbols"),
                }
                arg_count += 1;
                closure_params = *b;
                arguments = *d;
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
    eval(&closure_body, new_env)
}

fn main() -> Result<(), std::io::Error> {
    pretty_env_logger::init();

    //TODO(robert) this is a good error message to get right
    //                                            v
    //let _source = "((λ (f v) . (f (f v))) (λ (x y). (* x x) ) 3)";
    let source = "(
    'a
    )";

    let tokens = tokenise(source);

    debug!("Tokens: {}", Token::list_to_string(&tokens));

    let environment = Environment::new();

    match make_root_expression(&tokens) {
        Ok(exp) => {
            debug!("Expression: {}", exp);
            match eval(&exp, environment) {
                Ok(value) => println!("{}", value),
                Err(report) => {
                    report.finish().print(Source::from(source))?;
                }
            }
        }
        Err(report) => {
            report.finish().print(Source::from(source))?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    fn expr_test(source: &'static str, expression: Expression) {
        let environment = Environment::new();
        match make_expression(&tokenise(source)) {
            Ok((expr, _)) => {
                let ans = eval(&expr, environment).ok().unwrap();
                assert!(ans == expression, "{} != {}", ans, expression);
            }
            Err(_) => {
                panic!("Could not make expression");
            }
        }
    }

    #[test]
    fn add() {
        expr_test("(+ 1 2)", Expression::Number(3.));
        expr_test("( + 1)", Expression::Number(1.));
        expr_test("( + 1 2 3 )", Expression::Number(6.));

        expr_test("( + (+ 1 2) 4 )", Expression::Number(7.));
        expr_test("( + 1 (+ 2 5) )", Expression::Number(8.));
        println!("foo");
        expr_test("( + 1 (+ 2 5) (+ 3 4) )", Expression::Number(15.));
    }

    #[test]
    fn mul() {
        expr_test(
            "(* (* 3.1415 4) ( * 10 (* 10 10)))",
            Expression::Number(12566.),
        );
    }

    #[test]
    fn lambdas() {
        expr_test("((λ (x y) . (+ x y)) 3 9)", Expression::Number(12.));
    }

    #[test]
    fn scopes() {
        expr_test(
            "(((λ (x) . (λ (y) . (+ x y))) 3) 4)",
            Expression::Number(7.),
        );
        expr_test(
            "((λ (f v) . (f (f v))) (λ (x). (* x x) ) 3)",
            Expression::Number(81.),
        );
        expr_test(
            "(
                (λ (sub div x y) . (div (sub y x) x))
                    (λ (x y) . (+ x (neg . y)))
                    (λ (x y) . (* x (inv . y)))
                    32
                    42
                )",
            Expression::Number(0.3125),
        );
    }
}
