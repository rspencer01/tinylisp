#![feature(result_flattening)]
#![feature(iter_intersperse)]
#![warn(clippy::unwrap_used, clippy::panic)]
use ariadne::{sources, Color, Label, Report, ReportBuilder, ReportKind};

use tracing::{field, instrument, trace, trace_span};
use tracing_forest::{traits::*, util::EnvFilter, ForestLayer, SpanFieldEventLayer};
use tracing_subscriber::Registry;

use std::env;
use std::fs::read_to_string;
use std::io::{stdin, Read};
use std::path::Path;
use std::rc::Rc;

type ErrReport = ReportBuilder<(&'static str, std::ops::Range<usize>)>;

mod token;
use token::{tokenise, Token};
mod builtins;
mod number;
mod ratio;
use number::Number;
use number::{ONE, ZERO};
mod expression;
use expression::{cons_from_iter_of_result, expression_iter, Expression};
mod environment;
use environment::Environment;
mod format;
use format::pretty_format;

struct Interpreter {
    global: Environment,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            global: Environment::new_base(),
        }
    }

    fn execute(&mut self, source: &str, source_id: &'static str) -> Result<(), ErrReport> {
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
            let span_guard =
                trace_span!("Root statement", %statement, result=field::Empty).entered();

            let ans = eval(&statement, &self.global, &self.global)?;
            span_guard.record("result", field::display(&ans));
            match ans {
                Expression::Define(token, value) => {
                    self.global.bind(token, value);
                }
                x => println!("{}", x),
            }
        }
        Ok(())
    }
}

fn eval(
    expression: &Expression,
    environment: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    match expression {
        Expression::Symbol(symbol) => match environment
            .associate(symbol.clone())
            .or_else(|| global.associate(symbol.clone()))
        {
            Some(value) => Ok(value.clone()),
            None => Err(Report::build(ReportKind::Error, "evaluation", 0)
                .with_message(format!("Symbol {} not found in environment", symbol))),
        },
        Expression::Cons(f, v) => apply(
            eval(f, environment, global)?,
            v.as_ref(),
            environment,
            global,
        ),
        expression => Ok(expression.clone()),
    }
}

fn eval_list(
    expression: &Expression,
    environment: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    match expression {
        Expression::Cons(h, t) => Ok(Expression::Cons(
            Rc::new(eval(h, environment, global)?),
            Rc::new(eval_list(t, environment, global)?),
        )),
        e => eval(e, environment, global),
    }
}

fn apply(
    function: Expression,
    arguments: &Expression,
    environment: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    let _span_guard = trace_span!("apply", %function, %arguments).entered();
    match function {
        Expression::Builtin(_, function) => function(arguments, environment, global),
        Expression::Closure(parameters, body, closure_environment) => reduce(
            (
                parameters.as_ref(),
                body.as_ref(),
                closure_environment.as_ref(),
            ),
            arguments,
            environment,
            global,
        ),
        Expression::Macro(parameters, body) => expand(
            (parameters.as_ref(), body.as_ref()),
            arguments,
            environment,
            global,
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
    (closure_params, closure_body, closure_env): (&Expression, &Expression, &Environment),
    arguments: &Expression,
    environment: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    let span_guard = trace_span!("reduce", %arguments).entered();
    let arguments = Rc::new(eval_list(arguments, environment, global)?);
    span_guard.record("arguments", field::display(&arguments));

    let mut new_env = closure_env.clone();

    let (pairings, remaining_parameters) =
        Expression::mtch(Rc::new(closure_params.clone()), arguments);

    let pairings = pairings
        .into_iter()
        .map(|(name, value)| name.as_token().map(|name| (name, value)))
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| {
            Report::build(ReportKind::Error, "evaluation", 0)
                .with_message("All bindings must be symbols")
        })?;

    for (name, value) in pairings {
        new_env.bind(name, value);
    }

    if let Some(remaining_parameters) = remaining_parameters {
        Ok(Expression::Closure(
            remaining_parameters,
            Rc::new(closure_body.clone()),
            Rc::new(new_env),
        ))
    } else {
        eval(closure_body, &new_env, global)
    }
}

fn expand(
    (mut params, body): (&Expression, &Expression),
    arguments: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Expanding...");
    let mut arguments = Rc::new(arguments.clone());
    let mut new_env = Environment::new();
    let mut arg_count = 0;
    loop {
        match (params, arguments.as_ref()) {
            (Expression::Nil, _) => break,
            (Expression::Symbol(symbol), value) => {
                new_env.bind(symbol.clone(), Rc::new(value.clone()));
                break;
            }
            (Expression::Cons(a, b), Expression::Cons(value, d)) => {
                match a.as_ref().clone() {
                    Expression::Symbol(symbol) => {
                        new_env.bind(symbol, value.clone());
                    }
                    _ => panic!("Bindings must be symbols"),
                }
                arg_count += 1;
                params = b.as_ref();
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
    trace!("Evaluating {} in {}", body, new_env);
    eval(&eval(body, &new_env, global)?, env, global)
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
                    if token.chars().next() == Some('⋮') {
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
                        Label::new((first_token.source_id(), first_token.start()..usize::MAX))
                            .with_message("This list")
                            .with_color(Color::Blue),
                    )),
            }
        }
        None => Err(Report::build(ReportKind::Error, first_token.source_id(), 0)
            .with_message("EOF while scanning list")
            .with_label(
                Label::new((first_token.source_id(), first_token.start()..usize::MAX))
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
                    Ok((Expression::Symbol(token.clone()), 1))
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

#[instrument]
fn run(sources: &[(&'static str, String)]) -> Result<(), ErrReport> {
    let mut interpreter = Interpreter::new();
    for source in sources {
        interpreter.execute(&source.1, source.0)?;
    }
    Ok(())
}

fn main() -> Result<(), std::io::Error> {
    Registry::default()
        .with(SpanFieldEventLayer::default())
        .with(EnvFilter::from_env("TINYLISP_LOG"))
        .with(ForestLayer::default())
        .init();
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 || (args.len() == 3 && args[1] != "--format") {
        return Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, "evaluation", 0)
            .with_message(USAGE)
            .finish()
            .print(sources([("evaluation", "")]));
    }
    let path = Path::new(&args[args.len() - 1]);
    let source = if path == Path::new("-") {
        let mut ans = String::new();
        stdin().read_to_string(&mut ans)?;
        ans
    } else {
        read_to_string(path)?
    };
    if args.len() == 2 {
        let our_sources = [
            (
                "stdlib",
                include_str!("../standard_library/slib.tinylisp").to_owned(),
            ),
            ("input", source),
            ("evaluation", "()".to_owned()),
        ];
        match run(&our_sources) {
            Ok(_) => Ok(()),
            Err(report) => report.finish().print(sources(our_sources)),
        }
    } else {
        let tokens = tokenise(&source, "input");

        match make_root_expression(&tokens) {
            Ok(expression) => {
                println!("{}", pretty_format(&expression));
                Ok(())
            }
            Err(report) => report.finish().print(sources([("input", source)])),
        }
    }
}

const USAGE: &str = "
USAGE: tinylisp <path-to-input>
       OR
       tinylisp --format <path-to-input>
";

#[cfg(test)]
mod test {
    use super::*;

    fn expr_test_num<T: Into<Number>>(source: &'static str, val: T) {
        //        let environment = Environment::new();
        //        match make_expression(&tokenise(source, "test")) {
        //            Ok((expr, _)) => {
        //                let ans = eval(&expr, &environment).ok().unwrap();
        //                match ans {
        //                    Expression::Number(v) if v == val.into() => {}
        //                    _ => panic!(),
        //                }
        //            }
        //            Err(_) => {
        //                panic!("Could not make expression");
        //            }
        //        }
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
