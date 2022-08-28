use crate::*;

fn _builtin_mul(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("Mul on {}", e);
    let e = Rc::new(eval_list(&e, env)?);
    trace!("Parameter for mul evaluated to {}", e);
    expression_iter(e)
        .map(|exp| {
            exp.as_number().ok_or_else(|| {
                Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message(format!("Cannot multiply non numeric type {}", exp))
            })
        })
        .try_fold(ONE, |accum, x| x.map(|y| accum * y))
        .map(|v| Expression::Number(v))
}

/// Add numeric types together
///
/// ```lisp
///  (+ 1 2 3) => 6
///  (+ 5) => 5
///  (+ . (1 2 3)) => 6
///  (+ . ()) => 0
/// ```
fn _builtin_add(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("Add on {}", e);
    let e = Rc::new(eval_list(&e, env)?);
    trace!("Parameter for add evaluated to {}", e);
    expression_iter(e)
        .map(|exp| {
            exp.as_number().ok_or_else(|| {
                Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message(format!("Cannot add non numeric type {}", exp))
            })
        })
        .try_fold(ZERO, |accum, x| x.map(|y| accum + y))
        .map(|v| Expression::Number(v))
}

fn _builtin_lambda(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("Lambda on {}", e);
    let mut param_body_iter = expression_iter(Rc::new(e.clone()));
    let param_body = (param_body_iter.next(), param_body_iter.next());
    match param_body_iter.next() {
        None => match param_body {
            (Some(parameters), Some(body)) => Ok(Expression::Closure(
                parameters.clone(),
                body.clone(),
                env.clone(),
            )),
            _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
                .with_message("Lambda must be called on array")
                .with_help("Constructing a closure must take an array.")
                .with_help("The first element of the array are the arguments.")
                .with_help("The remainder of the array is the expression to evaluate")),
        },
        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
            .with_message("Lambda must be called on an array of exactly two elements")
            .with_help("The first element must be parameters.")
            .with_help("The second element is the body.")
            .with_help("This must be a NIL terminated list")),
    }
}

fn _builtin_neg(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("Neg on {}", e);
    let e = eval_list(&e, env)?;
    trace!("Parameter for neg evaluated to {}", e);
    cons_from_iter_of_result(expression_iter(Rc::new(e)).map(|item| {
        item.as_number()
            .ok_or_else(|| {
                Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message(format!("Cannot take neg of non-numeric type {}", item))
            })
            .map(|x| Expression::Number(-x))
    }))
}

fn _builtin_inv(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("Inv on {}", e);
    let e = eval(&e, env)?;
    trace!("Parameter for inv evaluated to {}", e);
    e.as_number()
        .ok_or_else(|| {
            Report::build(ReportKind::Error, "evaluation", 0)
                .with_message(format!("Cannot take inverse of non-numeric type {}", e))
        })
        .map(|x| {
            if x.is_zero() {
                Err(Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message(format!("Cannot take inverse of 0")))
            } else {
                Ok(Expression::Number(x.inverse()))
            }
        })
        .flatten()
}

fn _builtin_lt(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("lt on {}", e);
    let e = eval_list(&e, env)?;
    trace!("Parameter for lt evaluated to {}", e);
    match e {
        Expression::Cons(a, b) if a.as_number().is_some() && b.as_number().is_some() => {
            let a = a.as_number().unwrap();
            let b = b.as_number().unwrap();
            if a < b {
                Ok(BUILTIN_TRUE)
            } else {
                Ok(Expression::Nil)
            }
        }
        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
            .with_message("Less than cannot act on anything but a pair of numbers")),
    }
}

fn _builtin_not(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("not on {}", e);
    let e = eval_list(&e, env)?;
    trace!("Parameter for not evaluated to {}", e);
    match e {
        Expression::Nil => Ok(BUILTIN_TRUE),
        _ => Ok(Expression::Nil),
    }
}

fn _builtin_if(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    trace!("If on {}", e);
    match e {
        Expression::Cons(pred, options) => match options.as_ref() {
            Expression::Cons(true_option, false_option) => {
                if matches!(pred.as_ref(), Expression::Nil) {
                    eval(true_option.as_ref(), env)
                } else {
                    eval(false_option.as_ref(), env)
                }
            }
            _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
                .with_message("If statement requires two options")),
        },
        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
            .with_message("If statement requires a predicate and a list for options")),
    }
}

fn _builtin_quote(e: &Expression, _env: &Environment) -> Result<Expression, ErrReport> {
    trace!("Quote on {}", e);
    Ok(e.clone())
}

fn _builtin_eval(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    eval(&e, env)
}

fn _builtin_define(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    match e {
        Expression::Cons(name, val) => match name.as_ref() {
            Expression::Symbol(token) => Ok(Expression::Define(
                *token,
                Rc::new(eval(val.as_ref(), env)?),
            )),
            _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
                .with_message("Definition requires a token as name")),
        },
        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
            .with_message("Definition requires a list")),
    }
}

fn _builtin_list(e: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    eval_list(e, env)
}

fn _builtin_print_env(_: &Expression, env: &Environment) -> Result<Expression, ErrReport> {
    println!("{}", env);
    Ok(Expression::Nil)
}

pub const BUILTIN_MUL: Expression = Expression::Builtin("×", _builtin_mul);
pub const BUILTIN_ADD: Expression = Expression::Builtin("+", _builtin_add);
pub const BUILTIN_NEG: Expression = Expression::Builtin("neg", _builtin_neg);
pub const BUILTIN_INV: Expression = Expression::Builtin("inv", _builtin_inv);
pub const BUILTIN_LAMBDA: Expression = Expression::Builtin("λ", _builtin_lambda);
pub const BUILTIN_LT: Expression = Expression::Builtin("<", _builtin_lt);
pub const BUILTIN_TRUE: Expression = Expression::Symbol(Token::new("#t", "builtin", 0, 2));
pub const BUILTIN_NOT: Expression = Expression::Builtin("not", _builtin_not);
pub const BUILTIN_IF: Expression = Expression::Builtin("if", _builtin_if);
pub const BUILTIN_QUOTE: Expression = Expression::Builtin("'", _builtin_quote);
pub const BUILTIN_EVAL: Expression = Expression::Builtin("eval", _builtin_eval);
pub const BUILTIN_DEFINE: Expression = Expression::Builtin("define", _builtin_define);
pub const BUILTIN_LIST: Expression = Expression::Builtin("define", _builtin_list);
pub const BUILTIN_PRINT_ENVIRONEMNT: Expression = Expression::Builtin("#env", _builtin_print_env);
