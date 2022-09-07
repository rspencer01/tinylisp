use crate::*;

fn _builtin_mul(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Mul on {}", e);
    let e = Rc::new(eval_list(&e, env, global)?);
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
fn _builtin_add(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Add on {}", e);
    let e = Rc::new(eval_list(&e, env, global)?);
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

fn _builtin_lambda(
    e: &Expression,
    env: &Environment,
    _global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Lambda on {}", e);
    let mut param_body_iter = expression_iter(Rc::new(e.clone()));
    let param_body = (param_body_iter.next(), param_body_iter.next());
    match param_body_iter.next() {
        None => match param_body {
            (Some(parameters), Some(body)) => Ok(Expression::Closure(
                parameters.clone(),
                body.clone(),
                Rc::new(env.clone()),
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

fn _builtin_macro(
    e: &Expression,
    _env: &Environment,
    _global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Macro on {}", e);
    let mut param_body_iter = expression_iter(Rc::new(e.clone()));
    let param_body = (param_body_iter.next(), param_body_iter.next());
    match param_body_iter.next() {
        None => match param_body {
            (Some(parameters), Some(body)) => Ok(Expression::Macro(
                parameters.clone(),
                body.clone(),
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

fn _builtin_neg(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Neg on {}", e);
    let e = eval_list(&e, env, global)?;
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

fn _builtin_inv(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Inv on {}", e);
    let e = eval(&e, env, global)?;
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

fn _builtin_lt(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("lt on {}", e);
    let e = eval_list(&e, env, global)?;
    trace!("Parameter for lt evaluated to {}", e);
    let mut param_iter = expression_iter(Rc::new(e.clone()));
    let params = (param_iter.next(), param_iter.next());
    match param_iter.next() {
        None => match params {
            (Some(a), Some(b)) if a.as_number().is_some() && b.as_number().is_some() => {
                let a = a.as_number().unwrap();
                let b = b.as_number().unwrap();
                if a < b {
                    eval(
                        &Expression::Symbol(Token::new("#t", "builtin", 0, 2)),
                        env,
                        global,
                    )
                } else {
                    Ok(Expression::Nil)
                }
            }
            _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
                .with_message("Less than cannot act on anything but a pair of numbers")),
        },
        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
            .with_message("Less than cannot act on anything but a pair of numbers")),
    }
    //    match e {
    //        Expression::Cons(a, b) if a.as_number().is_some() && b.as_number().is_some() => {
    //        }
    //        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
    //            .with_message("Less than cannot act on anything but a pair of numbers")),
    //    }
}

fn _builtin_not(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("not on {}", e);
    let e = eval_list(&e, env, global)?;
    trace!("Parameter for not evaluated to {}", e);
    match e {
        Expression::Nil => eval(&Expression::Symbol(Token::new("#t", "builtin", 0, 2)), env, global),
        _ => Ok(Expression::Nil),
    }
}

fn _builtin_cond(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Cond on {}", e);
    for item in expression_iter(Rc::new(e.clone())) {
        match item.as_ref() {
            Expression::Cons(pred, value) => match eval(pred.as_ref(), env, global)? {
                Expression::Nil => {
                    trace!("Condition {} evaluated to false", pred);
                    continue;
                },
                _ => {
                    trace!("Condition {} evaluated to true", pred);
                    return eval(value.as_ref(), env, global);
                }
            },
            _ => {
                return Err(Report::build(ReportKind::Error, "evaluation", 0)
                    .with_message("Conditional statement requires array of tuples"))
            }
        }
    }
    Err(Report::build(ReportKind::Error, "evaluation", 0).with_message("Conditional bottomed out"))
}

fn _builtin_quote(
    e: &Expression,
    _env: &Environment,
    _global: &Environment,
) -> Result<Expression, ErrReport> {
    trace!("Quote on {}", e);
    Ok(e.clone())
}

fn _builtin_eval(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    eval(&e, env, global)
}

fn _builtin_define(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    let mut define_body_iter = expression_iter(Rc::new(e.clone()));
    let define_body = (define_body_iter.next(), define_body_iter.next());
    match define_body_iter.next() {
        None => match define_body {
            (Some(name), Some(value)) => {
                if let Expression::Symbol(name) = name.as_ref().clone() {
                    Ok(Expression::Define(
                        name,
                        Rc::new(eval(value.as_ref(), env, global)?),
                    ))
                } else {
                    Err(Report::build(ReportKind::Error, "evaluation", 0)
                        .with_message("Define must be given a token as a first argument"))
                }
            }
            _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
                .with_message("Define must be called on array")
                .with_help("The first element of the array is the name.")
                .with_help("The remainder of the array is the expression to evaluate")),
        },
        _ => Err(Report::build(ReportKind::Error, "evaluation", 0)
            .with_message("Define must be called on an array of exactly two elements")
            .with_help("The first element must be the name.")
            .with_help("The second element is the value.")
            .with_help("This must be a NIL terminated list")),
    }
}

fn _builtin_list(
    e: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    eval_list(e, env, global)
}

fn _builtin_print_env(
    _: &Expression,
    env: &Environment,
    global: &Environment,
) -> Result<Expression, ErrReport> {
    println!("env : {}", env);
    println!("global : {}", global);
    Ok(Expression::Nil)
}

pub const BUILTIN_MUL: Expression = Expression::Builtin("×", _builtin_mul);
pub const BUILTIN_ADD: Expression = Expression::Builtin("+", _builtin_add);
pub const BUILTIN_NEG: Expression = Expression::Builtin("neg", _builtin_neg);
pub const BUILTIN_INV: Expression = Expression::Builtin("inv", _builtin_inv);
pub const BUILTIN_LAMBDA: Expression = Expression::Builtin("λ", _builtin_lambda);
pub const BUILTIN_MACRO: Expression = Expression::Builtin("macro", _builtin_macro);
pub const BUILTIN_LT: Expression = Expression::Builtin("<", _builtin_lt);
pub const BUILTIN_NOT: Expression = Expression::Builtin("not", _builtin_not);
pub const BUILTIN_COND: Expression = Expression::Builtin("cond", _builtin_cond);
pub const BUILTIN_QUOTE: Expression = Expression::Builtin("'", _builtin_quote);
pub const BUILTIN_EVAL: Expression = Expression::Builtin("eval", _builtin_eval);
pub const BUILTIN_DEFINE: Expression = Expression::Builtin("define", _builtin_define);
pub const BUILTIN_LIST: Expression = Expression::Builtin("list", _builtin_list);
pub const BUILTIN_PRINT_ENVIRONEMNT: Expression = Expression::Builtin("#env", _builtin_print_env);
