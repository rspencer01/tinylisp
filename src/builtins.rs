use crate::*;

fn _builtin_mul(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("Mul on {}", e);
    let mut ans = 1.;
    let mut e = Rc::new(eval_list(&e, env)?);
    trace!("Parameter for mul evaluated to {}", e);
    loop {
        match e.as_ref() {
            Expression::Cons(a, b) => match a.as_ref() {
                Expression::Number(a) => {
                    ans *= a;
                    e = b.clone();
                }
                _ => panic!("Cannot mul non numeric types"),
            },
            Expression::Nil => {
                break;
            }
            _ => panic!("Cannot mul not list"),
        }
    }
    Ok(Expression::Number(ans))
}

fn _builtin_add(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("Add on {}", e);
    let mut ans = 0.;
    let mut e = Rc::new(eval_list(&e, env)?);
    trace!("Parameter for add evaluated to {}", e);
    loop {
        match e.as_ref() {
            Expression::Cons(a, b) => match a.as_ref() {
                Expression::Number(a) => {
                    ans += a;
                    e = b.clone();
                }
                _ => panic!("Cannot mul non numeric types"),
            },
            Expression::Nil => {
                break;
            }
            _ => panic!("Cannot mul not list"),
        }
    }
    Ok(Expression::Number(ans))
}

fn _builtin_lambda(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("Lambda on {}", e);
    match e {
        Expression::Cons(parameters, body) => Ok(Expression::Closure(parameters.clone(), body.clone(), env)),
        _ => panic!("Lambda not acting on array"),
    }
}

fn _builtin_neg(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("Neg on {}", e);
    let e = eval(&e, env)?;
    trace!("Parameter for neg evaluated to {}", e);
    match e {
        Expression::Number(x) => Ok(Expression::Number(-x)),
        _ => Err(Report::build(ReportKind::Error, (), 0)
            .with_message("Neg cannot act on anything but a number")),
    }
}

fn _builtin_inv(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("Inv on {}", e);
    let e = eval(&e, env)?;
    trace!("Parameter for inv evaluated to {}", e);
    match e {
        Expression::Number(0.) => {
            Err(Report::build(ReportKind::Error, (), 0).with_message("Inv cannot be applied to 0"))
        }
        Expression::Number(x) => Ok(Expression::Number(1. / x)),
        _ => Err(Report::build(ReportKind::Error, (), 0)
            .with_message("Neg cannot act on anything but a number")),
    }
}

fn _builtin_lt(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("lt on {}", e);
    let e = eval_list(&e, env)?;
    trace!("Parameter for lt evaluated to {}", e);
    match e {
        Expression::Cons(a, b)
            if matches!(*a, Expression::Number(_)) && matches!(*b, Expression::Number(_)) =>
        {
            let a = a.as_number().unwrap();
            let b = b.as_number().unwrap();
            if a < b {
                Ok(BUILTIN_TRUE)
            } else {
                Ok(Expression::Nil)
            }
        }
        _ => Err(Report::build(ReportKind::Error, (), 0)
            .with_message("Less than cannot act on anything but a pair of numbers")),
    }
}

fn _builtin_not(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    trace!("not on {}", e);
    let e = eval_list(&e, env)?;
    trace!("Parameter for not evaluated to {}", e);
    match e {
        Expression::Nil => Ok(BUILTIN_TRUE),
        _ => Ok(Expression::Nil),
    }
}

fn _builtin_if(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
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
            _ => Err(Report::build(ReportKind::Error, (), 0)
                .with_message("If statement requires two options")),
        },
        _ => Err(Report::build(ReportKind::Error, (), 0)
            .with_message("If statement requires a predicate and a list for options")),
    }
}

fn _builtin_quote(e: &Expression, _env: Environment) -> Result<Expression, ErrReport> {
    trace!("Quote on {}", e);
    Ok(e.clone())
}

fn _builtin_eval(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    eval(&e, env)
}

fn _builtin_define(e: &Expression, env: Environment) -> Result<Expression, ErrReport> {
    match e {
        Expression::Cons(name, val) => match name.as_ref() {
            Expression::Symbol(token) => Ok(Expression::Define(
                *token,
                Rc::new(eval(val.as_ref(), env)?),
            )),
            _ => Err(Report::build(ReportKind::Error, (), 0)
                .with_message("Definition requires a token as name")),
        },
        _ => {
            Err(Report::build(ReportKind::Error, (), 0).with_message("Definition requires a list"))
        }
    }
}

pub const BUILTIN_MUL: Expression = Expression::Builtin("×", _builtin_mul);
pub const BUILTIN_ADD: Expression = Expression::Builtin("+", _builtin_add);
pub const BUILTIN_NEG: Expression = Expression::Builtin("neg", _builtin_neg);
pub const BUILTIN_INV: Expression = Expression::Builtin("inv", _builtin_inv);
pub const BUILTIN_LAMBDA: Expression = Expression::Builtin("λ", _builtin_lambda);
pub const BUILTIN_LT: Expression = Expression::Builtin("<", _builtin_lt);
pub const BUILTIN_TRUE: Expression = Expression::Symbol(Token::new("#t", 0, 2));
pub const BUILTIN_NOT: Expression = Expression::Builtin("not", _builtin_not);
pub const BUILTIN_IF: Expression = Expression::Builtin("if", _builtin_if);
pub const BUILTIN_QUOTE: Expression = Expression::Builtin("'", _builtin_quote);
pub const BUILTIN_EVAL: Expression = Expression::Builtin("eval", _builtin_eval);
pub const BUILTIN_DEFINE: Expression = Expression::Builtin("define", _builtin_define);
