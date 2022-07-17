use ariadne::{Label, Report, ReportBuilder, ReportKind, Source};
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Token {
    source: &'static str,
    position: (usize, usize),
}

impl Token {
    fn chars(&self) -> impl Iterator<Item = char> {
        self.source
            .chars()
            .skip(self.position.0)
            .take(self.position.1 - self.position.0)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.chars() {
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}

fn print_list_of_tokens(tokens: &[Token]) -> String {
    let mut token_str = String::new();
    for token in tokens {
        token_str += &format!("{} ", token);
    }
    token_str
}

fn tokenise(source: &'static str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    let mut token_start = 0;
    while let Some(curr) = chars.next() {
        match curr {
            '(' => {
                tokens.push(Token {
                    source,
                    position: (token_start, token_start + 1),
                });
            }
            ')' => {
                tokens.push(Token {
                    source,
                    position: (token_start, token_start + 1),
                });
            }
            x if x.is_whitespace() => {}
            _ => {
                let mut token_end = token_start;
                while chars
                    .peek()
                    .map(|x| x.is_whitespace() || *x == '(' || *x == ')')
                    == Some(false)
                {
                    chars.next();
                    token_end += 1;
                }
                tokens.push(Token {
                    source,
                    position: (token_start, token_end + 1),
                });
                token_start = token_end;
            }
        }
        token_start += 1;
    }
    tokens
}

#[derive(Clone, PartialEq)]
enum Expression {
    Number(f32),
    Symbol(Token),
    Builtin(&'static str, fn(Expression, Environment) -> Expression),
    Cons(Box<Expression>, Box<Expression>),
    /// A closure contains a (cons) list of parameter names, an expression to evaluate and the
    /// environment in which to do it.
    Closure(Box<Expression>, Box<Expression>, Environment),
    Nil,
}

fn _builtin_mul(e: Expression, env: Environment) -> Expression {
    trace!("Mul on {}", e);
    let mut ans = 1.;
    let mut e = eval_list(&e, env);
    loop {
        match e {
            Expression::Cons(a, b) => match *a {
                Expression::Number(a) => {
                    ans *= a;
                    e = *b;
                }
                _ => panic!("Cannot mul non numeric types"),
            },
            Expression::Nil => {
                break;
            }
            _ => panic!("Cannot mul not list"),
        }
    }
    Expression::Number(ans)
}

fn _builtin_add(e: Expression, env: Environment) -> Expression {
    trace!("Add on {}", e);
    let mut ans = 0.;
    let mut e = eval_list(&e, env);
    loop {
        match e {
            Expression::Cons(a, b) => match *a {
                Expression::Number(a) => {
                    ans += a;
                    e = *b;
                }
                _ => panic!("Cannot mul non numeric types"),
            },
            Expression::Nil => {
                break;
            }
            _ => panic!("Cannot mul not list"),
        }
    }
    Expression::Number(ans)
}

fn _builtin_lambda(e: Expression, env: Environment) -> Expression {
    trace!("Lambda on {}", e);
    match e {
        Expression::Cons(parameters, body) => {
            Expression::Closure(parameters.clone(), body.clone(), env)
        }
        _ => panic!("Lambda not acting on array"),
    }
}

const BUILTIN_MUL: Expression = Expression::Builtin("×", _builtin_mul);
const BUILTIN_ADD: Expression = Expression::Builtin("+", _builtin_add);
const BUILTIN_LAMBDA: Expression = Expression::Builtin("λ", _builtin_lambda);

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

    fn car(&self) -> &Expression {
        match self {
            Self::Cons(a, _) => a,
            _ => panic!("Cannot take car of non array"),
        }
    }

    fn cdr(&self) -> &Expression {
        match self {
            Self::Cons(_, b) => b,
            _ => panic!("Cannot take cdr of non array"),
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
) -> Result<(Expression, usize), ReportBuilder<std::ops::Range<usize>>> {
    match tokens.first() {
        Some(token) => {
            if token.chars().next() == Some(')') {
                return Ok((Expression::Nil, 1));
            }
            let (head, head_len) = make_expression(tokens)?;
            let tokens = &tokens[head_len..];
            assert!(tokens.len() > 0);
            if tokens[0].chars().next() == Some('.') {
                let (tail, tail_len) = make_expression(&tokens[1..])?;
                Ok((
                    Expression::Cons(Box::new(head), Box::new(tail)),
                    //TODO(robert) ensure there is a close brace here
                    head_len + tail_len + 2,
                ))
            } else {
                let (tail, tail_len) = make_list_expression(tokens)?;
                Ok((
                    Expression::Cons(Box::new(head), Box::new(tail)),
                    head_len + tail_len,
                ))
            }
        }
        None => {
            Err(Report::build(ReportKind::Error, (), 0).with_message("EOF while scanning list"))
        }
    }
}

fn make_expression(
    tokens: &[Token],
) -> Result<(Expression, usize), ReportBuilder<std::ops::Range<usize>>> {
    match tokens.split_first() {
        Some((token, rest)) => {
            if token.chars().next() == Some(')') {
                return Err(Report::build(ReportKind::Error, (), 0)
                    .with_message("Unexpected close brace")
                    .with_label(
                        Label::new(token.position.0..token.position.1)
                            .with_message("This close brace"),
                    ));
            }
            if token.chars().next() == Some('(') {
                let (list, list_len) = make_list_expression(rest)?;
                return Ok((list, list_len + 1));
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
            return Err(Report::build(ReportKind::Error, (), 0).with_message("Unexpected EOF"));
        }
    }
}

#[derive(PartialEq, Clone)]
struct Environment {
    variables: Vec<(String, Expression)>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: vec![
                ("ZERO".to_string(), Expression::Number(0.)),
                ("λ".to_string(), BUILTIN_LAMBDA),
                ("+".to_string(), BUILTIN_ADD),
                ("*".to_string(), BUILTIN_MUL),
            ],
        }
    }
    fn associate(&self, symbol: Token) -> Expression {
        let symbol_name: String = symbol.chars().collect();
        for (name, value) in self.variables.iter().rev() {
            if name == &symbol_name {
                return value.clone();
            }
        }
        panic!("Symbol not in environment");
    }
    fn bind(&mut self, symbol: Token, value: Expression) {
        let symbol_name: String = symbol.chars().collect();
        self.variables.push((symbol_name, value));
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{..")?;
        for (name, value) in self.variables.iter().skip(4) {
            write!(f, ", {} => {}", name, value)?;
        }
        write!(f, "}}")
    }
}

fn eval(expression: &Expression, environment: Environment) -> Expression {
    trace!("Eval {} in {}", expression, environment);
    match expression {
        Expression::Symbol(symbol) => environment.associate(*symbol),
        Expression::Cons(f, v) => apply(
            eval(f, environment.clone()),
            *v.clone(),
            environment.clone(),
        ),
        expression => expression.clone(),
    }
}

fn eval_list(expression: &Expression, environment: Environment) -> Expression {
    trace!("Eval list {}", expression);
    match expression {
        Expression::Cons(h, t) => Expression::Cons(
            Box::new(eval(h, environment.clone())),
            Box::new(eval_list(t, environment.clone())),
        ),
        e => eval(e, environment),
    }
}

fn apply(function: Expression, arguments: Expression, environment: Environment) -> Expression {
    trace!("Apply {} to {}", function, arguments);
    match function {
        Expression::Builtin(_, function) => function(arguments, environment),
        Expression::Closure(parameters, body, closure_environment) => reduce(
            (*parameters, *body, closure_environment),
            arguments,
            environment,
        ),
        /*match *parameters {

            Expression::Nil => eval(body.as_ref(), environment),
            Expression::Cons(x, rest_param) => match *x {
                Expression::Symbol(token) => match arguments {
                    Expression::Cons(val, rest_arg) => {
                        todo!()
                    }
                    Expression::Nil => {
                        panic!("Insufficient arguments");
                    }
                    _ => panic!("Arguments must be list"),
                },
                _ => panic!("Paramters must be symbols"),
            },
            _ => panic!("Only symbols allowed as parameters"),
        },*/
        _ => panic!("Cannot apply {}", function),
    }
}

fn reduce(
    (mut closure_params, closure_body, closure_env): (Expression, Expression, Environment),
    arguments: Expression,
    environment: Environment,
) -> Expression {
    let mut arguments = eval_list(&arguments, environment);
    let mut new_env = closure_env.clone();
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
                closure_params = *b;
                arguments = *d;
            }
            _ => panic!("Mismatch of bindings"),
        }
    }
    eval(&closure_body, new_env)
}

fn main() {
    pretty_env_logger::init();

    let source = "(
    ((λ (x) . (λ (y) . (+ x y))) 3) 4
    )";

    let tokens = tokenise(source);

    debug!("Tokens: {}", print_list_of_tokens(&tokens));

    let environment = Environment::new();

    match make_expression(&tokens) {
        Ok(exp) => {
            debug!("Expression: {}", exp.0);
            println!("{}", eval(&exp.0, environment));
        }
        Err(report) => {
            report.finish().print(Source::from(source)).unwrap();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn expr_test(source: &'static str, expression: Expression) {
        let environment = Environment::new();
        match make_expression(&tokenise(source)) {
            Ok((expr, _)) => {
                let ans = eval(&expr, environment);
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
}
