use ariadne::{Label, Report, ReportBuilder, ReportKind, Source};

#[derive(Debug, Copy, Clone)]
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

#[derive(Copy, Clone, Debug)]
enum Atom {
    Number(f32),
    Symbol(Token),
    Mul,
}

impl Atom {
    fn new(token: Token) -> Self {
        match token.chars().collect::<String>().parse() {
            Ok(v) => Atom::Number(v),
            Err(_) => {
                if token.chars().collect::<String>() == "*" {
                    Atom::Mul
                } else {
                    Atom::Symbol(token)
                }
            }
        }
    }
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Number(v) => {
                write!(f, "{}", v)
            }
            Atom::Symbol(t) => {
                write!(f, "{}", t)
            }
            Atom::Mul => {
                write!(f, "\x1B[1;31m×\x1B[0m")
            }
        }
    }
}

enum Expression {
    Atom(Atom),
    List(Vec<Expression>),
}

impl Expression {
    fn len(&self) -> usize {
        match self {
            Expression::List(l) => {
                let mut len = 2;
                for item in l {
                    len += item.len()
                }
                len
            }
            Expression::Atom(_) => 1,
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Atom(t) => {
                write!(f, "{}", t)?;
            }
            Expression::List(l) => {
                write!(f, "[ ")?;
                let mut first = true;
                for item in l.iter() {
                    if !first {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                    first = false;
                }
                write!(f, " ]")?;
            }
        }
        Ok(())
    }
}

fn make_expression(tokens: &[Token]) -> Result<Expression, ReportBuilder<std::ops::Range<usize>>> {
    match tokens.split_first() {
        Some((token, mut rest)) => {
            if token.chars().next() == Some(')') {
                return Err(Report::build(ReportKind::Error, (), 0)
                    .with_message("Unexpected close brace")
                    .with_label(
                        Label::new(token.position.0..token.position.1)
                            .with_message("This close brace"),
                    ));
            }
            if token.chars().next() == Some('(') {
                let mut list = Vec::new();
                loop {
                    match rest.first() {
                        None => {
                            return Err(Report::build(ReportKind::Error, (), 0)
                                .with_message("Unexpected EOF")
                                .with_label(
                                    Label::new(token.position.0..token.position.1)
                                        .with_message("While scanning this list"),
                                ));
                        }
                        Some(t) => {
                            if t.chars().next() == Some(')') {
                                break;
                            }
                        }
                    }
                    let item = make_expression(rest)?;
                    rest = &rest[item.len()..];
                    list.push(item);
                }
                Ok(Expression::List(list))
            } else {
                Ok(Expression::Atom(Atom::new(*token)))
            }
        }
        None => {
            return Err(Report::build(ReportKind::Error, (), 0).with_message("Unexpected EOF"));
        }
    }
}

fn eval(expression: &Expression) -> Atom {
    match expression {
        Expression::Atom(atom) => *atom,
        Expression::List(list) => {
            let list: Vec<_> = list.iter().map(eval).collect();
            match list.split_first() {
                Some((&Atom::Mul, vals)) => {
                    assert!(vals.len() == 2);
                    match (vals[0], vals[1]) {
                        (Atom::Number(a), Atom::Number(b)) => Atom::Number(a * b),
                        (_, _) => {
                            panic!();
                        }
                    }
                }
                None => {
                    panic!()
                }
                _ => todo!(),
            }
        }
    }
}

fn main() {
    let _source = "(let (radius 10.200) (* pi (* radius radius)))";
    let _source = "(* 3.1415 (* 10 10))";
    let source = "(* (* 3.1415 4) ( * 10 (* 10 10)))";
    let tokens = tokenise(source);
    for token in &tokens {
        print!("{} ", token);
    }
    println!();
    match make_expression(&tokens) {
        Ok(exp) => {
            println!("{}", exp);
            println!("{}", eval(&exp));
        }
        Err(report) => {
            report.finish().print(Source::from(source)).unwrap();
        }
    }
}
