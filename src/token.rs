use std::fmt::Write;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    source: &'static str,
    // This is in characters from the beginning of the file
    position: usize,
    len: usize,
}

impl Token {
    pub const fn new(source: &'static str, position: usize, len: usize) -> Self {
        Token {
            source,
            position,
            len,
        }
    }
    pub fn chars(&self) -> impl Iterator<Item = char> {
        self.source.chars().skip(self.position).take(self.len)
    }
    pub fn list_to_string(tokens: &[Token]) -> String {
        let mut token_str = String::new();
        for token in tokens {
            // Safe as writing to a string should never fail
            write!(token_str, "{} ", token).unwrap();
        }
        token_str
    }
    pub fn start(&self) -> usize {
        self.position
    }
    pub fn end(&self) -> usize {
        self.position + self.len
    }
    pub fn source(&self) -> &'static str {
        self.source
    }
}

pub fn tokenise(source: &'static str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    let mut token_start = 0;
    while let Some(curr) = chars.next() {
        match curr {
            x if x.is_whitespace() => {}
            '(' | ')' | '\'' => {
                tokens.push(Token::new(source, token_start, 1));
            }
            _ => {
                let mut token_len = 0;
                while chars
                    .peek()
                    .map(|x| x.is_whitespace() || *x == '(' || *x == ')')
                    == Some(false)
                {
                    chars.next();
                    token_len += 1;
                }
                tokens.push(Token::new(source, token_start, token_len + 1));
                token_start += token_len;
            }
        }
        token_start += 1;
    }
    tokens
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.chars() {
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}
