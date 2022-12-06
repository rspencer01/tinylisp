// Tinylisp: yet another lisp
// Copyright (C) 2022 Robert Spencer<code@robertandrewspencer.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    value: String,
    source_id: &'static str,
    // This is in characters from the beginning of the file
    position: usize,
    len: usize,
}

impl Token {
    pub fn new(source: &str, source_id: &'static str, position: usize, len: usize) -> Self {
        Token {
            source_id,
            position,
            len,
            value: source.chars().skip(position).take(len).collect(),
        }
    }
    pub fn chars(&self) -> Chars {
        self.value.chars()
    }
    pub fn start(&self) -> usize {
        self.position
    }
    pub fn end(&self) -> usize {
        self.position + self.len
    }
    pub fn source_id(&self) -> &'static str {
        self.source_id
    }
}

pub fn tokenise(source: &str, source_id: &'static str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    let mut token_start = 0;
    while let Some(curr) = chars.next() {
        match curr {
            x if x.is_whitespace() => {}
            '(' | ')' | '\'' | '⋮' => {
                tokens.push(Token::new(source, source_id, token_start, 1));
            }
            ';' => {
                while chars.peek().map(|x| *x == '\n') == Some(false) {
                    chars.next();
                    token_start += 1;
                }
            }
            _ => {
                let mut token_len = 0;
                while chars
                    .peek()
                    .map(|x| x.is_whitespace() || *x == '(' || *x == ')' || *x == '⋮')
                    == Some(false)
                {
                    chars.next();
                    token_len += 1;
                }
                tokens.push(Token::new(source, source_id, token_start, token_len + 1));
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
