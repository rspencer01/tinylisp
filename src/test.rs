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

struct ReplacingIterator<P, T>
where
    P: Fn(char) -> bool,
    T: Iterator<Item = char>,
{
    source: T,
    predicate: P,
    to_insert: &'static str,
    replacement_iter: Option<Chars<'static>>,
}

impl<P, T> ReplacingIterator<P, T>
where
    P: Fn(char) -> bool,
    T: Iterator<Item = char>,
{
    fn new(source: T, predicate: P, to_insert: &'static str) -> Self {
        ReplacingIterator {
            source,
            predicate,
            to_insert,
            replacement_iter: None,
        }
    }
}

impl<P, T> Iterator for ReplacingIterator<P, T>
where
    P: Fn(char) -> bool,
    T: Iterator<Item = char>,
{
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = &mut self.replacement_iter {
            if let Some(c) = x.next() {
                return Some(c);
            } else {
                self.replacement_iter = None;
            }
        }
        'replacement: loop {
            let v = self.source.next();
            if v.map(&self.predicate) == Some(true) {
                let mut replacement_iter = self.to_insert.chars();
                let nxt = replacement_iter.next();
                if nxt.is_none() {
                    continue 'replacement;
                }
                self.replacement_iter = Some(replacement_iter);
                return nxt;
            } else {
                return v;
            }
        }
    }
}

