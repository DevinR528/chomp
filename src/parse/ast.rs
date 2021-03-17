use crate::lex::{Token, TokenKind, tokenize};

pub mod token;

use token::Item;

// TODO: this is basically one file = one mod/crate/program unit add mods and crates linking or whatever.
/// Create an AST from input `str`.
#[derive(Debug)]
pub struct AstBuilder {
    items: Vec<Item>,
}

impl AstBuilder {
    pub fn parse(input: &str) -> Self {
        let stream = tokenize(input);

        Self {
            items: parse_helper(stream, input),
        }
    }
}

fn parse_helper<I: Iterator<Item = Token>>(mut stream: I, input: &str) -> Vec<Item> {
    let mut items = vec![];
    loop {
        match stream.next() {
            Some(Token { kind: TokenKind::Ident, len }) => 
            None => break,
        }
    }
    items
}
