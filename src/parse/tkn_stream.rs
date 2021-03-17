use crate::lex::{tokenize, Token};

#[derive(Debug)]
pub struct TokenStream<'s> {
    stream: Vec<Token>,
    input: &'s str,
}

impl<'s> TokenStream<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            stream: tokenize(input).collect(),
            input,
        }
    }
}
