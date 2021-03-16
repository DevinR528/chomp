use crate::lex::Token;

pub struct TokenStream<'s> {
    stream: Vec<Token>,
    input: &'s str,
}
