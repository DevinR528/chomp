use std::{collections::HashMap, convert::TryInto};

use crate::lex::{tokenize, Token as LexToken, TokenKind, TokenMatch};

use super::span::{kw, Ident, Span};

pub mod error;
pub mod token;

use ast::{Ty, TyKind};
use error::ParseError;
use token as ast;

pub type ParseResult<T> = Result<T, ParseError>;

// TODO: this is basically one file = one mod/crate/program unit add mods and crates linking or whatever.
/// Create an AST from input `str`.
#[derive(Debug, Default)]
pub struct AstBuilder<'a> {
    tokens: Vec<LexToken>,
    curr: LexToken,

    input: &'a str,
    input_idx: usize,

    items: Vec<ast::Item>,
    /// Since we are relying on the user to give us correct code this just
    /// keeps track of generic things and their uses so we can emit monomorphized
    /// versions of generic whatevers.
    generic_resolver: HashMap<(), ()>,
}

impl<'a> AstBuilder<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut tokens = tokenize(input).collect::<Vec<_>>();
        let curr = tokens.remove(0);
        Self {
            tokens,
            curr,
            input,
            ..Default::default()
        }
    }
    pub fn items(&self) -> &[ast::Item] {
        &self.items
    }

    pub fn parse(&mut self) -> ParseResult<()> {
        loop {
            println!("here {}", self.input_curr());
            println!("here {:?}", self.curr);
            // We pay no attention to the privacy of items.
            self.eat_keyword(kw::Keywords::Pub);

            match self.curr.kind {
                // Ignore
                TokenKind::LineComment { .. }
                | TokenKind::BlockComment { .. }
                | TokenKind::Whitespace { .. } => {
                    self.eat_tkn();
                    continue;
                }
                TokenKind::Ident => {
                    let keyword: kw::Keywords = self.input_curr().try_into()?;
                    match keyword {
                        kw::Keywords::Const => self.parse_const()?,
                        kw::Keywords::Extern => {}
                        kw::Keywords::Fn => self.parse_fn()?,
                        kw::Keywords::Impl => {}
                        kw::Keywords::Mod => {}
                        kw::Keywords::Pub => {}
                        kw::Keywords::Static => {}
                        kw::Keywords::Struct => {}
                        kw::Keywords::Enum => {}
                        kw::Keywords::Trait => {}
                        kw::Keywords::Type => {}
                        // kw::Keywords::Unsafe => {}
                        kw::Keywords::Use => {}
                        kw::Keywords::Macro => {}
                        kw::Keywords::MacroRules => {}
                        kw::Keywords::Union => {}
                        _ => todo!("can we reach here?"),
                    }
                }
                TokenKind::Pound => {
                    self.eat_attr();
                    continue;
                }
                TokenKind::Unknown => todo!("Unknown token"),
                tkn => todo!("Unknown token {:?}", tkn),
            }
        }
        Ok(())
    }

    fn parse_const(&mut self) -> ParseResult<()> {
        let start = self.input_idx;

        // Eat the `const` token
        self.eat_tkn();
        self.eat_if(&TokenMatch::Whitespace);
        let id = self.make_ident()?;
        self.eat_if(&TokenMatch::Colon);
        self.eat_if(&TokenMatch::Whitespace);
        let ty = self.make_ty()?;
        Ok(())
    }

    fn parse_fn(&mut self) -> ParseResult<()> {
        Ok(())
    }

    fn make_ident(&mut self) -> ParseResult<Ident> {
        let id = Ident::new(self.curr_span());
        println!("make id `{}`", id.name(self.input));
        println!("make id {:?}", self.curr);

        self.eat_if(&TokenMatch::Ident);
        Ok(id)
    }

    fn make_ty(&mut self) -> ParseResult<Ty> {
        println!("make ty {}", self.input_curr());
        println!("make ty {:?}", self.curr);

        match self.curr.kind {
            TokenKind::Ident => {}
            TokenKind::RawIdent => {}
            TokenKind::Literal { kind, suffix_start } => {}
            TokenKind::Lifetime { starts_with_number } => {}
            TokenKind::OpenParen => {}
            TokenKind::OpenBrace => {}
            TokenKind::OpenBracket => {}
            // TokenKind::Lt => {}
            // TokenKind::Gt => {}
            TokenKind::And => {}
            TokenKind::Star => {}
            tkn => todo!("Unknown token {:?}", tkn),
        }

        Ok(todo!())
    }

    /// FIXME: for now we ignore attributes.
    fn eat_attr(&mut self) {
        if matches!(
            self.peek().unwrap_or(&TokenKind::Unknown),
            TokenKind::OpenBracket
        ) {
            self.eat_until(&TokenMatch::CloseBracket);
            // eat the CloseBracket
            self.eat_tkn();
        }
    }

    /// Eat the key word iff it matches `kw`.
    fn eat_keyword(&mut self, kw: kw::Keywords) {
        if self.input_curr() == kw.text() {
            self.eat_tkn();
        }
    }

    /// Check if a sequence matches `iter`.
    fn cmp_seq<I: Iterator<Item = TokenMatch>>(&mut self, mut iter: I) -> bool {
        let first = iter.next().unwrap_or(TokenMatch::Unknown);
        if first != self.curr.kind {
            return false;
        }

        let tkns = self.tokens.iter();
        tkns.zip(iter).all(|(ours, cmp)| cmp == ours.kind)
    }

    /// Throw away a sequence of tokens.
    ///
    /// Returns true if all the given tokens were matched.
    fn eat_seq<I: Iterator<Item = TokenMatch>>(&mut self, iter: I) -> bool {
        for kind in iter {
            if kind == self.curr.kind {
                self.eat_tkn();
            } else {
                return false;
            }
        }
        true
    }

    /// Eat tokens until `pat` matches current.
    fn eat_until(&mut self, pat: &TokenMatch) {
        while pat != &self.curr.kind {
            self.eat_tkn();
        }
    }

    /// Bump the current token if it matches `pat`.
    fn eat_if(&mut self, pat: &TokenMatch) {
        if pat == &self.curr.kind {
            self.eat_tkn();
        }
    }

    /// Bump the next token into the current spot.
    fn eat_tkn(&mut self) {
        self.input_idx += self.curr.len;
        self.curr = self.tokens.remove(0);
    }

    /// Peek the next token.
    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.first().map(|t| &t.kind)
    }

    /// Peek the next `n` tokens.
    fn peek_n(&self, n: usize) -> impl Iterator<Item = &TokenKind> {
        self.tokens.iter().take(n).map(|t| &t.kind)
    }

    /// The input `str` from current index to `stop`.
    fn input_to(&self, stop: usize) -> &str {
        &self.input[self.input_idx..stop]
    }

    /// The input `str` from current index to `Token` length.
    fn input_curr(&self) -> &str {
        let stop = self.input_idx + self.curr.len;
        &self.input[self.input_idx..stop]
    }

    /// The input `str` from current index to `stop`.
    fn curr_span(&self) -> Span {
        let stop = self.input_idx + self.curr.len;
        Span::new(self.input_idx..stop)
    }
}
