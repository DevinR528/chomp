use std::convert::TryInto;

use rustc_hash::FxHashMap;
use syntax::{
    ast::{
        self, ArgListOwner, AstNode, AttrsOwner, GenericParamsOwner, LoopBodyOwner,
        ModuleItemOwner, NameOwner, TypeBoundsOwner, VisibilityOwner,
    },
    SourceFile, SyntaxKind, SyntaxNode, SyntaxToken, T,
};

use crate::resolve::TypeResolver;

use super::span::{self, kw, Ident, Span};

pub mod error;

use error::ParseError;

pub type ParseResult<T> = Result<T, ParseError>;

// TODO: this is basically one file = one mod/crate/program unit.
// Need to add mods and crates linking or whatever.
/// Create an AST from input `str`.
#[derive(Debug)]
pub struct AstBuilder {
    root: SyntaxNode,
    items: Vec<ast::Item>,
    /// Since we are relying on the user to give us correct code this just
    /// keeps track of generic things and their uses so we can emit monomorphized
    /// versions of generic whatevers.
    generic_resolver: FxHashMap<(), ()>,
    type_resolver: TypeResolver,
}

impl AstBuilder {
    pub fn new(input: &str) -> Self {
        let source = SourceFile::parse(input)
            .ok()
            .expect("TODO: SyntaxError's from SourceFile::parse");

        Self {
            root: source.syntax().clone(),
            items: vec![],
            generic_resolver: FxHashMap::default(),
            type_resolver: TypeResolver::default(),
        }
    }

    pub fn items(&self) -> &[ast::Item] {
        &self.items
    }

    pub fn type_res(&self) -> &TypeResolver {
        &self.type_resolver
    }

    pub fn parse(&mut self) -> ParseResult<()> {
        let items = self.root.children();
        for item in items.flat_map(ast::Item::cast) {
            self.items.push(item);
        }
        Ok(())
    }
}
