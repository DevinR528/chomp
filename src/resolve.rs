use std::hash::{Hash, Hasher};

use fxhash::{FxHashMap, FxHashSet, FxHasher64};

use crate::parse::{ast_build::token as tkn, span::Ident};

pub type ScopeId = u64;

pub type NodeId = u64;

pub type ItemId = u64;

#[derive(Debug)]
pub enum TyId {
    Known(u64),
    Unknown,
}

/// The scope kind and the restrictions placed on vars.
#[derive(Copy, Clone, Debug)]
pub enum ScopeKind {
    /// We are in a function.
    ///
    /// Only allow the parameters since we cannot capture anything.
    Function,
    /// Module scope context.
    Mod,
    /// Any kind of normal scope where outer vars are allowed.
    NormalBlock,
    /// Closure scope keep track of what is captured.
    Closure,
    /// Item scope.
    ///
    /// Only other items are allowed to cross into this scope.
    Item,
}

#[derive(Debug)]
pub struct Scope {
    vis: FxHashSet<NodeId>,
    id_type: FxHashMap<NodeId, TyId>,
    kind: ScopeKind,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            vis: FxHashSet::default(),
            id_type: FxHashMap::default(),
            kind,
        }
    }

    fn add_ident(&mut self, id: &Ident, ty: Option<&tkn::Ty>) {
        let mut hasher = FxHasher64::default();
        let hash = id.hash(&mut hasher);
        let id = hasher.finish();

        self.vis.insert(id);

        let ty = if let Some(ty) = ty {
            let mut hasher = FxHasher64::default();
            let hash = ty.hash(&mut hasher);
            let id = hasher.finish();
            TyId::Known(id)
        } else {
            TyId::Unknown
        };

        self.id_type.insert(id, ty);
    }
}
#[derive(Debug, Default)]
pub struct TypeResolver {
    scope_map: FxHashMap<ItemId, Scope>,
    rev_scope_map: FxHashMap<ItemId, FxHashMap<NodeId, ScopeId>>,
    type_map: FxHashMap<NodeId, TyId>,
    interactions: FxHashMap<NodeId, Vec<NodeId>>,
    rev_inter: FxHashMap<NodeId, Vec<NodeId>>,
}

impl TypeResolver {
    pub fn resolve(items: &[tkn::Item]) -> Self {
        let mut resolver = Self::default();
        for item in items {
            match &item.kind {
                tkn::ItemKind::Krate => {}
                tkn::ItemKind::Use => {}
                tkn::ItemKind::Static(_, _) => {}
                tkn::ItemKind::Const(_, _) => {}
                tkn::ItemKind::Fn(func) => {
                    let id = resolver.enter_item(func);
                    resolver.func_params(&func.sig.inputs);
                    resolver.func_body(&func.block, id);
                    resolver.func_return(func, &func.sig.ret);
                }
                tkn::ItemKind::Mod(_) => {}
                tkn::ItemKind::TyAlias(_) => {}
                tkn::ItemKind::Enum => {}
                tkn::ItemKind::Struct => {}
                tkn::ItemKind::Union => {}
                tkn::ItemKind::Trait => {}
                tkn::ItemKind::Impl => {}
            }
        }
        resolver
    }

    pub fn func_params(&mut self, inputs: &[tkn::Param]) {}

    pub fn func_body(&mut self, blk: &tkn::Block, it_id: ItemId) {
        for stmt in &blk.stmts {
            match &stmt.kind {
                tkn::StmtKind::Local(loc) => match &loc.pat.kind {
                    tkn::PatKind::Ident { bind, id, sub } => {
                        self.add_var(&it_id, id, loc.ty.as_ref());
                    }
                    tkn::PatKind::Struct { name, fields } => {}
                    tkn::PatKind::TupleStruct { name, data } => {}
                    tkn::PatKind::Tuple(_) => {}
                    tkn::PatKind::Ref { mutable, pat } => {}
                    tkn::PatKind::Paren(_) => {}
                    todo_tkn => todo!("{:?}", todo_tkn),
                },
                tkn::StmtKind::Item(_) => {}
                tkn::StmtKind::Expr(_) => {}
                tkn::StmtKind::Semi(_) => {}
                tkn::StmtKind::Empty => {}
            }
        }
    }

    pub fn func_return(&mut self, func: &tkn::FnKind, inputs: &tkn::FnReturn) {}

    pub fn enter_item(&mut self, item: impl Hash) -> ItemId {
        let mut hasher = FxHasher64::default();
        let hash = item.hash(&mut hasher);
        let item = hasher.finish();

        self.scope_map.insert(item, Scope::new(ScopeKind::Item));
        item
    }

    fn add_var(&mut self, it_id: &ItemId, id: &Ident, ty: Option<&tkn::Ty>) {
        self.scope_map.get_mut(it_id).unwrap().add_ident(id, ty);
    }
}
