use std::hash::{Hash, Hasher};

use fxhash::{FxHashMap, FxHashSet, FxHasher64};
use syntax::ast::{
    self, ArgListOwner, AstNode, AttrsOwner, GenericParamsOwner, LoopBodyOwner, ModuleItemOwner,
    NameOwner, TypeBoundsOwner,
};

mod fir;
mod intern;

use intern::{TyId, TypeIntern};

pub type ScopeId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(usize);

impl NodeId {
    pub fn new<T: AstNode>(node: T) -> Self {
        Self(node.syntax().index())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(usize);

impl ItemId {
    pub fn new<T: AstNode>(node: &T) -> Self {
        Self(node.syntax().index())
    }
}

#[derive(Debug)]
pub struct Ident {
    id: NodeId,
}

#[derive(Debug)]
pub enum Stmt {
    Local(),
    Expr(),
}

#[derive(Debug)]
pub struct TyContext {}

#[derive(Debug)]
pub struct BodyContext {
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct ArgContext {
    ident: Ident,
    ty: TyId,
}

#[derive(Debug)]
pub struct FnContext {
    name: String,
    uses: Vec<NodeId>,
    args: Vec<ArgContext>,
    body: BodyContext,
    ret: TyId,
}

#[derive(Debug)]
pub enum ItemKind {
    Fn(FnContext),
}

#[derive(Debug)]
pub struct ItemContext {
    scope: Scope,
    kind: ItemKind,
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

    pub fn add_node(&mut self, n: NodeId, ty: TyContext) {}
}
#[derive(Debug, Default)]
pub struct TypeResolver {
    types: TypeIntern,
    items: FxHashMap<ItemId, ItemContext>,
    type_map: FxHashMap<NodeId, TyId>,
    interactions: FxHashMap<NodeId, Vec<NodeId>>,
}

impl TypeResolver {
    pub fn resolve(items: &[ast::Item]) -> Self {
        let mut resolver = Self::default();
        for item in items {
            match &item {
                ast::Item::Const(_) => {}
                ast::Item::Enum(_) => {}
                ast::Item::ExternBlock(_) => {}
                ast::Item::ExternCrate(_) => {}
                ast::Item::Fn(func) => resolver.alloc_func(func),
                ast::Item::Impl(_) => {}
                ast::Item::MacroCall(_) => {}
                ast::Item::MacroRules(_) => {}
                ast::Item::MacroDef(_) => {}
                ast::Item::Module(_) => {}
                ast::Item::Static(_) => {}
                ast::Item::Struct(_) => {}
                ast::Item::Trait(_) => {}
                ast::Item::TypeAlias(_) => {}
                ast::Item::Union(_) => {}
                ast::Item::Use(_) => {}
            }
        }
        resolver
    }

    pub fn alloc_func(&mut self, func: &ast::Fn) {
        let id = ItemId::new(func);
        let mut scope = Scope::new(ScopeKind::Function);
        let args = self.alloc_params(&mut scope, func.param_list().unwrap());
        let body = self.alloc_body(&mut scope, func.body().unwrap());
        let ret = self.fn_ret(func.ret_type().unwrap());
        self.items.insert(
            id,
            ItemContext {
                scope,
                kind: ItemKind::Fn(FnContext {
                    name: func.name().unwrap().to_string(),
                    args,
                    ret,
                    body,
                    uses: vec![],
                }),
            },
        );
    }

    pub fn alloc_body(&mut self, scope: &mut Scope, blk: ast::BlockExpr) -> BodyContext {
        todo!()
    }

    pub fn alloc_params(&mut self, scope: &mut Scope, params: ast::ParamList) -> Vec<ArgContext> {
        let mut args = vec![];

        if let Some(this) = params.self_param() {
            todo!();
            scope.vis.insert(NodeId(0));
        }

        for param in params.params() {
            let ident = match param.pat().unwrap() {
                ast::Pat::IdentPat(ident) => {
                    let id = NodeId::new(ident);
                    Ident { id }
                }
                ast::Pat::BoxPat(_) => todo!(),
                ast::Pat::RestPat(_) => todo!(),
                ast::Pat::LiteralPat(lit) => todo!(),
                ast::Pat::MacroPat(_) => todo!(),
                ast::Pat::OrPat(_) => todo!(),
                ast::Pat::ParenPat(_) => todo!(),
                ast::Pat::PathPat(_) => todo!(),
                ast::Pat::WildcardPat(_) => todo!(),
                ast::Pat::RangePat(_) => todo!(),
                ast::Pat::RecordPat(_) => todo!(),
                ast::Pat::RefPat(_) => todo!(),
                ast::Pat::SlicePat(_) => todo!(),
                ast::Pat::TuplePat(_) => todo!(),
                ast::Pat::TupleStructPat(_) => todo!(),
                ast::Pat::ConstBlockPat(_) => todo!(),
            };
            let ty = match param.ty().unwrap() {
                ast::Type::ArrayType(_) => todo!("ArrayType"),
                ast::Type::DynTraitType(_) => todo!("DynTraitType"),
                ast::Type::FnPtrType(_) => todo!("FnPtrType"),
                ast::Type::ForType(_) => todo!("ForType"),
                ast::Type::ImplTraitType(_) => todo!("ImplTraitType"),
                ast::Type::InferType(_) => todo!("InferType"),
                ast::Type::MacroType(_) => todo!("MacroType"),
                ast::Type::NeverType(_) => todo!("NeverType"),
                ast::Type::ParenType(_) => todo!("ParenType"),
                ast::Type::PathType(path_ty) => TyContext {},
                ast::Type::PtrType(ptr) => todo!("PtrType"),
                ast::Type::RefType(ref_ty) => todo!("RefType"),
                ast::Type::SliceType(_) => todo!("SliceType"),
                ast::Type::TupleType(_) => todo!("TupleType"),
            };

            let ty_id = self.add_node(ident.id, ty);

            args.push(ArgContext { ident, ty: ty_id });
        }

        args
    }

    fn add_node(&mut self, n: NodeId, ty: TyContext) -> TyId {
        let tid = self.types.insert(ty);
        self.type_map.insert(n, tid);
        tid
    }
}
