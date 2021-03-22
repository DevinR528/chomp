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
    pub fn new<T: AstNode>(node: &T) -> Self {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    id: NodeId,
}

#[derive(Debug)]
pub enum Stmt {
    Local(),
    Expr(),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum TyKind {
    Slice(Box<TyKind>),
    Arr(Box<TyKind>, NodeId),
    Ref(Lifetime, MutTy),
    FnPtr(Box<BareFn>),
    Tup(Vec<TyKind>),
    Path {
        qualified: Qualified,
        p: Path,
    },
    BuiltIn(BuiltInKind),
    TraitObj,
    ImplTrait,
    Paren(Box<TyKind>),
    RawPtr,
    /// The ! type.
    Never,
    /// The () type.
    ///
    /// Rust version of null sort of...
    Empty,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum BuiltInKind {
    Str,
    ByteStr,
    Char,
    Byte,
    F32,
    F64,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    Bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Lifetime {
    Anon,
    Named(Ident),
    None,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct BareFn {}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct MutTy {
    pub ty: Box<TyKind>,
    pub mutable: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Path {
    pub seg: Vec<Ident>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Qualified {
    Empty,
    Type { ty: Path, pos: usize },
}

#[derive(Debug)]
pub struct TyContext {
    ty: TyKind,
}

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
    node_type: FxHashMap<NodeId, TyId>,
    ident_node: FxHashMap<Ident, NodeId>,
    kind: ScopeKind,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            vis: FxHashSet::default(),
            node_type: FxHashMap::default(),
            ident_node: FxHashMap::default(),
            kind,
        }
    }

    pub fn add_node(&mut self, n: NodeId, ty: TyId) {
        self.vis.insert(n);
        self.node_type.insert(n, ty);
    }

    pub fn add_unknown_node(&mut self, n: NodeId) {
        self.vis.insert(n);
        self.node_type
            .insert(n, TyId::Unknown(self.node_type.len()));
    }

    pub fn add_ident(&mut self, id: Ident, nid: NodeId) {
        self.ident_node.insert(id, nid);
    }
}

#[derive(Debug)]
pub struct TypeResolver {
    types: TypeIntern,
    items: FxHashMap<ItemId, ItemContext>,
    mod_scope: Scope,
    interactions: FxHashMap<NodeId, Vec<NodeId>>,
}

impl TypeResolver {
    fn new(mod_scope: Scope) -> Self {
        Self {
            types: TypeIntern::default(),
            items: FxHashMap::default(),
            mod_scope,
            interactions: FxHashMap::default(),
        }
    }

    pub fn resolve(items: &[ast::Item]) -> Self {
        let mut resolver = Self::new(Scope::new(ScopeKind::Mod));
        let mut scope = Scope::new(ScopeKind::Mod);

        for item in items {
            resolver.alloc_item(&mut scope, item);
        }
        // ugly hacky way to fill module def map
        resolver.mod_scope = scope;

        resolver
    }

    fn alloc_item(&mut self, scope: &mut Scope, item: &ast::Item) {
        match item {
            ast::Item::Const(_) => {}
            ast::Item::Enum(_) => {}
            ast::Item::ExternBlock(_) => {}
            ast::Item::ExternCrate(_) => {}
            ast::Item::Fn(func) => self.alloc_func(scope, func),
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

    pub fn alloc_func(&mut self, mod_scope: &mut Scope, func: &ast::Fn) {
        let id = ItemId::new(func);
        let nid = NodeId::new(func);

        let mut scope = Scope::new(ScopeKind::Function);
        let args = self.alloc_params(&mut scope, func.param_list().unwrap());
        let body = self.alloc_body(&mut scope, func.body().unwrap());
        let ret = if let Some(ret) = func.ret_type().and_then(|t| t.ty()) {
            self.alloc_ty(ret)
        } else {
            TyId::Default
        };

        // Add the function name and type for call Exprs
        mod_scope.add_node(nid, ret);

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
        let stmts = vec![];

        for stmt in blk.statements() {
            match stmt {
                ast::Stmt::ExprStmt(expr) => {
                    self.alloc_expr(scope, expr.expr().unwrap());
                }
                ast::Stmt::Item(item) => self.alloc_item(scope, &item),
                ast::Stmt::LetStmt(stmt) => self.alloc_let(scope, stmt),
            }
        }

        BodyContext { stmts }
    }

    pub fn alloc_params(&mut self, scope: &mut Scope, params: ast::ParamList) -> Vec<ArgContext> {
        let mut args = vec![];

        if let Some(this) = params.self_param() {
            todo!();
            scope.vis.insert(NodeId(0));
        }

        for param in params.params() {
            let ident = self.alloc_pat(param.pat().unwrap());
            let ty = self.alloc_ty(param.ty().unwrap());
            scope.add_node(ident.id, ty);
            scope.add_ident(ident, ident.id);

            args.push(ArgContext { ident, ty });
        }

        args
    }

    pub fn alloc_let(&mut self, scope: &mut Scope, loc: ast::LetStmt) {
        let ident = self.alloc_pat(loc.pat().unwrap());
        let nid = self.alloc_expr(scope, loc.initializer().unwrap());
        scope.add_ident(ident, nid);

        if let Some(t) = loc.ty() {
            let tid = self.alloc_ty(t);
            scope.add_node(nid, tid);
        } else {
            scope.add_unknown_node(nid)
        }
    }

    pub fn alloc_expr(&mut self, scope: &mut Scope, expr: ast::Expr) -> NodeId {
        match expr {
            ast::Expr::ArrayExpr(_) => todo!("ArrayExpr"),
            ast::Expr::AwaitExpr(_) => todo!("AwaitExpr"),
            ast::Expr::BinExpr(_) => todo!("BinExpr"),
            ast::Expr::BlockExpr(_) => todo!("BlockExpr"),
            ast::Expr::BoxExpr(_) => todo!("BoxExpr"),
            ast::Expr::BreakExpr(_) => todo!("BreakExpr"),
            ast::Expr::CallExpr(_) => todo!("CallExpr"),
            ast::Expr::CastExpr(_) => todo!("CastExpr"),
            ast::Expr::ClosureExpr(_) => todo!("ClosureExpr"),
            ast::Expr::ContinueExpr(_) => todo!("ContinueExpr"),
            ast::Expr::EffectExpr(_) => todo!("EffectExpr"),
            ast::Expr::FieldExpr(_) => todo!("FieldExpr"),
            ast::Expr::ForExpr(_) => todo!("ForExpr"),
            ast::Expr::IfExpr(_) => todo!("IfExpr"),
            ast::Expr::IndexExpr(_) => todo!("IndexExpr"),
            ast::Expr::Literal(lit) => {
                let nid = NodeId::new(&lit);

                match lit.kind() {
                    ast::LiteralKind::String(s) => {
                        let tyid = self.types.insert(TyContext {
                            ty: TyKind::BuiltIn(BuiltInKind::Str),
                        });
                        scope.add_node(nid, tyid);
                    }
                    ast::LiteralKind::ByteString(bs) => todo!(),
                    ast::LiteralKind::IntNumber(int) => {
                        let val = int.value().unwrap();
                        if let Some(suf) = int.suffix() {
                            let ty = match suf {
                                "u8" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::U8),
                                },
                                "u16" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::U16),
                                },
                                "u32" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::U32),
                                },
                                "u64" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::U64),
                                },
                                "u128" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::U128),
                                },
                                "usize" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::Usize),
                                },
                                "i8" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::I8),
                                },
                                "i16" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::I16),
                                },
                                "i32" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::I32),
                                },
                                "i64" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::I64),
                                },
                                "i128" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::I128),
                                },
                                "isize" => TyContext {
                                    ty: TyKind::BuiltIn(BuiltInKind::Isize),
                                },
                                _ => panic!("Invalid number type"),
                            };
                            let tyid = self.types.insert(ty);
                            scope.add_node(nid, tyid);
                        } else {
                            scope.add_unknown_node(nid);
                        };
                    }
                    ast::LiteralKind::FloatNumber(f) => {
                        let kind = if let Some(suf) = f.suffix() {
                            match suf {
                                "f32" => todo!(),
                                "f64" => todo!(),
                                _ => panic!("Invalid float type"),
                            }
                        } else {
                            scope.add_unknown_node(nid);
                        };
                    }
                    ast::LiteralKind::Char => {
                        todo!()
                    }
                    ast::LiteralKind::Byte => {
                        todo!()
                    }
                    ast::LiteralKind::Bool(b) => {
                        let tyid = self.types.insert(TyContext {
                            ty: TyKind::BuiltIn(BuiltInKind::Bool),
                        });
                        scope.add_node(nid, tyid);
                    }
                };
                nid
            }
            ast::Expr::LoopExpr(_) => todo!("LoopExpr"),
            ast::Expr::MacroCall(_) => todo!("MacroCall"),
            ast::Expr::MacroStmts(_) => todo!("MacroStmts"),
            ast::Expr::MatchExpr(_) => todo!("MatchExpr"),
            ast::Expr::MethodCallExpr(_) => todo!("MethodCallExpr"),
            ast::Expr::ParenExpr(_) => todo!("ParenExpr"),
            ast::Expr::PathExpr(_) => todo!("PathExpr"),
            ast::Expr::PrefixExpr(_) => todo!("PrefixExpr"),
            ast::Expr::RangeExpr(_) => todo!("RangeExpr"),
            ast::Expr::RecordExpr(_) => todo!("RecordExpr"),
            ast::Expr::RefExpr(_) => todo!("RefExpr"),
            ast::Expr::ReturnExpr(_) => todo!("ReturnExpr"),
            ast::Expr::TryExpr(_) => todo!("TryExpr"),
            ast::Expr::TupleExpr(_) => todo!("TupleExpr"),
            ast::Expr::WhileExpr(_) => todo!("WhileExpr"),
            ast::Expr::YieldExpr(_) => todo!("YieldExpr"),
        }
    }

    fn alloc_ty(&mut self, ty: ast::Type) -> TyId {
        match ty {
            ast::Type::ArrayType(_) => todo!("ArrayType"),
            ast::Type::DynTraitType(_) => todo!("DynTraitType"),
            ast::Type::FnPtrType(_) => todo!("FnPtrType"),
            ast::Type::ForType(_) => todo!("ForType"),
            ast::Type::ImplTraitType(_) => todo!("ImplTraitType"),
            ast::Type::InferType(_) => todo!("InferType"),
            ast::Type::MacroType(_) => todo!("MacroType"),
            ast::Type::NeverType(_) => todo!("NeverType"),
            ast::Type::ParenType(_) => todo!("ParenType"),
            ast::Type::PathType(path_ty) => {
                let p = path_ty;
                self.types.insert(TyContext { ty: todo!() })
            }
            ast::Type::PtrType(ptr) => todo!("PtrType"),
            ast::Type::RefType(ref_ty) => todo!("RefType"),
            ast::Type::SliceType(_) => todo!("SliceType"),
            ast::Type::TupleType(_) => todo!("TupleType"),
        }
    }

    fn alloc_pat(&mut self, pat: ast::Pat) -> Ident {
        match pat {
            ast::Pat::IdentPat(ident) => {
                let id = NodeId::new(&ident);
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
        }
    }
}
