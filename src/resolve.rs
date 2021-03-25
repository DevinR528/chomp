use std::{
    fmt,
    hash::{Hash, Hasher},
};

use fxhash::{FxHashMap, FxHashSet, FxHasher64};
use syntax::{
    ast::{
        self, ArgListOwner, AstNode, AttrsOwner, GenericParamsOwner, LoopBodyOwner,
        ModuleItemOwner, NameOwner, TypeBoundsOwner,
    },
    SyntaxNode,
};

mod fir;
mod intern;

use intern::{TyId, TypeIntern};

pub type ScopeId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(usize);

impl NodeId {
    pub fn new<T: AstNode>(node: &T) -> Self {
        let mut hasher = FxHasher64::default();
        node.syntax().hash(&mut hasher);
        Self(hasher.finish() as usize)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(usize);

impl ItemId {
    pub fn new<T: AstNode>(node: &T) -> Self {
        let mut hasher = FxHasher64::default();
        node.syntax().hash(&mut hasher);
        Self(hasher.finish() as usize)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    rep: String,
}

impl Ident {
    pub fn new(rep: String) -> Self {
        Self { rep }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Local(Ident, Scope),
    Expr(),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct TyContext {
    ty: TyKind,
}

impl fmt::Display for TyContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.ty))
    }
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

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            TyKind::Slice(ty) => format!("[{}]", ty),
            TyKind::Arr(ty, _) => format!("[{}]", ty),
            TyKind::Ref(lt, mut_ty) => format!(
                "&{}{}",
                match lt {
                    Lifetime::Anon => "`_".to_string(),
                    Lifetime::Named(id) => id.rep.to_string(),
                    Lifetime::None => "".to_string(),
                },
                if mut_ty.mutable {
                    format!("mut {}", mut_ty.ty)
                } else {
                    format!("{}", mut_ty.ty)
                }
            ),
            TyKind::FnPtr(bare) => format!("fn({:?})", bare),
            TyKind::Tup(tup) => {
                let mut s = "(".to_string();
                for ty in tup {
                    s += &format!("{}, ", ty);
                }
                s.truncate(s.len() - 2);
                s.push(')');
                s
            }
            TyKind::Path { qualified, p } => p.seg.iter().fold(String::new(), |mut s, id| {
                s.push_str(&id.rep);
                s
            }),
            TyKind::BuiltIn(bt) => bt.to_string(),
            TyKind::TraitObj => "Trait Obj".to_string(),
            TyKind::ImplTrait => "Impl Trait".to_string(),
            TyKind::Paren(paren) => format!("({})", paren),
            TyKind::RawPtr => "Raw Pointer".to_string(),
            TyKind::Never => "!".to_string(),
            TyKind::Empty => "()".to_string(),
        };
        f.write_str(&string)
    }
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
impl fmt::Display for BuiltInKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BuiltInKind::Str => "str",
                BuiltInKind::ByteStr => "byte str",
                BuiltInKind::Char => "char",
                BuiltInKind::Byte => "byte",
                BuiltInKind::F32 => "f32",
                BuiltInKind::F64 => "f64",
                BuiltInKind::U8 => "u8",
                BuiltInKind::U16 => "u16",
                BuiltInKind::U32 => "u32",
                BuiltInKind::U64 => "u64",
                BuiltInKind::U128 => "u128",
                BuiltInKind::Usize => "usize",
                BuiltInKind::I8 => "i8",
                BuiltInKind::I16 => "i16",
                BuiltInKind::I32 => "i32",
                BuiltInKind::I64 => "i64",
                BuiltInKind::I128 => "i128",
                BuiltInKind::Isize => "isize",
                BuiltInKind::Bool => "bool",
            }
        )
    }
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

#[derive(Debug, Clone)]
pub struct Scope {
    vis: Vec<NodeId>,
    node_type: FxHashMap<NodeId, TyId>,
    ident_node: FxHashMap<Ident, NodeId>,
    kind: ScopeKind,
    sub: Vec<Scope>,
    ident_refs: FxHashMap<Ident, NodeId>,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            vis: Vec::default(),
            node_type: FxHashMap::default(),
            ident_node: FxHashMap::default(),
            kind,
            sub: vec![],
            ident_refs: FxHashMap::default(),
        }
    }

    pub fn add_node(&mut self, n: NodeId, ty: TyId) {
        if !self.vis.contains(&n) {
            self.vis.push(n);
        }
        self.node_type.insert(n, ty);
    }

    pub fn add_unknown_node(&mut self, n: NodeId) {
        if !self.vis.contains(&n) {
            self.vis.push(n);
        }
        self.node_type
            .insert(n, TyId::Unknown(self.node_type.len()));
    }

    pub fn add_ident(&mut self, id: Ident, nid: NodeId) {
        self.ident_node.insert(id, nid);
    }

    pub fn add_ident_ref(&mut self, id: Ident, nid: NodeId) {
        self.ident_refs.insert(id, nid);
    }

    pub fn add_sub_scope(&mut self, scope: Scope) {
        self.sub.push(scope)
    }
}

pub struct TypeResolver {
    types: TypeIntern,
    items: FxHashMap<ItemId, ItemContext>,
    mod_scope: Scope,
    nodes: FxHashMap<NodeId, SyntaxNode>,
}

impl fmt::Debug for TypeResolver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeResolver")
            .field("types", &self.types)
            .field("items", &self.items)
            .field("mod_scope", &self.mod_scope)
            .field("nodes", &format!("{:?}", self.nodes))
            // .field("nodes", &self.nodes)
            .finish()
    }
}

impl TypeResolver {
    fn new(mod_scope: Scope) -> Self {
        Self {
            types: TypeIntern::default(),
            items: FxHashMap::default(),
            mod_scope,
            nodes: FxHashMap::default(),
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

    pub fn unify(&self) {
        fn _unify(
            scope: &Scope,
            parent: Option<&Scope>,
            init_ty: Option<TyId>,
            types: &TypeIntern,
        ) -> Option<TyId> {
            let mut ty = init_ty;
            for sub in &scope.sub {
                ty = _unify(sub, Some(&scope), ty, types);
            }

            let mut node_ty = init_ty;
            for (node, ty) in &scope.node_type {
                node_ty = cheap_unify(Some(*ty), node_ty, types);
            }

            cheap_unify(ty, node_ty, types)
        }

        fn _name_res(
            scope: &Scope,
            parent: Option<&Scope>,
            idents: &FxHashMap<Ident, Option<TyId>>,
        ) -> Option<TyId> {
            for sub in &scope.sub {
                if let Some(ty) = _name_res(sub, Some(&scope), idents) {
                    return Some(ty);
                }
            }

            for id in scope.ident_refs.keys() {
                if let Some((_, ty)) = idents.iter().find(|(i, _)| i == &id) {
                    return ty.map(|t| t);
                }
            }

            None
        }

        let mut idents = FxHashMap::default();

        for (id, item) in &self.items {
            match &item.kind {
                ItemKind::Fn(func) => {
                    for stmt in &func.body.stmts {
                        match stmt {
                            Stmt::Local(ident, scope) => {
                                let ty = _name_res(scope, Some(&self.mod_scope), &idents);
                                idents.insert(
                                    ident.clone(),
                                    _unify(scope, Some(&self.mod_scope), ty, &self.types),
                                );
                            }
                            Stmt::Expr() => {}
                        }
                    }
                }
            }
        }
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
        let ident = Ident {
            rep: func.name().unwrap().to_string(),
        };
        mod_scope.add_ident(ident, nid);

        self.nodes.insert(nid, func.syntax().clone());
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
        let mut stmts = vec![];

        for stmt in blk.statements() {
            match stmt {
                ast::Stmt::ExprStmt(expr) => {
                    let ty = self.alloc_expr(scope, expr.expr().unwrap());
                    stmts.push(Stmt::Expr())
                }
                ast::Stmt::Item(item) => self.alloc_item(scope, &item),
                ast::Stmt::LetStmt(stmt) => {
                    let (ident, node) = self.alloc_let(scope, stmt);
                    stmts.push(Stmt::Local(ident, node));
                }
            }
        }

        BodyContext { stmts }
    }

    pub fn alloc_params(&mut self, scope: &mut Scope, params: ast::ParamList) -> Vec<ArgContext> {
        let mut args = vec![];

        if let Some(this) = params.self_param() {
            todo!();
            scope.vis.push(NodeId(0));
        }

        for param in params.params() {
            let nid = NodeId::new(&param);
            let ident = self.alloc_pat(param.pat().unwrap());
            let ty = self.alloc_ty(param.ty().unwrap());

            self.nodes.insert(nid, param.syntax().clone());

            scope.add_node(nid, ty);
            scope.add_ident(ident.clone(), nid);

            args.push(ArgContext { ident, ty });
        }

        args
    }

    pub fn alloc_let(&mut self, scope: &mut Scope, loc: ast::LetStmt) -> (Ident, Scope) {
        let nid = NodeId::new(&loc);
        let mut sub = Scope::new(ScopeKind::NormalBlock);

        let ident = self.alloc_pat(loc.pat().unwrap());
        let (expr_node, ty) = self.alloc_expr(&mut sub, loc.initializer().unwrap());

        self.nodes.insert(nid, loc.syntax().clone());

        scope.add_ident(ident.clone(), expr_node);
        scope.add_sub_scope(sub.clone());

        if let Some(t) = cheap_unify(ty, loc.ty().map(|t| self.alloc_ty(t)), &self.types) {
            scope.add_node(nid, t);
        } else {
            scope.add_unknown_node(nid);
        }

        (ident, sub)
    }

    pub fn alloc_expr(&mut self, scope: &mut Scope, expr: ast::Expr) -> (NodeId, Option<TyId>) {
        match expr {
            ast::Expr::ArrayExpr(_) => todo!("ArrayExpr"),
            ast::Expr::AwaitExpr(_) => todo!("AwaitExpr"),
            ast::Expr::BinExpr(bin) => {
                let nid = NodeId::new(&bin);
                let mut sub = Scope::new(ScopeKind::NormalBlock);

                self.nodes.insert(nid, bin.syntax().clone());

                let (_node, lhs_ty) = self.alloc_expr(&mut sub, bin.lhs().unwrap());
                let (_node, rhs_ty) = self.alloc_expr(&mut sub, bin.rhs().unwrap());

                scope.add_sub_scope(sub);
                if let Some(t) = cheap_unify(lhs_ty, rhs_ty, &self.types) {
                    scope.add_node(nid, t);
                    (nid, Some(t))
                } else {
                    scope.add_unknown_node(nid);
                    (nid, None)
                }
            }
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
                if self.nodes.contains_key(&nid) {
                    panic!("{:?}", lit);
                }
                self.nodes.insert(nid, lit.syntax().clone());

                let ty = match lit.kind() {
                    ast::LiteralKind::String(s) => {
                        let tyid = self.types.insert(TyContext {
                            ty: TyKind::BuiltIn(BuiltInKind::Str),
                        });
                        scope.add_node(nid, tyid);
                        (nid, Some(tyid))
                    }
                    ast::LiteralKind::ByteString(bs) => todo!(),
                    ast::LiteralKind::IntNumber(int) => {
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
                            (nid, Some(tyid))
                        } else {
                            scope.add_unknown_node(nid);
                            (nid, None)
                        }
                    }
                    ast::LiteralKind::FloatNumber(f) => {
                        if let Some(suf) = f.suffix() {
                            match suf {
                                "f32" => todo!(),
                                "f64" => todo!(),
                                _ => panic!("Invalid float type"),
                            }
                        } else {
                            scope.add_unknown_node(nid);
                            (nid, None)
                        }
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
                        (nid, Some(tyid))
                    }
                };
                ty
            }
            ast::Expr::LoopExpr(_) => todo!("LoopExpr"),
            ast::Expr::MacroCall(_) => todo!("MacroCall"),
            ast::Expr::MacroStmts(_) => todo!("MacroStmts"),
            ast::Expr::MatchExpr(_) => todo!("MatchExpr"),
            ast::Expr::MethodCallExpr(_) => todo!("MethodCallExpr"),
            ast::Expr::ParenExpr(_) => todo!("ParenExpr"),
            ast::Expr::PathExpr(path) => {
                let nid = NodeId::new(&path);
                self.nodes.insert(nid, path.syntax().clone());
                scope.add_unknown_node(nid);
                scope.add_ident_ref(
                    Ident::new(
                        path.path()
                            .unwrap()
                            .as_single_segment()
                            .unwrap()
                            .name_ref()
                            .unwrap()
                            .to_string(),
                    ),
                    nid,
                );
                (nid, None)
            }
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
                let name = ident.name().unwrap();
                Ident {
                    rep: name.to_string(),
                }
            }
            ast::Pat::BoxPat(_) => todo!(),
            ast::Pat::RestPat(_) => todo!(),
            ast::Pat::LiteralPat(lit) => todo!(),
            ast::Pat::MacroPat(_) => todo!(),
            ast::Pat::OrPat(_) => todo!(),
            ast::Pat::ParenPat(_) => todo!(),
            ast::Pat::PathPat(path) => todo!("{:#?}", path),
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

pub fn cheap_unify(a: Option<TyId>, b: Option<TyId>, types: &TypeIntern) -> Option<TyId> {
    match (a, b) {
        (Some(a @ TyId::T(_)), Some(b @ TyId::T(_))) => {
            if types.get(&a) == types.get(&b) {
                Some(a)
            } else {
                println!("a {:?} b {:?}", types.get(&a), types.get(&b));
                println!("a {:?} b {:?}", a, b);
                None
            }
        }
        (Some(a @ TyId::T(_)), None) => Some(a),
        (None, Some(b @ TyId::T(_))) => Some(b),
        (_, _) => None,
    }
}
