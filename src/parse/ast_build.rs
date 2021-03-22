use std::convert::TryInto;

use fxhash::FxHashMap;
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
pub mod token;

use error::ParseError;
use token::{self as tkn};

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

    pub fn parse_item(&mut self, item: ast::Item) -> ParseResult<tkn::Item> {
        let span = Span::new(item.syntax().text_range());

        let kind = match item {
            ast::Item::Const(c) => self.parse_const(c)?,
            ast::Item::Enum(_) => {
                // self.parse_enum(en)
                todo!()
            }
            ast::Item::ExternBlock(_) => todo!(),
            ast::Item::ExternCrate(_) => todo!(),
            ast::Item::Fn(func) => self.parse_fn(func)?,
            ast::Item::Impl(_) => {
                // self.parse_impl(imp)
                todo!()
            }
            ast::Item::MacroCall(_) => todo!("Macro stuff"),
            ast::Item::MacroRules(_) => todo!("Macro stuff"),
            ast::Item::MacroDef(_) => todo!("Macro stuff"),
            ast::Item::Module(_) => todo!("Module"),
            ast::Item::Static(_) => {
                // self.parse_static(stat)
                todo!()
            }
            ast::Item::Struct(_) => {
                // self.parse_struct(st)
                todo!()
            }
            ast::Item::Trait(_) => {
                // self.parse_trait(tr)
                todo!()
            }
            ast::Item::TypeAlias(_) => {
                // self.parse_ty_alias(ty)
                todo!()
            }
            ast::Item::Union(_) => {
                // self.parse_union(un)
                todo!()
            }
            ast::Item::Use(_) => {
                // self.parse_use(u)
                todo!()
            }
        };

        Ok(tkn::Item { kind, span })
    }

    pub fn parse_const(&mut self, const_: ast::Const) -> ParseResult<tkn::ItemKind> {
        let span = Span::new(const_.syntax().text_range());

        Ok(tkn::ItemKind::Const(
            self.parse_ty(const_.ty().ok_or(ParseError::IncorrectToken)?)?,
            self.parse_expr(const_.body().ok_or(ParseError::IncorrectToken)?)?,
        ))
    }

    pub fn parse_fn(&mut self, func: ast::Fn) -> ParseResult<tkn::ItemKind> {
        let span = Span::new(func.syntax().text_range());

        let func = tkn::FnKind {
            block: self.parse_block(func.body().unwrap())?,
            gen: self.parse_gen_params(func.generic_param_list())?,
            sig: self.parse_sig(&func)?,
        };

        Ok(tkn::ItemKind::Fn(func))
    }

    pub fn parse_block(&mut self, blk: ast::BlockExpr) -> ParseResult<tkn::Block> {
        let span = Span::new(blk.syntax().text_range());

        let mut stmts = vec![];
        for stmt in blk.statements() {
            stmts.push(self.parse_stmt(stmt)?);
        }
        Ok(tkn::Block { stmts, span })
    }

    pub fn parse_sig(&mut self, func: &ast::Fn) -> ParseResult<tkn::FnSig> {
        let sig = tkn::FnSig {
            inputs: self.parse_params(func.param_list())?,
            ret: if let Some(ret) = func.ret_type() {
                tkn::FnReturn::Explicit(Box::new(self.parse_ty(ret.ty().unwrap())?))
            } else {
                tkn::FnReturn::None
            },
        };

        Ok(sig)
    }

    pub fn parse_gen_params(&mut self, func: Option<ast::GenericParamList>) -> ParseResult<()> {
        Ok(())
    }

    pub fn parse_params(&mut self, params: Option<ast::ParamList>) -> ParseResult<Vec<tkn::Param>> {
        let mut args = vec![];
        for param in params.into_iter().flat_map(|p| p.params()) {
            let span = Span::new(param.syntax().text_range());
            args.push(tkn::Param {
                pat: self.parse_pat(param.pat().unwrap())?,
                ty: self.parse_ty(param.ty().unwrap())?,
                span,
            })
        }
        Ok(args)
    }

    pub fn parse_stmt(&mut self, stmt: ast::Stmt) -> ParseResult<tkn::Stmt> {
        let span = Span::new(stmt.syntax().text_range());
        let kind = match stmt {
            ast::Stmt::ExprStmt(expr) => {
                if expr.semicolon_token().is_some() {
                    tkn::StmtKind::Semi(Box::new(self.parse_expr(expr.expr().unwrap())?))
                } else {
                    tkn::StmtKind::Expr(Box::new(self.parse_expr(expr.expr().unwrap())?))
                }
            }
            ast::Stmt::Item(it) => tkn::StmtKind::Item(Box::new(self.parse_item(it)?)),
            ast::Stmt::LetStmt(stmt) => {
                if stmt.let_token().is_some() {
                    tkn::StmtKind::Local(Box::new(self.parse_local(stmt)?))
                } else {
                    todo!("Let stmt without let??{:?}", stmt)
                }
            }
        };
        Ok(tkn::Stmt { kind, span })
    }

    pub fn parse_local(&mut self, stmt: ast::LetStmt) -> ParseResult<tkn::Local> {
        let span = Span::new(stmt.syntax().text_range());
        Ok(tkn::Local {
            pat: self.parse_pat(stmt.pat().unwrap())?,
            ty: stmt.ty().map(|ty| self.parse_ty(ty)).transpose()?,
            init: if let Some(ex) = stmt.initializer() {
                Some(self.parse_expr(ex)?)
            } else {
                None
            },
            span,
        })
    }

    pub fn parse_pat(&mut self, pat: ast::Pat) -> ParseResult<tkn::Pat> {
        let span = Span::new(pat.syntax().text_range());
        let kind = match pat {
            ast::Pat::IdentPat(id) => tkn::PatKind::Ident {
                id: Ident::new(
                    Span::new(id.syntax().text_range()),
                    id.name().unwrap().to_string(),
                ),
                bind: if id.ref_token().is_some() {
                    tkn::BindingMode::Ref {
                        mutable: id.mut_token().is_some(),
                    }
                } else {
                    tkn::BindingMode::Value {
                        mutable: id.mut_token().is_some(),
                    }
                },
                sub: id
                    .pat()
                    .map(|p| self.parse_pat(p).map(Box::new))
                    .transpose()?,
            },
            ast::Pat::BoxPat(_) => todo!(),
            ast::Pat::RestPat(_) => tkn::PatKind::Rest,
            ast::Pat::LiteralPat(lit) => tkn::PatKind::Lit(Box::new(
                self.parse_expr(ast::Expr::Literal(lit.literal().unwrap()))?,
            )),
            ast::Pat::MacroPat(_) => todo!(),
            ast::Pat::OrPat(or) => tkn::PatKind::Or(
                or.pats()
                    .map(|p| self.parse_pat(p))
                    .collect::<ParseResult<Vec<_>>>()?,
            ),
            ast::Pat::ParenPat(paren) => {
                tkn::PatKind::Paren(Box::new(self.parse_pat(paren.pat().unwrap())?))
            }
            ast::Pat::PathPat(path) => tkn::PatKind::Path(self.parse_pat_path(&path)?),
            ast::Pat::WildcardPat(_) => tkn::PatKind::Wild,
            ast::Pat::RangePat(rng) => {
                todo!("{:?}", rng)
            }
            ast::Pat::RecordPat(rec) => todo!("{:?}", rec),
            ast::Pat::RefPat(r_pat) => tkn::PatKind::Ref {
                mutable: r_pat.mut_token().is_some(),
                pat: Box::new(self.parse_pat(r_pat.pat().unwrap())?),
            },
            ast::Pat::SlicePat(slice) => tkn::PatKind::Slice(
                slice
                    .pats()
                    .map(|p| self.parse_pat(p))
                    .collect::<ParseResult<Vec<_>>>()?,
            ),
            ast::Pat::TuplePat(tup) => tkn::PatKind::Tuple(
                tup.fields()
                    .map(|p| self.parse_pat(p))
                    .collect::<ParseResult<Vec<_>>>()?,
            ),
            ast::Pat::TupleStructPat(tup_struct) => {
                let p = tup_struct.path().unwrap();
                let path_span = Span::new(p.syntax().text_range());
                let seg = vec![Ident::new(
                    path_span.clone(),
                    p.segment().unwrap().to_string(),
                )];
                tkn::PatKind::TupleStruct {
                    name: tkn::Path {
                        seg,
                        span: path_span,
                    },
                    data: tup_struct
                        .fields()
                        .map(|p| self.parse_pat(p))
                        .collect::<ParseResult<Vec<_>>>()?,
                }
            }
            ast::Pat::ConstBlockPat(c) => {
                todo!("What is this const block patter {:?}", c)
            }
        };

        Ok(tkn::Pat { kind, span })
    }

    pub fn parse_ty(&self, ty: ast::Type) -> ParseResult<tkn::Ty> {
        let span = Span::new(ty.syntax().text_range());
        let kind = Box::new(match ty {
            ast::Type::ArrayType(arr) => tkn::TyKind::Arr(
                Box::new(self.parse_ty(arr.ty().ok_or(ParseError::IncorrectToken)?)?),
                self.parse_expr(arr.expr().ok_or(ParseError::IncorrectToken)?)?,
            ),
            ast::Type::DynTraitType(_) => todo!("DynTraitType"),
            ast::Type::FnPtrType(func) => todo!("FnPtr"),
            ast::Type::ForType(for_ty) => todo!("ForType"),
            ast::Type::ImplTraitType(_) => todo!("ImplTrait"),
            ast::Type::InferType(inf) => todo!("Infer"),
            ast::Type::MacroType(_) => todo!("Macros yikes"),
            ast::Type::NeverType(_) => tkn::TyKind::Never,
            ast::Type::ParenType(paren) => tkn::TyKind::Paren(Box::new(
                self.parse_ty(paren.ty().ok_or(ParseError::IncorrectToken)?)?,
            )),
            ast::Type::PathType(path) => tkn::TyKind::Path {
                qualified: self.parse_qualified(&path)?,
                p: self.parse_path(&path)?,
            },
            ast::Type::PtrType(ptr_ty) => tkn::TyKind::RawPtr(tkn::MutTy {
                mutable: ptr_ty.mut_token().is_some(),
                ty: Box::new(self.parse_ty(ptr_ty.ty().unwrap())?),
                span: Span::new(ptr_ty.syntax().text_range()),
            }),
            ast::Type::RefType(reference) => tkn::TyKind::Ref(
                self.parse_lifetime(reference.lifetime().as_ref())?,
                tkn::MutTy {
                    mutable: reference.mut_token().is_some(),
                    ty: Box::new(self.parse_ty(reference.ty().unwrap())?),
                    span: Span::new(reference.syntax().text_range()),
                },
            ),
            ast::Type::SliceType(slice) => {
                tkn::TyKind::Slice(Box::new(self.parse_ty(slice.ty().unwrap())?))
            }
            ast::Type::TupleType(tup) => tkn::TyKind::Tup(
                tup.fields()
                    .map(|t| self.parse_ty(t))
                    .collect::<ParseResult<Vec<_>>>()?,
            ),
        });
        Ok(tkn::Ty { kind, span })
    }

    pub fn parse_expr(&self, expr: ast::Expr) -> ParseResult<tkn::Expr> {
        let span = Span::new(expr.syntax().text_range());
        let kind = match expr {
            ast::Expr::ArrayExpr(arr) => todo!(),
            ast::Expr::AwaitExpr(_) => todo!(),
            ast::Expr::BinExpr(binop) => {
                let op = tkn::BinOp {
                    kind: match binop.op_kind().unwrap() {
                        ast::BinOp::BooleanOr => tkn::BinOpKind::Or,
                        ast::BinOp::BooleanAnd => tkn::BinOpKind::And,
                        ast::BinOp::EqualityTest => tkn::BinOpKind::Eq,
                        ast::BinOp::NegatedEqualityTest => tkn::BinOpKind::Ne,
                        ast::BinOp::LesserEqualTest => tkn::BinOpKind::Le,
                        ast::BinOp::GreaterEqualTest => tkn::BinOpKind::Ge,
                        ast::BinOp::LesserTest => tkn::BinOpKind::Lt,
                        ast::BinOp::GreaterTest => tkn::BinOpKind::Gt,
                        ast::BinOp::Addition => tkn::BinOpKind::Add,
                        ast::BinOp::Multiplication => tkn::BinOpKind::Mul,
                        ast::BinOp::Subtraction => tkn::BinOpKind::Sub,
                        ast::BinOp::Division => tkn::BinOpKind::Div,
                        ast::BinOp::Remainder => tkn::BinOpKind::Rem,
                        ast::BinOp::LeftShift => tkn::BinOpKind::Shl,
                        ast::BinOp::RightShift => tkn::BinOpKind::Shr,
                        ast::BinOp::BitwiseXor => tkn::BinOpKind::BitXor,
                        ast::BinOp::BitwiseOr => tkn::BinOpKind::BitOr,
                        ast::BinOp::BitwiseAnd => tkn::BinOpKind::BitAnd,
                        ast::BinOp::Assignment => {
                            // FIXME: this isn't ideal
                            // We have to short circuit to get around lex/parse differences
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::Assign {
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::AddAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Add,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::DivAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Div,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::MulAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Mul,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::RemAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Rem,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::ShrAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Shr,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::ShlAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Shl,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::SubAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::Sub,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::BitOrAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::BitOr,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::BitAndAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::BitAnd,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                        ast::BinOp::BitXorAssign => {
                            return Ok(tkn::Expr {
                                kind: tkn::ExprKind::AssignOp {
                                    op: tkn::BinOp {
                                        kind: tkn::BinOpKind::BitXor,
                                        span: Span::new(binop.op_token().unwrap().text_range()),
                                    },
                                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                                },
                                span,
                            });
                        }
                    },
                    span: Span::new(binop.op_token().unwrap().text_range()),
                };
                tkn::ExprKind::Binary {
                    lhs: Box::new(self.parse_expr(binop.lhs().unwrap())?),
                    rhs: Box::new(self.parse_expr(binop.rhs().unwrap())?),
                    op,
                }
            }
            ast::Expr::BlockExpr(_) => todo!(),
            ast::Expr::BoxExpr(_) => todo!(),
            ast::Expr::BreakExpr(_) => todo!(),
            ast::Expr::CallExpr(_) => todo!(),
            ast::Expr::CastExpr(_) => todo!(),
            ast::Expr::ClosureExpr(_) => todo!(),
            ast::Expr::ContinueExpr(_) => todo!(),
            ast::Expr::EffectExpr(_) => todo!(),
            ast::Expr::FieldExpr(_) => todo!(),
            ast::Expr::ForExpr(_) => todo!(),
            ast::Expr::IfExpr(_) => todo!(),
            ast::Expr::IndexExpr(_) => todo!(),
            ast::Expr::Literal(lit) => {
                let kind = match lit.kind() {
                    ast::LiteralKind::String(s) => {
                        tkn::LitKind::Str(s.value().unwrap().to_string(), tkn::StrStyle::Cooked)
                    }
                    ast::LiteralKind::ByteString(bs) => {
                        tkn::LitKind::ByteStr(bs.to_string().into_bytes().into_boxed_slice())
                    }
                    ast::LiteralKind::IntNumber(int) => {
                        let val = int.value().unwrap();
                        let kind = if let Some(suf) = int.suffix() {
                            match suf {
                                "u8" => tkn::LitIntType::Unsigned(tkn::UintTy::U8),
                                "u16" => tkn::LitIntType::Unsigned(tkn::UintTy::U16),
                                "u32" => tkn::LitIntType::Unsigned(tkn::UintTy::U32),
                                "u64" => tkn::LitIntType::Unsigned(tkn::UintTy::U64),
                                "u128" => tkn::LitIntType::Unsigned(tkn::UintTy::U128),
                                "usize" => tkn::LitIntType::Unsigned(tkn::UintTy::Usize),
                                "i8" => tkn::LitIntType::Signed(tkn::IntTy::I8),
                                "i16" => tkn::LitIntType::Signed(tkn::IntTy::I16),
                                "i32" => tkn::LitIntType::Signed(tkn::IntTy::I32),
                                "i64" => tkn::LitIntType::Signed(tkn::IntTy::I64),
                                "i128" => tkn::LitIntType::Signed(tkn::IntTy::I128),
                                "isize" => tkn::LitIntType::Signed(tkn::IntTy::Isize),
                                _ => panic!("Invalid number type"),
                            }
                        } else {
                            tkn::LitIntType::Unsuffixed
                        };

                        tkn::LitKind::Int(val, kind)
                    }
                    ast::LiteralKind::FloatNumber(f) => {
                        let kind = if let Some(suf) = f.suffix() {
                            match suf {
                                "f32" => tkn::LitFloatType::Suffixed(tkn::FloatTy::F32),
                                "f64" => tkn::LitFloatType::Suffixed(tkn::FloatTy::F64),
                                _ => panic!("Invalid float type"),
                            }
                        } else {
                            tkn::LitFloatType::Unsuffixed
                        };

                        tkn::LitKind::Float(f.to_string(), kind)
                    }
                    ast::LiteralKind::Char => {
                        tkn::LitKind::Char(lit.to_string().chars().next().unwrap())
                    }
                    ast::LiteralKind::Byte => {
                        tkn::LitKind::Byte(lit.to_string().bytes().next().unwrap())
                    }
                    ast::LiteralKind::Bool(b) => tkn::LitKind::Bool(b),
                };
                tkn::ExprKind::Lit(tkn::Literal {
                    kind,
                    span: Span::new(lit.syntax().text_range()),
                })
            }
            ast::Expr::LoopExpr(_) => todo!(),
            ast::Expr::MacroCall(_) => todo!(),
            ast::Expr::MacroStmts(_) => todo!(),
            ast::Expr::MatchExpr(_) => todo!(),
            ast::Expr::MethodCallExpr(_) => todo!(),
            ast::Expr::ParenExpr(_) => todo!(),
            ast::Expr::PathExpr(path) => {
                let path = path.path().unwrap();
                let path_span = Span::new(path.syntax().text_range());
                let seg = vec![Ident::new(
                    path_span.clone(),
                    path.segment().unwrap().to_string(),
                )];
                tkn::ExprKind::Path(tkn::Path {
                    seg,
                    span: path_span,
                })
            }
            ast::Expr::PrefixExpr(ex) => {
                let op = match ex.op_kind().unwrap() {
                    ast::PrefixOp::Deref => tkn::UnOp::Deref,
                    ast::PrefixOp::Not => tkn::UnOp::Not,
                    ast::PrefixOp::Neg => tkn::UnOp::Neg,
                };
                tkn::ExprKind::Unary {
                    op,
                    lhs: Box::new(self.parse_expr(ex.expr().unwrap())?),
                }
            }
            ast::Expr::RangeExpr(_) => todo!(),
            ast::Expr::RecordExpr(_) => todo!(),
            ast::Expr::RefExpr(_) => todo!(),
            ast::Expr::ReturnExpr(_) => todo!(),
            ast::Expr::TryExpr(_) => todo!(),
            ast::Expr::TupleExpr(_) => todo!(),
            ast::Expr::WhileExpr(_) => todo!(),
            ast::Expr::YieldExpr(_) => todo!(),
        };

        Ok(tkn::Expr { kind, span })
    }

    pub fn parse_qualified(&self, path: &ast::PathType) -> ParseResult<tkn::Qualified> {
        Ok(if let Some(qual) = path.path().unwrap().qualifier() {
            tkn::Qualified::Type {
                ty: todo!("{:#?}", qual),
                path_span: Span::new(qual.syntax().text_range()),
                pos: 0,
            }
        } else {
            token::Qualified::Empty
        })
    }

    pub fn parse_pat_path(&self, path: &ast::PathPat) -> ParseResult<tkn::Path> {
        let mut seg = vec![];
        let mut p = path.path();
        while let Some(pa) = p {
            seg.push(Ident::new(
                Span::new(pa.syntax().text_range()),
                pa.to_string(),
            ));
            p = pa
                .segment()
                .and_then(|p| p.path_type())
                .and_then(|p| p.path());
        }
        Ok(tkn::Path {
            seg,
            span: Span::new(path.syntax().text_range()),
        })
    }

    pub fn parse_path(&self, path: &ast::PathType) -> ParseResult<tkn::Path> {
        let seg = self.make_seg(&path)?;
        Ok(tkn::Path {
            seg,
            span: Span::new(path.syntax().text_range()),
        })
    }

    pub fn parse_lifetime(&self, lifetime: Option<&ast::Lifetime>) -> ParseResult<tkn::Lifetime> {
        Ok(if let Some(lt) = lifetime {
            if lt.lifetime_ident_token().map(|t| t.kind()) == Some(T![_]) {
                tkn::Lifetime::Anon
            } else {
                tkn::Lifetime::Named(Ident::new(
                    Span::new(lt.lifetime_ident_token().unwrap().text_range()),
                    lt.to_string(),
                ))
            }
        } else {
            tkn::Lifetime::None
        })
    }

    fn make_seg(&self, path: &ast::PathType) -> ParseResult<Vec<Ident>> {
        let mut ids = vec![];
        let mut p = path.path();
        while let Some(pa) = p {
            ids.push(Ident::new(
                Span::new(pa.syntax().text_range()),
                pa.to_string(),
            ));
            p = pa
                .segment()
                .and_then(|p| p.path_type())
                .and_then(|p| p.path());
        }
        Ok(ids)
    }
}
