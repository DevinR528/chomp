use crate::parse::span::{Ident, Span};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum ItemKind {
    /// TODO: make it possible to compile crates together.
    Krate,
    /// TODO: same as above.
    Use,

    Static(Ty, Expr),
    Const(Ty, Expr),
    Fn(FnKind),
    Mod(Vec<Item>),
    TyAlias(Ty),
    Enum,
    Struct,
    Union,
    Trait,
    Impl,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum ExprKind {
    Arr(Vec<Expr>),
    ConstBlk(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    MethodCall(Path, Vec<Expr>),
    Tup(Vec<Expr>),
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnOp,
        lhs: Box<Expr>,
    },
    Lit(Literal),
    If {
        ifexpr: Box<Expr>,
        blk: Box<Block>,
        els: Option<Box<Expr>>,
    },
    While {
        cond: Box<Expr>,
        blk: Box<Block>,
        label: Option<Ident>,
    },
    For {
        p: Box<Pat>,
        expr: Box<Expr>,
        blk: Box<Block>,
        label: Option<Ident>,
    },
    Loop {
        blk: Box<Block>,
        label: Option<Ident>,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<Arm>,
    },
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    AssignOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Field(Box<Expr>, Ident),
    Index(Box<Expr>, Box<Expr>),
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        bound: RangeLimits,
    },
    Underscore,
    Path(Path),
    AddrOf {
        brw: BorrowKind,
        mutable: bool,
        expr: Box<Expr>,
    },
    Break {
        label: Option<Ident>,
        expr: Option<Box<Expr>>,
    },
    Continue(Option<Ident>),
    Ret(Option<Box<Expr>>),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum StmtKind {
    /// A local (let) binding.
    Local(Box<Local>),
    /// An item definition.
    Item(Box<Item>),
    /// Expr without trailing semi-colon.
    Expr(Box<Expr>),
    /// Expr with a trailing semi-colon.
    Semi(Box<Expr>),
    /// Just a semi.
    Empty,
    // TODO:
    // MacCall(Box<MacCallStmt>)
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum PatKind {
    Wild,
    /// A `PatKind::Ident` may either be a new bound variable (`ref mut binding @ OPT_SUBPATTERN`),
    /// or a unit struct/variant pattern, or a const pattern (in the last two cases the third
    /// field must be `None`). Disambiguation cannot be done with parser alone, so it happens
    /// during name resolution.
    Ident {
        bind: BindingMode,
        id: Ident,
        sub: Option<Box<Pat>>,
    },
    Struct {
        name: Path,
        fields: Vec<FieldPat>,
    },
    TupleStruct {
        name: Path,
        data: Vec<Pat>,
    },
    Or(Vec<Pat>),
    Path(Path),
    Tuple(Vec<Pat>),
    Ref {
        mutable: bool,
        pat: Box<Pat>,
    },
    Lit(Box<Expr>),
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        bound: RangeEnd,
    },
    Slice(Vec<Pat>),
    Rest,
    Paren(Box<Pat>),
}

impl PatKind {
    pub fn ident(&self) -> Option<&Ident> {
        match self {
            PatKind::Wild => None,
            PatKind::Ident { bind, id, sub } => Some(id),
            PatKind::Struct { name, fields } => name.seg.first(),
            PatKind::TupleStruct { name, data } => name.seg.first(),
            PatKind::Or(_) => None,
            PatKind::Path(path) => path.seg.first(),
            PatKind::Tuple(_) => None,
            PatKind::Ref { mutable, pat } => pat.kind.ident(),
            PatKind::Lit(_) => None,
            PatKind::Range { start, end, bound } => None,
            PatKind::Slice(_) => None,
            PatKind::Rest => None,
            PatKind::Paren(_) => None,
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Arm {
    pub pat: Box<Pat>,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Local {
    pub pat: Pat,
    pub ty: Option<Ty>,
    pub init: Option<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct FieldPat {
    pub id: Ident,
    pub pat: Box<Pat>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct FnKind {
    pub sig: FnSig,
    pub gen: (),
    pub block: Block,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct FnSig {
    pub inputs: Vec<Param>,
    pub ret: FnReturn,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Param {
    pub ty: Ty,
    pub pat: Pat,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum FnReturn {
    None,
    Explicit(Box<Ty>),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Ty {
    pub kind: Box<TyKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum TyKind {
    Slice(Box<Ty>),
    Arr(Box<Ty>, Expr),
    Ref(Lifetime, MutTy),
    FnPtr(Box<BareFn>),
    Tup(Vec<Ty>),
    Path { qualified: Qualified, p: Path },
    TraitObj,
    ImplTrait,
    Paren(Box<Ty>),
    RawPtr(MutTy),
    Never,
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
    pub ty: Box<Ty>,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Path {
    pub seg: Vec<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Qualified {
    Empty,
    Type {
        ty: Path,
        path_span: Span,
        pos: usize,
    },
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Literal {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LitKind {
    /// A string literal (`"foo"`).
    Str(String, StrStyle),
    /// A byte string (`b"foo"`).
    ByteStr(Box<[u8]>),
    /// A byte char (`b'f'`).
    Byte(u8),
    /// A character literal (`'a'`).
    Char(char),
    /// An integer literal (`1`).
    Int(u128, LitIntType),
    /// A float literal (`1f64` or `1E10f64`).
    Float(String, LitFloatType),
    /// A boolean literal.
    Bool(bool),
    /// Placeholder for a literal that wasn't well-formed in some way.
    Err(String),
}

/// Type of the integer literal based on provided suffix.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum LitIntType {
    /// e.g. `42_i32`.
    Signed(IntTy),
    /// e.g. `42_u32`.
    Unsigned(UintTy),
    /// e.g. `42`.
    Unsuffixed,
}

/// Type of the float literal based on provided suffix.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum LitFloatType {
    /// A float literal with a suffix (`1f32` or `1E10f32`).
    Suffixed(FloatTy),
    /// A float literal without a suffix (`1.0 or 1.0E10`).
    Unsuffixed,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum FloatTy {
    F32,
    F64,
}

impl FloatTy {
    pub fn name_str(self) -> &'static str {
        match self {
            FloatTy::F32 => "f32",
            FloatTy::F64 => "f64",
        }
    }

    pub fn bit_width(self) -> u64 {
        match self {
            FloatTy::F32 => 32,
            FloatTy::F64 => 64,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

impl IntTy {
    pub fn name_str(&self) -> &'static str {
        match *self {
            IntTy::Isize => "isize",
            IntTy::I8 => "i8",
            IntTy::I16 => "i16",
            IntTy::I32 => "i32",
            IntTy::I64 => "i64",
            IntTy::I128 => "i128",
        }
    }

    pub fn bit_width(&self) -> Option<u64> {
        Some(match *self {
            IntTy::Isize => return None,
            IntTy::I8 => 8,
            IntTy::I16 => 16,
            IntTy::I32 => 32,
            IntTy::I64 => 64,
            IntTy::I128 => 128,
        })
    }

    pub fn normalize(&self, target_width: u32) -> Self {
        match self {
            IntTy::Isize => match target_width {
                16 => IntTy::I16,
                32 => IntTy::I32,
                64 => IntTy::I64,
                _ => unreachable!(),
            },
            _ => *self,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Debug)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl UintTy {
    pub fn name_str(&self) -> &'static str {
        match *self {
            UintTy::Usize => "usize",
            UintTy::U8 => "u8",
            UintTy::U16 => "u16",
            UintTy::U32 => "u32",
            UintTy::U64 => "u64",
            UintTy::U128 => "u128",
        }
    }

    pub fn bit_width(&self) -> Option<u64> {
        Some(match *self {
            UintTy::Usize => return None,
            UintTy::U8 => 8,
            UintTy::U16 => 16,
            UintTy::U32 => 32,
            UintTy::U64 => 64,
            UintTy::U128 => 128,
        })
    }

    pub fn normalize(&self, target_width: u32) -> Self {
        match self {
            UintTy::Usize => match target_width {
                16 => UintTy::U16,
                32 => UintTy::U32,
                64 => UintTy::U64,
                _ => unreachable!(),
            },
            _ => *self,
        }
    }
}

#[derive(Clone, Debug, Copy, Hash, Eq, PartialEq)]
pub enum StrStyle {
    /// A regular string, like `"foo"`.
    Cooked,
    /// A raw string, like `r##"foo"##`.
    ///
    /// The value is the number of `#` symbols used.
    Raw(u16),
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Clone, PartialEq, Hash, Eq, Debug, Copy)]
pub enum BinOpKind {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
}

impl BinOpKind {
    pub fn to_string(&self) -> &'static str {
        use BinOpKind::*;
        match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
            And => "&&",
            Or => "||",
            BitXor => "^",
            BitAnd => "&",
            BitOr => "|",
            Shl => "<<",
            Shr => ">>",
            Eq => "==",
            Lt => "<",
            Le => "<=",
            Ne => "!=",
            Ge => ">=",
            Gt => ">",
        }
    }
    pub fn lazy(&self) -> bool {
        matches!(self, BinOpKind::And | BinOpKind::Or)
    }

    pub fn is_comparison(&self) -> bool {
        use BinOpKind::*;
        // Note for developers: please keep this as is;
        // we want compilation to fail if another variant is added.
        match *self {
            Eq | Lt | Le | Ne | Gt | Ge => true,
            And | Or | Add | Sub | Mul | Div | Rem | BitXor | BitAnd | BitOr | Shl | Shr => false,
        }
    }
}

/// Unary operator.
///
/// Note that `&data` is not an operator, it's an `AddrOf` expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Copy)]
pub enum UnOp {
    /// The `*` operator for dereferencing
    Deref,
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

impl UnOp {
    pub fn to_string(op: UnOp) -> &'static str {
        match op {
            UnOp::Deref => "*",
            UnOp::Not => "!",
            UnOp::Neg => "-",
        }
    }
}

/// The kind of borrow in an `AddrOf` expression,
/// e.g., `&place` or `&raw const place`.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum BorrowKind {
    /// A normal borrow, `&$expr` or `&mut $expr`.
    /// The resulting type is either `&'a T` or `&'a mut T`
    /// where `T = typeof($expr)` and `'a` is some lifetime.
    Ref,
    /// A raw borrow, `&raw const $expr` or `&raw mut $expr`.
    /// The resulting type is either `*const T` or `*mut T`
    /// where `T = typeof($expr)`.
    Raw,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum BindingMode {
    Value { mutable: bool },
    Ref { mutable: bool },
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
/// Wether the range includes the end or not.
pub enum RangeLimits {
    Inclusive,
    Closed,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
/// Wether the range includes the end or not.
pub enum RangeEnd {
    Included,
    Excluded,
}
