use std::{
    hash::{Hash, Hasher},
    mem::{size_of, size_of_val},
};

use chalk_ir::{Scalar, UintTy};
use hir::BuiltinType;
use hir_def::{
    body::{Body, BodySourceMap},
    data::FunctionData,
    expr::{BinaryOp, Expr, ExprId, Pat, PatId, Statement},
};
use hir_def::{builtin_type::BuiltinUint, expr::Literal};
use hir_expand::name::Name;
use hir_ty::{InferenceResult, Interner, Ty, TyKind};
use rustc_hash::FxHashMap;

use super::inst::{Instruction, Location, Register, ARG_REGS};

/// Anything that can be on the left hand side of `=`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lvar {
    Ident(Name),
    Segmented(Vec<Name>),
}

impl Lvar {
    pub fn repr(&self) -> String {
        match self {
            Lvar::Ident(id) => format!("::{}", id),
            Lvar::Segmented(ids) => ids.iter().map(|id| format!("::{}", id)).collect(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Rval {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Usize(usize),
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Isize(isize),
    Bool(bool),
    Char(char),
}

// TODO: Do I care about the ...
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Rval {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Rval::U8(i) => i.hash(state),
            Rval::U16(i) => i.hash(state),
            Rval::U32(i) => i.hash(state),
            Rval::U64(i) => i.hash(state),
            Rval::U128(i) => i.hash(state),
            Rval::Usize(i) => i.hash(state),
            Rval::F32(i) => i.to_string().hash(state),
            Rval::F64(i) => i.to_string().hash(state),
            Rval::I8(i) => i.hash(state),
            Rval::I16(i) => i.hash(state),
            Rval::I32(i) => i.hash(state),
            Rval::I64(i) => i.hash(state),
            Rval::I128(i) => i.hash(state),
            Rval::Isize(i) => i.hash(state),
            Rval::Bool(i) => i.hash(state),
            Rval::Char(i) => i.hash(state),
        }
    }
}

impl Rval {
    pub fn size(&self) -> usize {
        match self {
            Rval::U8(i) => size_of_val(i),
            Rval::U16(i) => size_of_val(i),
            Rval::U32(i) => size_of_val(i),
            Rval::U64(i) => size_of_val(i),
            Rval::U128(i) => size_of_val(i),
            Rval::Usize(i) => size_of_val(i),
            Rval::F32(i) => size_of_val(i),
            Rval::F64(i) => size_of_val(i),
            Rval::I8(i) => size_of_val(i),
            Rval::I16(i) => size_of_val(i),
            Rval::I32(i) => size_of_val(i),
            Rval::I64(i) => size_of_val(i),
            Rval::I128(i) => size_of_val(i),
            Rval::Isize(i) => size_of_val(i),
            Rval::Bool(i) => size_of_val(i),
            Rval::Char(i) => size_of_val(i),
        }
    }

    pub fn repr(&self) -> String {
        match self {
            Rval::U8(i) => i.to_string(),
            Rval::U16(i) => i.to_string(),
            Rval::U32(i) => i.to_string(),
            Rval::U64(i) => i.to_string(),
            Rval::U128(i) => i.to_string(),
            Rval::Usize(i) => i.to_string(),
            Rval::F32(i) => i.to_string(),
            Rval::F64(i) => i.to_string(),
            Rval::I8(i) => i.to_string(),
            Rval::I16(i) => i.to_string(),
            Rval::I32(i) => i.to_string(),
            Rval::I64(i) => i.to_string(),
            Rval::I128(i) => i.to_string(),
            Rval::Isize(i) => i.to_string(),
            Rval::Bool(i) => i.to_string(),
            Rval::Char(i) => i.to_string(),
        }
    }

    pub fn uint_value(&self) -> usize {
        match self {
            Rval::U8(i) => *i as usize,
            Rval::U16(i) => *i as usize,
            Rval::U32(i) => *i as usize,
            Rval::U64(i) => *i as usize,
            Rval::U128(i) => *i as usize,
            Rval::Usize(i) => *i as usize,
            _ => todo!(),
        }
    }

    pub fn sint_value(&self) -> isize {
        match self {
            Rval::I8(i) => *i as isize,
            Rval::I16(i) => *i as isize,
            Rval::I32(i) => *i as isize,
            Rval::I64(i) => *i as isize,
            Rval::I128(i) => *i as isize,
            Rval::Isize(i) => *i as isize,
            _ => todo!(),
        }
    }

    pub fn from_uint(val: &u128, b_ty: Option<BuiltinUint>, ty: &Ty) -> Self {
        match b_ty {
            Some(BuiltinUint::U8) => Self::U8(*val as u8),
            Some(BuiltinUint::U16) => Self::U16(*val as u16),
            Some(BuiltinUint::U32) => Self::U32(*val as u32),
            Some(BuiltinUint::U64) => Self::U64(*val as u64),
            Some(BuiltinUint::U128) => Self::U128(*val),
            Some(BuiltinUint::Usize) => Self::Usize(*val as usize),
            _ => match ty.interned(&Interner) {
                TyKind::Scalar(scalar) => match scalar {
                    Scalar::Uint(UintTy::U8) => Self::U8(*val as u8),
                    Scalar::Uint(UintTy::U16) => Self::U16(*val as u16),
                    Scalar::Uint(UintTy::U32) => Self::U32(*val as u32),
                    Scalar::Uint(UintTy::U64) => Self::U64(*val as u64),
                    Scalar::Uint(UintTy::U128) => Self::U128(*val),
                    Scalar::Uint(UintTy::Usize) => Self::Usize(*val as usize),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
        }
    }

    /// This creates a Default value `Rval`, it does not use the
    /// parsed value from source code.
    pub fn from_scalar(scalar: &Scalar) -> Self {
        match scalar {
            Scalar::Bool => Self::Bool(false),
            Scalar::Char => Self::Char(Default::default()),
            Scalar::Int(int) => match int {
                chalk_ir::IntTy::Isize => Self::Isize(0),
                chalk_ir::IntTy::I8 => Self::I8(0),
                chalk_ir::IntTy::I16 => Self::I16(0),
                chalk_ir::IntTy::I32 => Self::I32(0),
                chalk_ir::IntTy::I64 => Self::I64(0),
                chalk_ir::IntTy::I128 => Self::I128(0),
            },
            Scalar::Uint(uint) => match uint {
                chalk_ir::UintTy::Usize => Self::Usize(0),
                chalk_ir::UintTy::U8 => Self::U8(0),
                chalk_ir::UintTy::U16 => Self::U16(0),
                chalk_ir::UintTy::U32 => Self::U32(0),
                chalk_ir::UintTy::U64 => Self::U64(0),
                chalk_ir::UintTy::U128 => Self::U128(0),
            },
            Scalar::Float(f) => match f {
                chalk_ir::FloatTy::F32 => Self::F32(0.0),
                chalk_ir::FloatTy::F64 => Self::F64(0.0),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Opp {
    Konst(Rval),
    Named {
        lvar: Lvar,
        rval: Box<Opp>,
        offset: usize,
    },
    BinOp {
        l: Box<Opp>,
        r: Box<Opp>,
        op: BinaryOp,
    },
    Call {
        name: Lvar,
        push_args: Vec<Opp>,
        ret: Rval,
    },
}

impl Opp {
    pub fn size(&self) -> usize {
        match self {
            Opp::Konst(r) => r.size(),
            Opp::Named { rval, .. } => rval.size(),
            Opp::BinOp { l, r, op } => match op {
                BinaryOp::LogicOp(_) => r.size(),
                BinaryOp::ArithOp(_) => r.size(),
                BinaryOp::CmpOp(_) => size_of::<bool>(),
                BinaryOp::Assignment { op } => l.size(),
            },
            Opp::Call { push_args, .. } => push_args.iter().map(|arg| arg.size()).sum(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct OppBuilder {}

#[derive(Clone, Debug, Default)]
pub struct BranchBuilder {}

#[derive(Clone, Debug)]
pub struct StmtBuilder<'a> {
    call_map: FxHashMap<ExprId, Rval>,
    lvars: FxHashMap<Lvar, Opp>,
    offset_map: FxHashMap<Lvar, Location>,
    rep: Vec<Opp>,
    infer: &'a InferenceResult,
    body: &'a Body,
}

impl<'a> StmtBuilder<'a> {
    pub fn new(
        infer: &'a InferenceResult,
        body: &'a Body,
        call_map: FxHashMap<ExprId, Expr>,
    ) -> Self {
        Self {
            call_map: FxHashMap::default(),
            lvars: FxHashMap::default(),
            offset_map: FxHashMap::default(),
            rep: vec![],
            infer,
            body,
        }
    }
    pub fn add_let(&mut self, ty: &Ty, pat: &Pat, expr: &Expr) {
        match pat {
            Pat::Bind { mode, name, subpat } => {
                let rval = Box::new(self.add_expr(expr, ty));

                let lvar = Lvar::Ident(name.clone());
                let offset = rval.size();
                let var = Opp::Named {
                    rval,
                    lvar: lvar.clone(),
                    offset,
                };

                self.lvars.insert(lvar, var.clone());
                self.rep.push(var);
            }
            pat => panic!("{:?}", pat),
        }
    }

    fn add_expr(&mut self, expr: &Expr, ty: &Ty) -> Opp {
        match expr {
            Expr::Path(path) => match path.mod_path().segments() {
                [single] => self
                    .lvars
                    .get(&Lvar::Ident(single.clone()))
                    .unwrap()
                    .clone(),
                more => self
                    .lvars
                    .get(&Lvar::Segmented(more.to_vec()))
                    .unwrap()
                    .clone(),
            },
            Expr::BinaryOp { lhs, rhs, ref op } => {
                let lhs = self.add_expr(&self.body[*lhs], ty);
                let rhs = self.add_expr(&self.body[*rhs], ty);
                Opp::BinOp {
                    l: Box::new(lhs),
                    r: Box::new(rhs),
                    op: op.unwrap(),
                }
            }
            Expr::Literal(lit) => match lit {
                Literal::String(_) => todo!(),
                Literal::ByteString(_) => todo!(),
                Literal::Char(_) => todo!(),
                Literal::Byte(_) => todo!(),
                Literal::Bool(_) => todo!(),
                Literal::Int(_, _) => todo!(),
                Literal::Uint(val, b_ty) => Opp::Konst(Rval::from_uint(val, *b_ty, ty)),
                Literal::Float(_, _) => todo!(),
            },
            Expr::Call { callee, args } => {
                let push_args = args
                    .iter()
                    .map(|arg| {
                        self.add_expr(&self.body[*arg], self.infer.type_of_expr.get(*arg).unwrap())
                    })
                    .collect::<Vec<_>>();

                let ret = self.call_map.get(callee).unwrap().clone();

                let call = if let Expr::Path(path) = &self.body[*callee] {
                    Opp::Call {
                        name: Lvar::Segmented(
                            path.segments().iter().map(|p| p.name.clone()).collect(),
                        ),
                        push_args,
                        ret,
                    }
                } else {
                    unreachable!()
                };

                call
            }
            expr => panic!("{:?}", expr),
        }
    }

    fn return_size(&self, ty: Option<&Ty>) -> usize {
        match ty.unwrap().interned(&Interner) {
            TyKind::Scalar(scalar) => Rval::from_scalar(scalar).size(),
            t => todo!("{:?}", t),
        }
    }

    pub fn lower(mut self) -> Vec<Instruction> {
        let opps = self.rep.drain(..).collect::<Vec<_>>();
        let mut offset = 0;
        let mut instructions = vec![];
        for opp in opps {
            match opp {
                // TODO: this assumes all locals
                Opp::Named {
                    lvar,
                    offset: size,
                    rval,
                } => {
                    offset += size;
                    let dst = Location::RegAddr {
                        reg: Register::RBP,
                        size,
                        offset,
                    };
                    self.offset_map.insert(lvar, dst.clone());

                    let (src, before) = self._lower(offset, &rval);

                    instructions.extend(before);
                    instructions.push(Instruction::Mov { dst, src });
                }

                Opp::Konst(rval) => {}
                Opp::BinOp { l, r, op } => {}
                call @ Opp::Call { .. } => {
                    let (label, inst) = self._lower(offset, &call);
                    instructions.extend(inst);
                    instructions.push(Instruction::Call(label));
                }
            }
        }

        instructions
    }

    pub fn _lower(&self, off: usize, var: &Opp) -> (Location, Vec<Instruction>) {
        let mut offset = off;
        let mut instructions = vec![];
        let src = match var {
            Opp::Konst(rval) => Location::Const(rval.repr()),
            Opp::Named { lvar, .. } => self.offset_map.get(lvar).cloned().unwrap(),
            Opp::BinOp { l, r, op } => {
                let (lhs, l_inst) = self._lower(off, &l);
                let (rhs, r_inst) = self._lower(off, &r);
                instructions.extend(l_inst);
                instructions.extend(r_inst);
                instructions.push(Instruction::from_binop(lhs.clone(), rhs, op));
                lhs
            }
            Opp::Call {
                name,
                push_args,
                ret,
            } => {
                for (count, arg) in push_args.iter().enumerate() {
                    let (src, inst) = self._lower(offset, arg);
                    instructions.push(Instruction::Mov {
                        dst: Location::Register(ARG_REGS[count]),
                        src,
                    });
                }
                Location::Label(name.repr())
            }
        };

        (src, instructions)
    }
}

#[derive(Clone, Debug)]
pub struct FnBuilder<'a> {
    name: Vec<Name>,
    params: FxHashMap<PatId, Ty>,
    stmts: StmtBuilder<'a>,
    stack_size: usize,
}

impl<'a> FnBuilder<'a> {
    pub fn new(
        func: &FunctionData,
        body: &'a Body,
        map: &BodySourceMap,
        infer: &'a InferenceResult,
        path: &[hir::Name],
        call_map: FxHashMap<ExprId, Expr>,
    ) -> Self {
        let mut name = path.to_vec();
        name.push(func.name.clone());

        let mut stmts = StmtBuilder::new(infer, body, call_map);
        let mut stack_size = 0;

        let mut params = FxHashMap::default();
        for p in &body.params {
            let ty = infer.type_of_pat.get(*p).unwrap();
            params.insert(*p, ty.clone());
        }

        if let Expr::Block {
            id,
            statements,
            tail,
            label,
        } = &body[body.body_expr]
        {
            for stmt in statements {
                match stmt {
                    Statement::Let {
                        pat,
                        type_ref,
                        initializer,
                    } => {
                        let ty = infer.type_of_pat.get(*pat).unwrap();
                        let expr = infer.type_of_expr.get(initializer.unwrap()).unwrap();

                        assert_eq!(ty, expr);

                        stmts.add_let(ty, &body[*pat], &body[initializer.unwrap()]);
                    }
                    Statement::Expr(ex) => {}
                }

                // Total stack size for this function
                stack_size += stmts.rep.last().map(|o| o.size()).unwrap_or_default();
            }

            if let Some(ret) = tail {}
        }

        let mut this = Self {
            name,
            params,
            stmts,
            stack_size,
        };
        let that = this.clone();
        this.into_instructions();
        that
    }

    // Load function call arguments. Arguments are already evaluated and
    // stored to the stack as local variables. What we need to do in this
    // function is to load them to registers or push them to the stack as
    // specified by the x86-64 psABI. Here is what the spec says:
    //
    // - Up to 6 arguments of integral type are passed using RDI, RSI,
    //   RDX, RCX, R8 and R9.
    //
    // - Up to 8 arguments of floating-point type are passed using XMM0 to
    //   XMM7.
    //
    // - If all registers of an appropriate type are already used, push an
    //   argument to the stack in the right-to-left order.
    //
    // - Each argument passed on the stack takes 8 bytes, and the end of
    //   the argument area must be aligned to a 16 byte boundary.
    //
    // - If a function is variadic, set the number of floating-point type
    //   arguments to RAX.
    pub fn into_instructions(mut self) -> Vec<Instruction> {
        let mut func = vec![
            Instruction::Label(self.name.iter().map(|n| format!("::{}", n)).collect()),
            Instruction::Push(Location::Register(Register::RBP)),
            Instruction::Mov {
                dst: Location::Register(Register::RBP),
                src: Location::Register(Register::RSP),
            },
        ];

        for (pat, ty) in self.params.iter() {
            // func.push(value)
        }

        let body = self.stmts.lower();

        func.extend(body);
        dbg!(func)
    }
}
