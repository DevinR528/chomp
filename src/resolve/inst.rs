use hir_def::expr::{ArithOp, BinaryOp};

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Start a new block with the given label.
    Label(String),
    /// Push `Location` to the stack.
    Push(Location),
    /// Jump to the address saving stack info.
    ///
    /// the `Location` is most often a label but can be an address.
    Call(Location),
    /// Jump to the specified `Location`.
    Jmp(Location),
    /// Move source to destination.
    Mov { dst: Location, src: Location },
    /// Add source to destination.
    Add { dst: Location, src: Location },
    /// Multiply source to destination.
    Mul { dst: Location, src: Location },
    /// Subtract source to destination.
    Sub { dst: Location, src: Location },
    /// Divide source to destination.
    Div { dst: Location, src: Location },
}

impl Instruction {
    pub fn from_binop(lhs: Location, rhs: Location, op: &BinaryOp) -> Self {
        match op {
            BinaryOp::LogicOp(_) => todo!(),
            BinaryOp::ArithOp(maths) => match maths {
                ArithOp::Add => Instruction::Add { dst: lhs, src: rhs },
                ArithOp::Mul => Instruction::Mul { dst: lhs, src: rhs },
                ArithOp::Sub => Instruction::Sub { dst: lhs, src: rhs },
                ArithOp::Div => Instruction::Div { dst: lhs, src: rhs },
                ArithOp::Rem => todo!(),
                ArithOp::Shl => todo!(),
                ArithOp::Shr => todo!(),
                ArithOp::BitXor => todo!(),
                ArithOp::BitOr => todo!(),
                ArithOp::BitAnd => todo!(),
            },
            BinaryOp::CmpOp(_) => todo!(),
            BinaryOp::Assignment { op } => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Location {
    /// Something like this `BYTE PTR [rbp-1]`.
    RegAddr {
        reg: Register,
        offset: usize,
        size: usize,
    },
    /// Plain register.
    Register(Register),
    /// Constant, like `10`.
    ///
    /// This always represents a value never a label.
    Const(String),
    /// A label to jump or call to.
    Label(String),
}

impl Location {
    pub fn reg_addr(size: usize, offset: usize) -> Self {
        todo!()
    }
}

#[rustfmt::skip]
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8, R9, R10, R11, R12, R13, R14, R15, R16,
}

use Register::*;
pub const ARG_REGS: [Register; 6] = [RDI, RSI, RDX, RCX, R8, R9];
