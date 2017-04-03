use ast;
use std::fmt;
use itertools::Itertools;

pub mod builder;
pub mod printer;
pub mod opt;
mod tyck;

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    ExternFunction { name: String, ty: FunctionType },
    Function {
        name: String,
        ty: FunctionType,
        locals: Vec<LocalVar>,
        bbs: Vec<BasicBlock>,
    },
}

#[derive(Debug, Clone)]
pub struct LocalVar {
    id: LocalVarId,
    ty: Type,
    param_index: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    id: BasicBlockId,
    stmts: Vec<Statement>,
    terminator: Terminator,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Br(BasicBlockId),
    BrCond(Value, BasicBlockId, BasicBlockId), // (true, false)
    Ret(Value),
}

#[derive(Debug, Clone)]
pub enum Statement {
    LValueSet(Value, Value),
    Assign(Value, Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    LocalVarLoad(LocalVarId),
    GlobalLoad(String),
    LValueLoad(Value),
    BinOp(BinOpCode, Value, Value),
    UnOp(UnOpCode, Value),
    ReadArray(Value, Value),
    FuncCall(Value, Vec<Value>),
    Literal(ast::Literal),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BasicBlockId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalVarId(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    id: usize,
    ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpCode {
    PtrAdd,

    IntAdd,
    IntSub,
    IntTimes,
    IntDivide,
    IntMod,
    DoubleAdd,
    DoubleSub,
    DoubleTimes,
    DoubleDivide,

    IntLess,
    IntLessEqual,
    IntGreater,
    IntGreaterEqual,
    DoubleLess,
    DoubleLessEqual,
    DoubleGreater,
    DoubleGreaterEqual,

    IntEqual,
    IntNotEqual,
    DoubleEqual,
    DoubleNotEqual,
    BoolEqual,
    BoolNotEqual,

    BoolLogicalAnd,
    BoolLogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpCode {
    IntMinus,
    DoubleMinus,
    BoolLogicalNot,
    AddressOf,
    PtrDeref,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Double,
    LValue(Box<Type>),
    Array(Box<Type>),
    Ptr(Box<Type>),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    return_ty: Box<Type>,
    params_ty: Vec<Type>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Double => write!(f, "double"),
            Type::LValue(ref sub) => write!(f, "&{}", *sub),
            Type::Array(ref sub) => write!(f, "[{}]", *sub),
            Type::Ptr(ref sub) => write!(f, "*{}", *sub),
            Type::Function(ref func) => write!(f, "{}", func),
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "({}) -> {}",
               self.params_ty.iter().join(", "),
               *self.return_ty)
    }
}
