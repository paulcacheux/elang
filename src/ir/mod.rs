pub mod builder;
pub mod printer;
pub mod opt;
pub mod symbol_table;
pub mod ty;

pub use self::symbol_table::{SymbolTable, GlobalTable};
pub use self::ty::{Type, FunctionType, StructType};

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
    pub id: LocalVarId,
    pub ty: Type,
    pub size: usize,
    pub param_index: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub stmts: Vec<Statement>,
    pub terminator: Terminator,
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
    CastOp(CastCode, Value),
    FuncCall(Value, Vec<Value>),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Double(f64),
    Bool(bool),
    Char(u8),
    Unit,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BasicBlockId(pub usize);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct LocalVarId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub id: usize,
    pub ty: Type,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpCode {
    IntMinus,
    DoubleMinus,
    BoolLogicalNot,
    AddressOf,
    PtrDeref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastCode {
    IntToDouble,
    DoubleToInt,
    IntToChar,
    CharToInt,
    IntToBool,
    BoolToInt,
}
