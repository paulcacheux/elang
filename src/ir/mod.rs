use ast;

pub mod builder;
mod tyck;

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function {
        name: String,
        ty: Type,
        params: Vec<String>,
        stmt: Statement,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound {
        stmts: Vec<Statement>,
    },
    Loop {
        stmt: Box<Statement>,
    },
    While {
        cond: Value,
        stmt: Box<Statement>,
    },
    If {
        cond: Value,
        if_stmt: Box<Statement>,
        else_stmt: Option<Box<Statement>>,
    },
    Break,
    Continue,
    Return {
        value: Value,
    },
    Print {
        value: Value,
    },
    VarDecl {
        name: String,
        value: Value,
    },
    LValueSet {
        lvalue: Value,
        rvalue: Value,
    },
    Assign {
        dest: usize,
        expr: Expr,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    LoadVar(String),
    LValueToRValue(Value),
    BinOp(BinOpCode, Value, Value),
    UnOp(UnOpCode, Value),
    Subscript(Value, Value),
    FuncCall(Value, Vec<Value>),
    Literal(ast::Literal),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub id: usize,
    pub ty: Type,
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
    Function(Box<Type>, Vec<Type>),
}
