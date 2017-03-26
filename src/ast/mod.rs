use std::ops::Deref;
use std::fmt;

pub mod printer;

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    pub declarations: Vec<Spanned<Declaration>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function {
        name: String,
        params: Vec<(String, Spanned<ParseType>)>,
        return_ty: Spanned<ParseType>,
        stmt: Spanned<Statement>,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound {
        stmts: Vec<Spanned<Statement>>,
    },
    Let {
        name: String,
        ty: Option<Spanned<ParseType>>,
        expr: Spanned<Expression>,
    },
    Loop {
        stmt: Box<Spanned<Statement>>,
    },
    While {
        cond: Spanned<Expression>,
        stmt: Box<Spanned<Statement>>,
    },
    If {
        if_branch: (Spanned<Expression>, Box<Spanned<Statement>>),
        elseif_branches: Vec<(Spanned<Expression>, Spanned<Statement>)>,
        else_branch: Option<Box<Spanned<Statement>>>,
    },
    Break,
    Continue,
    Return {
        expr: Spanned<Expression>,
    },
    Expression {
        expr: Spanned<Expression>,
    },
    Print {
        expr: Spanned<Expression>,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Assign(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Subscript(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    BinOp(BinOpCode, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    UnOp(UnOpCode, Box<Spanned<Expression>>),
    FuncCall(Box<Spanned<Expression>>, Vec<Spanned<Expression>>),
    Paren(Box<Spanned<Expression>>),
    Identifier(String),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Double(f64),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpCode {
    Add,
    Sub,
    Times,
    Divide,
    Mod,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpCode {
    Minus,
    LogicalNot,
    AddressOf,
    Deref,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span (pub usize, pub usize);

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Spanned {
            inner: inner,
            span: span
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseType {
    Unit,
    Lit(String),
    Array(Box<Spanned<ParseType>>),
    Ptr(Box<Spanned<ParseType>>),
}

impl fmt::Display for ParseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseType::Unit => write!(f, "()"),
            ParseType::Lit(ref lit) => write!(f, "{}", lit),
            ParseType::Array(ref sub) => write!(f, "[{}]", sub.inner),
            ParseType::Ptr(ref sub) => write!(f, "*{}", sub.inner),
        }
    }
}
