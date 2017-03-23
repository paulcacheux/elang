use std::ops::Deref;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TranslationUnit {
    pub stmts: Vec<Spanned<Statement>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    FuncDecl {
        name: String,
        params: Vec<(String, ParseType)>,
        return_ty: ParseType,
        stmt: Box<Spanned<Statement>>,
    },
    Compound {
        stmts: Vec<Spanned<Statement>>,
    },
    Let {
        name: String,
        ty: Option<ParseType>,
        expr: Spanned<Expr>,
    },
    Loop {
        stmt: Box<Spanned<Statement>>,
    },
    While {
        cond: Spanned<Expr>,
        stmt: Box<Spanned<Statement>>,
    },
    If {
        ifs_branches: Vec<(Spanned<Expr>, Spanned<Statement>)>,
        else_branch: Option<Box<Spanned<Statement>>>,
    },
    Break,
    Continue,
    Return {
        expr: Spanned<Expr>,
    },
    Expr {
        expr: Spanned<Expr>,
    },
    Print {
        expr: Spanned<Expr>,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    BinOp(BinOpCode, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    UnOp(UnOpCode, Box<Spanned<Expr>>),
    FuncCall(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    Paren(Box<Spanned<Expr>>),
    Identifier(String),
    Number(i32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpCode {
    Assign,
    Subscript,
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
    Array(Box<ParseType>),
    Ptr(Box<ParseType>),
}

impl fmt::Display for ParseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseType::Unit => write!(f, "()"),
            ParseType::Lit(ref lit) => write!(f, "{}", lit),
            ParseType::Array(ref sub) => write!(f, "[{}]", sub),
            ParseType::Ptr(ref sub) => write!(f, "*{}", sub),
        }
    }
}
