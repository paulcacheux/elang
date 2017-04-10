use std::ops::Deref;
use std::fmt;

pub mod printer;

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    pub imports: Vec<String>,
    pub declarations: Vec<Spanned<Declaration>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    ExternFunction {
        name: String,
        params: Vec<Spanned<ParseType>>,
        variadic: bool,
        return_ty: Spanned<ParseType>,
    },
    Function {
        name: String,
        params: Vec<(Spanned<String>, Spanned<ParseType>)>,
        return_ty: Spanned<ParseType>,
        stmt: Spanned<CompoundStatement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement(pub Vec<Spanned<Statement>>);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Spanned<CompoundStatement>),
    Let {
        name: String,
        ty: Option<Spanned<ParseType>>,
        expr: Spanned<Expression>,
    },
    Loop { stmt: Spanned<CompoundStatement> },
    While {
        cond: Spanned<Expression>,
        stmt: Spanned<CompoundStatement>,
    },
    For {
        name: String,
        init_expr: Spanned<Expression>,
        cond_expr: Spanned<Expression>,
        step_expr: Spanned<Expression>,
        stmt: Spanned<CompoundStatement>,
    },
    If {
        if_branch: (Spanned<Expression>, Spanned<CompoundStatement>),
        elseif_branches: Vec<(Spanned<Expression>, Spanned<CompoundStatement>)>,
        else_branch: Option<Spanned<CompoundStatement>>,
    },
    Break,
    Continue,
    Return { expr: Option<Spanned<Expression>> },
    Expression { expr: Spanned<Expression> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Assign(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Subscript(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    BinOp(BinOpCode, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    UnOp(UnOpCode, Box<Spanned<Expression>>),
    FuncCall(Box<Spanned<Expression>>, Vec<Spanned<Expression>>),
    Cast(Box<Spanned<Expression>>, Spanned<ParseType>),
    Paren(Box<Spanned<Expression>>),
    Identifier(String),
    Literal(Literal),
    StringLiteral(String),
    ArrayFullLiteral(Vec<Spanned<Expression>>),
    ArrayDefaultLiteral(Box<Spanned<Expression>>, i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Double(f64),
    Bool(bool),
    Char(String),
    Unit,
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

impl fmt::Display for BinOpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BinOpCode::*;
        let op = match *self {
            Add => "+",
            Sub => "-",
            Times => "*",
            Divide => "/",
            Mod => "%",
            Less => "<",
            LessEqual => "<=",
            Greater => ">",
            GreaterEqual => ">=",
            Equal => "==",
            NotEqual => "!=",
            LogicalAnd => "&&",
            LogicalOr => "||",
        };
        write!(f, "{}", op)
    }
}

impl fmt::Display for UnOpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::UnOpCode::*;
        let op = match *self {
            Minus => "-",
            LogicalNot => "!",
            AddressOf => "&",
            Deref => "*",
        };
        write!(f, "{}", op)
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Spanned {
            inner: inner,
            span: span,
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
    Ptr(Box<Spanned<ParseType>>),
}

impl fmt::Display for ParseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseType::Unit => write!(f, "()"),
            ParseType::Lit(ref lit) => write!(f, "{}", lit),
            ParseType::Ptr(ref sub) => write!(f, "*{}", sub.inner),
        }
    }
}
