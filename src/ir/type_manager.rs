use std::fmt;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Double,
    Char,
    LValue(Box<Type>),
    Ptr(Box<Type>),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub return_ty: Box<Type>,
    pub params_ty: Vec<Type>,
    pub variadic: bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Double => write!(f, "double"),
            Type::Char => write!(f, "char"),
            Type::LValue(ref sub) => write!(f, "&{}", *sub),
            Type::Ptr(ref sub) => write!(f, "*{}", *sub),
            Type::Function(ref func) => write!(f, "{}", func),
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "({}{}) -> {}",
               self.params_ty.iter().join(", "),
               if self.variadic { ", .." } else { "" },
               *self.return_ty)
    }
}
