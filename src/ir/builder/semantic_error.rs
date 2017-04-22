use std::fmt;
use span::Span;
use ir::Type;
use ast;

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    FunctionAlreadyDefined { name: String },
    ParameterAlreadyDefined { name: String },
    LocalVariableAlreadyDefined { name: String },
    FieldAlreadyDefined { name: String },
    TypeAlreadyDefined { name: String },
    MismatchingTypesAssignment { expected: Type, found: Type },
    MismatchingTypesCondition { found: Type },
    MismatchingTypesReturn { expected: Type, found: Type },
    MismatchingTypesArgument { expected: Type, found: Type },
    MismatchingTypesArrayLiteral { expected: Type, found: Type },
    UndefinedType { name: String },
    BinaryOperationUndefined {
        op: ast::BinOpCode,
        lhs_ty: Type,
        rhs_ty: Type,
    },
    UnaryOperationUndefined { op: ast::UnOpCode, expr_ty: Type },
    IndexNotInt { found: Type },
    NonAssignableExpression,
    NonSubscriptableType { found: Type },
    NonCallableType { found: Type },
    MismatchingParamLen { expected: usize, found: usize },
    CastUndefined { expr_ty: Type, target_ty: Type },
    NoFieldInStruct { struct_ty: Type, field: String },
    NotAStructType { ty: Type },
    IdentifierUndefined { name: String },
    InvalidEscapeChar { c: char },
    MultipleCharLiteral,
    EmptyCharLiteral,
    EmptyArrayLiteral,
    BreakOutsideLoop,
    ContinueOutsideLoop,
    NotAllPathsReturnAValue,
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SemanticErrorKind::*;
        match *self {
            FunctionAlreadyDefined { ref name } => {
                write!(f, "'{}' function is already defined.", name)
            }
            ParameterAlreadyDefined { ref name } => {
                write!(f, "'{}' parameter is already defined.", name)
            }
            LocalVariableAlreadyDefined { ref name } => {
                write!(f, "'{}' local variable is already defined.", name)
            }
            FieldAlreadyDefined { ref name } => {
                write!(f, "'{}' field is already defined in this struct.", name)
            }
            TypeAlreadyDefined { ref name } => {
                write!(f, "'{}' is already defined as a type.", name)
            }
            MismatchingTypesAssignment {
                ref expected,
                ref found,
            } => {
                write!(f,
                       "Mismatching types during assigment. Expected '{}', found '{}'.",
                       expected,
                       found)
            }
            MismatchingTypesCondition { ref found } => {
                write!(f,
                       "Mismatching types in condition. Expected '{}', found '{}'.",
                       Type::Bool,
                       found)
            }
            MismatchingTypesReturn {
                ref expected,
                ref found,
            } => {
                write!(f,
                       "Mismatching types in return statement. Expected '{}', found '{}'.",
                       expected,
                       found)
            }
            MismatchingTypesArgument {
                ref expected,
                ref found,
            } => {
                write!(f,
                       "Mismatching types in argument. Expected '{}', found '{}'.",
                       expected,
                       found)
            }
            MismatchingTypesArrayLiteral {
                ref expected,
                ref found,
            } => {
                write!(f,
                       "Mismatching types in array literal. Expected '{}', found '{}'.",
                       expected,
                       found)
            }
            UndefinedType { ref name } => write!(f, "'{}' is not defined as a type.", name),
            BinaryOperationUndefined {
                op,
                ref lhs_ty,
                ref rhs_ty,
            } => {
                write!(f,
                       "'{}' is not defined between '{}' and '{}'.",
                       op,
                       lhs_ty,
                       rhs_ty)
            }
            UnaryOperationUndefined { op, ref expr_ty } => {
                write!(f, "'{}' is not defined for '{}'.", op, expr_ty)
            }
            IndexNotInt { ref found } => {
                write!(f,
                       "Mismatching types in array index. Expected '{}', found '{}'.",
                       Type::Int,
                       found)
            }
            NonAssignableExpression => write!(f, "This expression is not assignable."),
            NonSubscriptableType { ref found } => {
                write!(f, "'{}' type is not subscriptable.", found)
            }
            NonCallableType { ref found } => write!(f, "'{}' type is not callable", found),
            MismatchingParamLen { expected, found } => {
                write!(f,
                       "This function takes '{}' parameters, but '{}' arguments were supplied.",
                       expected,
                       found)
            }
            CastUndefined {
                ref expr_ty,
                ref target_ty,
            } => {
                write!(f,
                       "The cast between '{}' and '{}' is undefined.",
                       expr_ty,
                       target_ty)
            }
            NoFieldInStruct {
                ref struct_ty,
                ref field,
            } => write!(f, "There is no '{}' field in '{}'.", field, struct_ty),
            NotAStructType { ref ty } => write!(f, "'{}' is not a struct type.", ty),
            IdentifierUndefined { ref name } => write!(f, "'{}' is not defined here.", name),
            InvalidEscapeChar { c } => write!(f, "'{}' is not a valide escape character.", c),
            MultipleCharLiteral => write!(f, "Multiple characters in a character literal."),
            EmptyCharLiteral => write!(f, "Empty character literal."),
            EmptyArrayLiteral => write!(f, "Empty array literal."),
            BreakOutsideLoop => write!(f, "'break' outside of loop."),
            ContinueOutsideLoop => write!(f, "'continue' outside of loop."),
            NotAllPathsReturnAValue => write!(f, "Not all paths return a value."),
        }
    }
}
