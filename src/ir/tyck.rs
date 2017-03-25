use ast;
use ir;

pub fn binop_tyck(op: ast::BinOpCode, lhs_ty: &ir::Type, rhs_ty: &ir::Type) -> Option<(ir::BinOpCode, ir::Type)> {
    use ast::BinOpCode::*;
    use ir::Type;
    match (op, lhs_ty, rhs_ty) {
        (Add, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntAdd, Type::Int)),
        (Sub, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntSub, Type::Int)),
        (Times, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntTimes, Type::Int)),
        (Divide, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntDivide, Type::Int)),
        (Mod, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntMod, Type::Int)),

        (Add, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleAdd, Type::Double)),
        (Sub, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleSub, Type::Double)),
        (Times, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleTimes, Type::Double)),
        (Divide, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleDivide, Type::Double)),

        (Less, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntLess, Type::Bool)),
        (LessEqual, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntLessEqual, Type::Bool)),
        (Greater, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntGreater, Type::Bool)),
        (GreaterEqual, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntGreaterEqual, Type::Bool)),

        (Less, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleLess, Type::Bool)),
        (LessEqual, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleLessEqual, Type::Bool)),
        (Greater, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleGreater, Type::Bool)),
        (GreaterEqual, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleGreaterEqual, Type::Bool)),

        (Equal, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntEqual, Type::Bool)),
        (NotEqual, &Type::Int, &Type::Int) => Some((ir::BinOpCode::IntNotEqual, Type::Bool)),
        (Equal, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleEqual, Type::Bool)),
        (NotEqual, &Type::Double, &Type::Double) => Some((ir::BinOpCode::DoubleNotEqual, Type::Bool)),
        (Equal, &Type::Bool, &Type::Bool) => Some((ir::BinOpCode::BoolEqual, Type::Bool)),
        (NotEqual, &Type::Bool, &Type::Bool) => Some((ir::BinOpCode::BoolNotEqual, Type::Bool)),

        (LogicalAnd, &Type::Bool, &Type::Bool) => Some((ir::BinOpCode::BoolLogicalAnd, Type::Bool)),
        (LogicalOr, &Type::Bool, &Type::Bool) => Some((ir::BinOpCode::BoolLogicalOr, Type::Bool)),

        (Add, &Type::Ptr(_), &Type::Int) => Some((ir::BinOpCode::PtrAdd, lhs_ty.clone())),
        _ => None
    }
}

pub fn unop_tyck(op: ast::UnOpCode, ty: &ir::Type) -> Option<(ir::UnOpCode, ir::Type)> {
    use ast::UnOpCode::*;
    use ir::Type;
    match (op, ty) {
        (Minus, &Type::Int) => Some((ir::UnOpCode::IntMinus, Type::Int)),
        (Minus, &Type::Double) => Some((ir::UnOpCode::DoubleMinus, Type::Double)),
        (LogicalNot, &Type::Bool) => Some((ir::UnOpCode::BoolLogicalNot, Type::Bool)),
        _ => None
    }
}
