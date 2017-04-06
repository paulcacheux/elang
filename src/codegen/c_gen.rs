use ir;
use std::io::prelude::*;
use std::io;
use itertools::Itertools;

pub fn gen_translation_unit<F: Write>(f: &mut F, tu: ir::TranslationUnit) -> io::Result<()> {
    writeln!(f, "#include <stdlib.h>")?;
    writeln!(f, "#include <stdio.h>\n")?;

    writeln!(f, "void print_int(int a) {{")?;
    writeln!(f, "\tprintf(\"%d\\n\", a);")?;
    writeln!(f, "}}")?;

    for declaration in tu.declarations {
        gen_declaration(f, declaration)?;
    }
    Ok(())
}

fn gen_declaration<F: Write>(f: &mut F, declaration: ir::Declaration) -> io::Result<()> {
    match declaration {
        ir::Declaration::ExternFunction { name, ty } => {
            writeln!(f,
                     "extern {} {}({});",
                     type_to_string(*ty.return_ty),
                     name,
                     ty.params_ty.into_iter().map(type_to_string).join(", "))?;
        }
        ir::Declaration::Function {
            name,
            ty,
            locals,
            bbs,
        } => {
            writeln!(f,
                     "{} {}({}) {{",
                     type_to_string(*ty.return_ty),
                     name,
                     ty.params_ty
                         .into_iter()
                         .map(type_to_string)
                         .enumerate()
                         .map(|(index, ty)| format!("{} arg{}", ty, index))
                         .join(", "))?;

            for local in locals {
                gen_local(f, local)?;
            }
            writeln!(f, "")?;
            for bb in bbs {
                gen_basic_block(f, bb)?;
            }

            writeln!(f, "}}")?;
        }
    }
    Ok(())
}

fn gen_local<F: Write>(f: &mut F, local: ir::LocalVar) -> io::Result<()> {
    writeln!(f,
             "\t{}{};",
             type_to_string_with_name(local.ty, format!("local_{}", local.id.0)),
             local
                 .param_index
                 .map(|index| format!(" = arg{}", index))
                 .unwrap_or(String::new()))
}

fn gen_basic_block<F: Write>(f: &mut F, bb: ir::BasicBlock) -> io::Result<()> {
    writeln!(f, "bb{}: {{", bb.id.0)?;
    for stmt in bb.stmts {
        gen_statement(f, stmt)?;
    }

    gen_terminator(f, bb.terminator)?;
    writeln!(f, "}}")?;
    Ok(())
}

fn gen_statement<F: Write>(f: &mut F, stmt: ir::Statement) -> io::Result<()> {
    match stmt {
        ir::Statement::LValueSet(dest, val) => {
            writeln!(f, "\t*temp_{} = temp_{};", dest.id, val.id)
        }
        ir::Statement::Assign(dest, expr) => {
            if dest.ty != ir::Type::Unit {
                write!(f,
                       "\t{} = ",
                       type_to_string_with_name(dest.ty, format!("temp_{}", dest.id)))?;
            }
            gen_expr(f, expr)?;
            writeln!(f, ";")
        }
    }
}

fn gen_expr<F: Write>(f: &mut F, expr: ir::Expression) -> io::Result<()> {
    match expr {
        ir::Expression::LocalVarLoad(id) => write!(f, "&local_{}", id.0),
        ir::Expression::GlobalLoad(name) => write!(f, "{}", name),
        ir::Expression::LValueLoad(val) => write!(f, "*temp_{}", val.id),
        ir::Expression::BinOp(op, lhs, rhs) => {
            use ir::BinOpCode::*;
            let op = match op {
                PtrAdd | IntAdd | DoubleAdd => "+",
                IntSub | DoubleSub => "-",
                IntTimes | DoubleTimes => "*",
                IntDivide | DoubleDivide => "/",
                IntMod => "%",
                IntLess | DoubleLess => "<",
                IntLessEqual | DoubleLessEqual => "<=",
                IntGreater | DoubleGreater => ">",
                IntGreaterEqual | DoubleGreaterEqual => ">=",
                IntEqual | DoubleEqual | BoolEqual => "==",
                IntNotEqual | DoubleNotEqual | BoolNotEqual => "!=",
            };

            write!(f, "temp_{} {} temp_{}", lhs.id, op, rhs.id)
        }
        ir::Expression::UnOp(op, sub) => {
            use ir::UnOpCode::*;
            let op = match op {
                IntMinus | DoubleMinus => "-",
                BoolLogicalNot => "!",
                AddressOf => "&",
                PtrDeref => "*",
            };

            write!(f, "{}temp_{}", op, sub.id)
        }
        ir::Expression::CastOp(op, expr) => {
            use ir::CastCode::*;
            let target_ty = match op {
                IntToDouble => "double",
                DoubleToInt => "int",
                IntToChar => "char",
                CharToInt => "int",
                IntToBool => "bool",
                BoolToInt => "int",
            };
            write!(f, "(({})temp_{})", target_ty, expr.id)
        }
        ir::Expression::ReadArray(array, index) => {
            write!(f, "temp_{}[temp_{}]", array.id, index.id)
        }
        ir::Expression::FuncCall(func, params) => {
            write!(f,
                   "temp_{}({})",
                   func.id,
                   params
                       .into_iter()
                       .map(|val| format!("temp_{}", val.id))
                       .join(", "))
        }
        ir::Expression::Literal(lit) => {
            match lit {
                ir::Literal::Int(val) => write!(f, "{}", val),
                ir::Literal::Double(val) => write!(f, "{}", val),
                ir::Literal::Bool(val) => write!(f, "{}", if val { 1 } else { 0 }),
                ir::Literal::Char(val) => write!(f, "{}", val),
                ir::Literal::Unit => Ok(()),
            }
        }
    }
}

fn gen_terminator<F: Write>(f: &mut F, term: ir::Terminator) -> io::Result<()> {
    match term {
        ir::Terminator::Br(id) => writeln!(f, "\tgoto bb{};", id.0),
        ir::Terminator::BrCond(value, id1, id2) => {
            writeln!(f,
                     "\tif (temp_{}) {{ goto bb{}; }} else {{ goto bb{}; }}",
                     value.id,
                     id1.0,
                     id2.0)
        }
        ir::Terminator::Ret(value) => {
            if value.ty != ir::Type::Unit {
                writeln!(f, "\treturn temp_{};", value.id)
            } else {
                writeln!(f, "\treturn;")
            }
        },
    }
}

fn type_to_string_with_name(ty: ir::Type, name: String) -> String {
    match ty {
        ir::Type::Unit => format!("void {}", name),
        ir::Type::Bool => format!("int {}", name), // cause c you know
        ir::Type::Int => format!("int {}", name),
        ir::Type::Double => format!("double {}", name),
        ir::Type::Char => format!("char {}", name),
        ir::Type::LValue(sub) => format!("{} *{}", type_to_string(*sub), name),
        ir::Type::Array(sub, size) => format!("{} {}[{}]", type_to_string(*sub), name, size),
        ir::Type::Ptr(sub) => format!("{} *{}", type_to_string(*sub), name),
        ir::Type::Function(func) => {
            format!("{}(*{})({})",
                    type_to_string(*func.return_ty),
                    name,
                    func.params_ty
                        .into_iter()
                        .map(type_to_string)
                        .join(", "))
        }
    }
}

fn type_to_string(ty: ir::Type) -> String {
    match ty {
        ir::Type::Unit => format!("void"),
        ir::Type::Bool => format!("int"), // cause c you know
        ir::Type::Int => format!("int"),
        ir::Type::Double => format!("double"),
        ir::Type::Char => format!("char"),
        ir::Type::LValue(sub) => format!("{}*", type_to_string(*sub)),
        ir::Type::Array(sub, size) => format!("{}[{}]", type_to_string(*sub), size),
        ir::Type::Ptr(sub) => format!("{}*", type_to_string(*sub)),
        ir::Type::Function(func) => {
            format!("{}(*)({})",
                    type_to_string(*func.return_ty),
                    func.params_ty
                        .into_iter()
                        .map(type_to_string)
                        .join(", "))
        }
    }
}
