use std::collections::HashMap;
use itertools::Itertools;

use ir;
use std::io::prelude::*;
use std::io;

pub fn gen_translation_unit<F: Write>(f: &mut F, tu: ir::TranslationUnit) -> io::Result<()> {
    let mut globals = HashMap::new();
    for decl in &tu.declarations {
        register_declaration(decl, &mut globals);
    }

    for declaration in tu.declarations {
        gen_declaration(f, declaration, &mut globals)?;
    }
    Ok(())
}

fn register_declaration(decl: &ir::Declaration, globals: &mut HashMap<String, ir::Type>) {
    match *decl {
        ir::Declaration::ExternFunction { ref name, ref ty } |
        ir::Declaration::Function { ref name, ref ty, .. } => {
            globals.insert(name.clone(), ir::Type::Function(ty.clone()));
        }
    }
}

fn gen_declaration<F: Write>(f: &mut F,
                             declaration: ir::Declaration,
                             globals: &mut HashMap<String, ir::Type>)
                             -> io::Result<()> {
    match declaration {
        ir::Declaration::ExternFunction { name, ty } => {
            writeln!(f,
                     "declare {} @{}({}{})",
                     type_to_string(*ty.return_ty),
                     name,
                     ty.params_ty.into_iter().map(type_to_string).join(", "),
                     if ty.variadic { ", ..." } else { "" })?;
        }
        ir::Declaration::Function {
            name,
            ty,
            locals,
            bbs,
        } => {
            writeln!(f,
                     "define {} @{}({}) {{\nentry:",
                     type_to_string(*ty.return_ty),
                     name,
                     ty.params_ty
                         .into_iter()
                         .map(type_to_string)
                         .enumerate()
                         .map(|(index, ty)| format!("{} %arg{}", ty, index))
                         .join(", "))?;

            let mut function_generator = FunctionGenerator {
                var_writer: Vec::new(),
                writer: Vec::new(),
                globals: globals,
                locals: HashMap::new(),
            };
            for local in locals {
                function_generator.gen_local(local)?;
            }

            let first_bb_id = bbs[0].id.0;
            for bb in bbs {
                function_generator.gen_basic_block(bb)?;
            }
            writeln!(function_generator.var_writer,
                     "\tbr label %bb{}",
                     first_bb_id)?;

            writeln!(f, "{}", function_generator.into_string())?;
            writeln!(f, "}}")?;
        }
    }
    Ok(())
}

struct FunctionGenerator<'a> {
    var_writer: Vec<u8>,
    writer: Vec<u8>,
    globals: &'a mut HashMap<String, ir::Type>,
    locals: HashMap<ir::LocalVarId, ir::Type>,
}

impl<'a> FunctionGenerator<'a> {
    fn into_string(self) -> String {
        format!("{}\n{}",
                String::from_utf8(self.var_writer).unwrap(),
                String::from_utf8(self.writer).unwrap())
    }

    fn gen_local(&mut self, local: ir::LocalVar) -> io::Result<()> {
        self.locals.insert(local.id, local.ty.clone());

        writeln!(self.var_writer,
                 "\t%local_{} = alloca {}, i32 {}",
                 local.id.0,
                 type_to_string(local.ty.clone()),
                 local.size)?;
        if let Some(index) = local.param_index {
            writeln!(self.var_writer,
                     "\tstore {0} %arg{1}, {0}* %local_{2}",
                     type_to_string(local.ty),
                     index,
                     local.id.0)?;
        }

        Ok(())
    }

    fn gen_basic_block(&mut self, bb: ir::BasicBlock) -> io::Result<()> {
        writeln!(self.writer, "bb{}:", bb.id.0)?;
        for stmt in bb.stmts {
            self.gen_statement(stmt)?;
        }

        self.gen_terminator(bb.terminator)?;
        Ok(())
    }

    fn gen_statement(&mut self, stmt: ir::Statement) -> io::Result<()> {
        match stmt {
            ir::Statement::LValueSet(dest, val) => {
                writeln!(self.writer,
                         "\tstore {} %temp_{}, {} %temp_{}",
                         type_to_string(val.ty),
                         val.id,
                         type_to_string(dest.ty),
                         dest.id)
            }
            ir::Statement::Assign(dest, expr) => {
                if dest.ty != ir::Type::Unit {
                    write!(self.writer, "\t%temp_{} = ", dest.id)?;
                } else {
                    write!(self.writer, "\t")?;
                }
                self.gen_expr(expr)?;
                writeln!(self.writer, "")
            }
        }
    }

    fn gen_expr(&mut self, expr: ir::Expression) -> io::Result<()> {
        match expr {
            ir::Expression::LocalVarLoad(id) => {
                let ty = self.locals[&id].clone();
                write!(self.writer,
                       "bitcast {0}* %local_{1} to {0}*",
                       type_to_string(ty),
                       id.0)
            }
            ir::Expression::GlobalLoad(name) => {
                let ty = self.globals[&name].clone();
                write!(self.writer,
                       "bitcast {0}* @{1} to {0}*",
                       type_to_string(ty),
                       name)
            }
            ir::Expression::LValueLoad(val) => {
                if let ir::Type::LValue(ty) = val.ty {
                    write!(self.writer,
                           "load {0}, {0}* %temp_{1}",
                           type_to_string(*ty),
                           val.id)
                } else {
                    unreachable!()
                }
            }
            ir::Expression::BinOp(op, lhs, rhs) => {
                if op == ir::BinOpCode::PtrAdd {
                    if let ir::Type::Ptr(sub_ty) = lhs.ty {
                        write!(self.writer,
                               "getelementptr {0}, {0}* %temp_{1}, {2} %temp_{3}",
                               type_to_string(*sub_ty),
                               lhs.id,
                               type_to_string(rhs.ty),
                               rhs.id)
                    } else {
                        unreachable!()
                    }
                } else {
                    use ir::BinOpCode::*;
                    let op = match op {
                        PtrAdd => unreachable!(),
                        IntAdd => "add i32",
                        DoubleAdd => "fadd double",
                        IntSub => "sub i32",
                        DoubleSub => "fsub double",
                        IntTimes => "mul i32",
                        DoubleTimes => "fmul double",
                        IntDivide => "sdiv i32",
                        DoubleDivide => "fdiv double",
                        IntMod => "srem i32",
                        IntLess => "icmp slt i32",
                        DoubleLess => "fcmp olt double",
                        IntLessEqual => "icmp sle i32",
                        DoubleLessEqual => "fcmp ole double",
                        IntGreater => "icmp sgt i32",
                        DoubleGreater => "fcmp ogt double",
                        IntGreaterEqual => "icmp sge i32",
                        DoubleGreaterEqual => "fcmp oge double",
                        IntEqual => "icmp eq i32",
                        BoolEqual => "icmp eq i1",
                        DoubleEqual => "fcmp oeq double",
                        IntNotEqual => "icmp ne i32",
                        BoolNotEqual => "icmp ne i1",
                        DoubleNotEqual => "fcmp one double",
                    };
                    write!(self.writer, "{} %temp_{}, %temp_{}", op, lhs.id, rhs.id)
                }
            }
            ir::Expression::UnOp(op, sub) => {
                use ir::UnOpCode::*;
                match op {
                    IntMinus => write!(self.writer, "sub i32 0, %temp_{}", sub.id),
                    DoubleMinus => write!(self.writer, "fsub double 0.0, %temp_{}", sub.id),
                    BoolLogicalNot => write!(self.writer, "xor i1 1, %temp_{}", sub.id),
                    // in fact we represent lvalue and ptr by ptr so AddressOf and PtrDeref are noop
                    AddressOf => {
                        if let ir::Type::LValue(ty) = sub.ty {
                            write!(self.writer,
                                   "getelementptr {0}, {0}* %temp_{1}, i64 0",
                                   type_to_string(*ty),
                                   sub.id)
                        } else {
                            unreachable!()
                        }
                    }
                    PtrDeref => {
                        if let ir::Type::Ptr(ty) = sub.ty {
                            write!(self.writer,
                                   "getelementptr {0}, {0}* %temp_{1}, i64 0",
                                   type_to_string(*ty),
                                   sub.id)
                        } else {
                            unreachable!()
                        }
                    }
                }
            }
            ir::Expression::CastOp(op, expr) => {
                use ir::CastCode::*;
                match op {
                    IntToDouble => {
                        write!(self.writer,
                               "sitofp {} %temp_{} to double",
                               type_to_string(expr.ty),
                               expr.id)
                    }
                    DoubleToInt => {
                        write!(self.writer,
                               "fptosi {} %temp_{} to i32",
                               type_to_string(expr.ty),
                               expr.id)
                    }
                    IntToChar => {
                        write!(self.writer,
                               "trunc {} %temp_{} to i8",
                               type_to_string(expr.ty),
                               expr.id)
                    }
                    CharToInt => {
                        write!(self.writer,
                               "zext {} %temp_{} to i32",
                               type_to_string(expr.ty),
                               expr.id)
                    }
                    IntToBool => {
                        write!(self.writer,
                               "icmp ne {} %temp_{}, 0",
                               type_to_string(expr.ty),
                               expr.id)
                    }
                    BoolToInt => {
                        write!(self.writer,
                               "zext {} %temp_{} to i32",
                               type_to_string(expr.ty),
                               expr.id)
                    }
                }
            }
            ir::Expression::FuncCall(func, params) => {
                let func_ty = match func.ty {
                    ir::Type::Ptr(sub_ty) => *sub_ty,
                    _ => unreachable!(),
                };

                write!(self.writer,
                       "call {} %temp_{}({})",
                       type_to_string(func_ty),
                       func.id,
                       params
                           .into_iter()
                           .map(|val| format!("{} %temp_{}", type_to_string(val.ty), val.id))
                           .join(", "))
            }
            ir::Expression::FieldAccess(st, index) => {
                let struct_ty = match st.ty {
                    ir::Type::Ptr(sub_ty) => *sub_ty,
                    _ => unreachable!(),
                };

                write!(self.writer,
                       "getelementptr {0}, {0}* %temp_{1}, i32 0, i32 {2}",
                       type_to_string(struct_ty),
                       st.id,
                       index)
            }
            ir::Expression::Literal(lit) => {
                match lit {
                    ir::Literal::Int(val) => {
                        write!(self.writer, "select i1 true, i32 {}, i32 0", val)
                    }
                    ir::Literal::Double(val) => {
                        write!(self.writer,
                               "select i1 true, double {:.10}, double 0.0",
                               val)
                    }
                    ir::Literal::Bool(val) => {
                        write!(self.writer,
                               "select i1 true, i1 {}, i1 0",
                               if val { 1 } else { 0 })
                    }
                    ir::Literal::Char(val) => {
                        write!(self.writer, "select i1 true, i8 {}, i8 0", val)
                    }
                    ir::Literal::Unit => Ok(()),

                }
            }
        }
    }

    fn gen_terminator(&mut self, term: ir::Terminator) -> io::Result<()> {
        match term {
            ir::Terminator::Br(id) => writeln!(self.writer, "\tbr label %bb{}", id.0),
            ir::Terminator::BrCond(value, id1, id2) => {
                writeln!(self.writer,
                         "\tbr i1 %temp_{}, label %bb{}, label %bb{}",
                         value.id,
                         id1.0,
                         id2.0)
            }
            ir::Terminator::Ret(value) => {
                if value.ty != ir::Type::Unit {
                    writeln!(self.writer,
                             "\tret {} %temp_{};",
                             type_to_string(value.ty),
                             value.id)
                } else {
                    writeln!(self.writer, "\tret void")
                }
            }
        }
    }
}

fn type_to_string(ty: ir::Type) -> String {
    match ty {
        ir::Type::Unit => "void".to_string(),
        ir::Type::Bool => "i1".to_string(), // cause c you know
        ir::Type::Int => "i32".to_string(),
        ir::Type::Double => "double".to_string(),
        ir::Type::Char => "i8".to_string(),
        ir::Type::LValue(sub) => format!("{}*", type_to_string(*sub)),
        ir::Type::Ptr(sub) => format!("{}*", type_to_string(*sub)),
        ir::Type::Function(func) => {
            format!("{}({}{})",
                    type_to_string(*func.return_ty),
                    func.params_ty
                        .into_iter()
                        .map(type_to_string)
                        .join(", "),
                    if func.variadic { ", ..." } else { "" })
        }
        ir::Type::Struct(st) => {
            format!("{{ {} }}",
                    st.fields_ty
                        .into_iter()
                        .map(|field| type_to_string(field.1))
                        .join(", "))
        }
    }
}
