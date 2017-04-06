use ir::*;

pub fn print_ir(tu: &TranslationUnit) {
    for decl in &tu.declarations {
        print_declaration(decl);
    }
}

fn print_declaration(decl: &Declaration) {
    match *decl {
        Declaration::ExternFunction { ref name, ref ty } => {
            println!("extern @{} {}\n", name, ty);
        }
        Declaration::Function {
            ref name,
            ref ty,
            ref locals,
            ref bbs,
        } => {
            println!("declare @{} {} {{", name, ty);

            for local in locals {
                print_local_var(local);
            }

            for bb in bbs {
                print_basic_block(bb);
            }
            println!("}}\n");
        }
    }
}

fn print_local_var(var: &LocalVar) {
    print!("\tlet ${}: {}", var.id.0, var.ty);
    if let Some(index) = var.param_index {
        println!(" = arg{};", index);
    } else {
        println!(";");
    }
}

fn print_basic_block(bb: &BasicBlock) {
    println!("#bb{}:", bb.id.0);
    for stmt in &bb.stmts {
        print_stmt(stmt);
    }

    print_terminator(&bb.terminator);
}

fn print_stmt(stmt: &Statement) {
    match *stmt {
        Statement::LValueSet(ref lhs, ref rhs) => {
            print!("\tlvalue_set(");
            print_value(lhs);
            print!(", ");
            print_value(rhs);
            println!(");");
        }
        Statement::Assign(ref lhs, ref expr) => {
            print!("\t");
            print_value(lhs);
            print!(" = ");
            print_expr(expr);
            println!(";");
        }
    }
}

fn print_expr(expr: &Expression) {
    match *expr {
        Expression::LocalVarLoad(id) => print!("local_var_load(${})", id.0),
        Expression::GlobalLoad(ref name) => print!("global_var_load(\"{}\")", name),
        Expression::LValueLoad(ref val) => {
            print!("lvalue_load(");
            print_value(val);
            print!(")");
        }
        Expression::BinOp(op, ref lhs, ref rhs) => {
            print!("binop({:?}, ", op);
            print_value(lhs);
            print!(", ");
            print_value(rhs);
            print!(")");
        }
        Expression::UnOp(op, ref val) => {
            print!("unop({:?}, ", op);
            print_value(val);
            print!(")");
        }
        Expression::CastOp(op, ref val) => {
            print!("castop({:?}, ", op);
            print_value(val);
            print!(")");
        }
        Expression::ReadArray(ref array, ref index) => {
            print!("readarray(");
            print_value(array);
            print!(", ");
            print_value(index);
            print!(")");
        }
        Expression::FuncCall(ref func, ref params) => {
            print!("call ");
            print_value(func);
            print!("(");
            for param in params {
                print_value(param);
                print!(", ");
            }
            print!(")")
        }
        Expression::Literal(ref lit) => {
            match *lit {
                Literal::Int(val) => print!("IntLit '{}'", val),
                Literal::Double(val) => print!("DoubleLit '{}'", val),
                Literal::Bool(val) => print!("BoolLit '{:?}'", val),
                Literal::Char(val) => print!("CharLit '{}' '{}'", val, val as char),
                Literal::Unit => print!("UnitLit"),
            }
        }
    }
}

fn print_terminator(term: &Terminator) {
    match *term {
        Terminator::Br(id) => println!("\tbr #bb{};", id.0),
        Terminator::BrCond(ref val, idt, idf) => {
            print!("\tbr_cond ");
            print_value(val);
            println!(", #bb{}, #bb{};", idt.0, idf.0);
        }
        Terminator::Ret(ref val) => {
            print!("\tret ");
            print_value(val);
            println!(";");
        }
    }
}

fn print_value(val: &Value) {
    print!("_{}: {}", val.id, val.ty);
}
