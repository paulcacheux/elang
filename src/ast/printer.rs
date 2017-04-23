use ast::*;
use span::{Spanned, Span};

pub fn print_ast(tu: &TranslationUnit) {
    println!("Translation Unit");
    let mut ast_printer = ASTPrinter::new();
    ast_printer.0 += 1;
    for decl in &tu.declarations {
        ast_printer.print_declaration(decl);
    }
}

#[derive(Debug, Clone)]
struct ASTPrinter(usize);

impl ASTPrinter {
    fn new() -> Self {
        ASTPrinter(0)
    }

    fn print_tab(&self) {
        if self.0 != 0 {
            print!("{}\u{2514}\u{2500}\u{2500}", " ".repeat((self.0 - 1) * 2));
        }
    }

    fn print_span(&self, span: &Span) {
        print!("({}:{}) ", span.lo, span.hi);
    }

    fn print_declaration(&mut self, decl: &Spanned<Declaration>) {
        self.print_tab();
        self.print_span(&decl.span);

        use self::Declaration::*;
        match decl.inner {
            ExternFunction {
                ref name,
                ref params,
                ref variadic,
                ref return_ty,
            } => {
                println!("ExternFunctionDecl '{}' '{}' var:{:?}",
                         name,
                         return_ty.inner,
                         variadic);
                self.0 += 1;
                for param in params {
                    self.print_tab();
                    println!("ParamDecl '{}'", param.inner);
                }
                self.0 -= 1;
            }
            Function {
                ref name,
                ref params,
                ref return_ty,
                ref stmt,
            } => {
                println!("FunctionDecl '{}' '{}'", name, return_ty.inner);
                self.0 += 1;
                for param in params {
                    self.print_tab();
                    self.print_span(&param.0.span);
                    println!("ParamDecl '{}':'{}'", param.0.inner, param.1.inner);
                }
                self.print_compound_statement(stmt);
                self.0 -= 1;
            }
            Struct {
                ref name,
                ref fields,
            } => {
                println!("StructField '{}'", name);
                self.0 += 1;
                for field in fields {
                    self.print_tab();
                    self.print_span(&field.0.span);
                    println!("FieldDecl '{}':'{}'", field.0.inner, field.1.inner);
                }
                self.0 -= 1;
            }
        }
    }

    fn print_compound_statement(&mut self, cstmt: &Spanned<CompoundStatement>) {
        self.print_tab();
        println!("CompoundStmt");
        self.0 += 1;
        for stmt in &cstmt.inner.0 {
            self.print_statement(stmt);
        }
        self.0 -= 1;
    }

    fn print_statement(&mut self, stmt: &Spanned<Statement>) {
        self.print_tab();
        self.print_span(&stmt.span);

        use self::Statement::*;
        match stmt.inner {
            Compound(ref cstmt) => {
                self.0 += 1;
                self.print_compound_statement(cstmt);
                self.0 -= 1;
            }
            Let {
                ref name,
                ref ty,
                ref expr,
            } => {
                println!("LetStmt '{}' '{}'",
                         name,
                         ty.as_ref()
                             .map(|ty| ty.inner.to_string())
                             .unwrap_or_else(|| String::from("undefined")));
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            }
            Loop { ref stmt } => {
                println!("LoopStmt");
                self.0 += 1;
                self.print_compound_statement(stmt);
                self.0 -= 1;
            }
            While { ref cond, ref stmt } => {
                println!("WhileStmt");
                self.0 += 1;
                self.print_expression(cond);
                self.print_compound_statement(stmt);
                self.0 -= 1;
            }
            For {
                ref name,
                ref init_expr,
                ref cond_expr,
                ref step_expr,
                ref stmt,
            } => {
                println!("ForStmt '{}'", name);
                self.0 += 1;
                self.print_expression(init_expr);
                self.print_expression(cond_expr);
                self.print_expression(step_expr);
                self.print_compound_statement(stmt);
                self.0 -= 1;
            }
            If {
                ref if_branch,
                ref elseif_branches,
                ref else_branch,
            } => {
                println!("IfStmt");
                self.0 += 1;

                self.print_tab();
                println!("IfBranch");
                self.0 += 1;
                self.print_expression(&if_branch.0);
                self.print_compound_statement(&if_branch.1);
                self.0 -= 1;

                for &(ref cond, ref stmt) in elseif_branches {
                    self.print_tab();
                    println!("ElseIfBranch");
                    self.0 += 1;
                    self.print_expression(cond);
                    self.print_compound_statement(stmt);
                    self.0 -= 1;
                }

                if let Some(ref stmt) = *else_branch {
                    self.print_tab();
                    println!("ElseBranch");
                    self.0 += 1;
                    self.print_compound_statement(stmt);
                    self.0 -= 1;
                }

                self.0 -= 1;
            }
            Break => {
                println!("BreakStmt");
            }
            Continue => {
                println!("ContinueStmt");
            }
            Return { ref expr } => {
                println!("ReturnStmt");
                self.0 += 1;
                if let Some(ref expr) = *expr {
                    self.print_expression(expr);
                }
                self.0 -= 1;
            }
            Expression { ref expr } => {
                println!("ExprStmt");
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            }
        }
    }

    fn print_expression(&mut self, expr: &Spanned<Expression>) {
        self.print_tab();
        self.print_span(&expr.span);

        use self::Expression::*;
        match expr.inner {
            Assign(op, ref lhs, ref rhs) => {
                println!("AssignExpr '{:?}'",
                         op.map(|op| op.to_string())
                             .unwrap_or_else(|| "=".to_string()));
                self.0 += 1;
                self.print_expression(lhs);
                self.print_expression(rhs);
                self.0 -= 1;
            }
            Subscript(ref array, ref index) => {
                println!("SubscriptExpr");
                self.0 += 1;
                self.print_expression(array);
                self.print_expression(index);
                self.0 -= 1;
            }
            BinOp(op, ref lhs, ref rhs) => {
                println!("BinOpExpr '{:?}'", op);
                self.0 += 1;
                self.print_expression(lhs);
                self.print_expression(rhs);
                self.0 -= 1;
            }
            UnOp(op, ref expr) => {
                println!("UnOpExpr '{:?}'", op);
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            }
            FuncCall(ref func, ref args) => {
                println!("FuncCall");
                self.0 += 1;
                self.print_expression(func);
                for arg in args {
                    self.print_expression(arg);
                }
                self.0 -= 1;
            }
            Cast(ref expr, ref target_ty) => {
                println!("CastOp to '{}'", target_ty.inner);
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            }
            FieldAccess(ref expr, ref field) => {
                println!("FieldAccess '{}'", field.inner);
                self.0 +=  1;
                self.print_expression(expr);
                self.0 -= 1;
            }
            Paren(ref expr) => {
                println!("ParenExpr");
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            }
            Identifier(ref name) => {
                println!("Identifier '{}'", name);
            }
            Literal(ref lit) => {
                use self::Literal;
                match *lit {
                    Literal::Int(val) => println!("IntLit '{}'", val),
                    Literal::Double(val) => println!("DoubleLit '{}'", val),
                    Literal::Bool(val) => println!("BoolLit '{:?}'", val),
                    Literal::Char(ref val) => println!("CharLit '{}'", val),
                    Literal::Unit => println!("UnitLit"),
                }
            }
            StringLiteral(ref lit) => {
                println!("StringLit {:?}", lit);
            }
            ArrayFullLiteral(ref values) => {
                println!("ArrayFullLiteral");
                self.0 += 1;
                for expr in values {
                    self.print_expression(expr);
                }
                self.0 -= 1;
            }
            ArrayDefaultLiteral(ref def, size) => {
                println!("ArrayDefaultLiteral (size: {})", size);
                self.0 += 1;
                self.print_expression(def);
                self.0 -= 1;
            }
            StructLiteral(ref struct_lit) => {
                println!("StructLiteral '{}'", struct_lit.name);
                self.0 += 1;
                for field in &struct_lit.fields {
                    self.print_tab();
                    self.print_span(&field.span);
                    println!("Field '{}'", field.inner.0);
                    self.0 += 1;
                    self.print_expression(&field.inner.1);
                    self.0 -= 1;
                }
                self.0 -= 1;
            }
        }
    }
}
