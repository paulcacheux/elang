use ast::*;

pub fn print_ast(tu: &TranslationUnit) {
    println!("Translation Unit");
    let mut ast_printer = ASTPrinter::new();
    ast_printer.0 += 1;
    for stmt in &tu.stmts {
        ast_printer.print_statement(stmt);
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
        print!("({}:{}) ", span.0, span.1);
    }

    fn print_statement(&mut self, stmt: &Spanned<Statement>) {
        self.print_tab();
        self.print_span(&stmt.span);

        use self::Statement::*;
        match stmt.inner {
            FuncDecl { ref name, ref params, ref return_ty, ref stmt } => {
                println!("FuncDecl '{}' '{}'", name, return_ty);
                self.0 += 1;
                for param in params {
                    self.print_tab();
                    println!("ParamDecl '{}':'{}'", param.0, param.1);
                }
                self.print_statement(stmt);
                self.0 -= 1;
            },
            Compound { ref stmts } => {
                println!("CompoundStmt");
                self.0 += 1;
                for stmt in stmts {
                    self.print_statement(stmt);
                }
                self.0 -= 1;
            },
            Let { ref name, ref ty, ref expr } => {
                println!(
                    "LetStmt '{}' '{}'",
                    name,
                    ty.as_ref().map(|ty| ty.to_string()).unwrap_or(String::from("undefined"))
                );
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            },
            Loop { ref stmt } => {
                println!("LoopStmt");
                self.0 += 1;
                self.print_statement(stmt);
                self.0 -= 1;
            },
            While { ref cond, ref stmt } => {
                println!("WhileStmt");
                self.0 += 1;
                self.print_expression(cond);
                self.print_statement(stmt);
                self.0 -= 1;
            },
            If { ref ifs_branches, ref else_branch } => {
                println!("IfStmt");
                self.0 += 1;

                for &(ref cond, ref stmt) in ifs_branches {
                    self.print_tab();
                    println!("IfBranch");
                    self.0 += 1;
                    self.print_expression(cond);
                    self.print_statement(stmt);
                    self.0 -= 1;
                }

                if let &Some(ref stmt) = else_branch {
                    self.print_tab();
                    println!("ElseBranch");
                    self.0 += 1;
                    self.print_statement(stmt);
                    self.0 -= 1;
                }

                self.0 -= 1;
            },
            Break => {
                println!("BreakStmt");
            },
            Continue => {
                println!("ContinueStmt");
            },
            Return { ref expr } => {
                println!("ReturnStmt");
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            },
            Expression { ref expr } => {
                println!("ExprStmt");
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            },
            Print { ref expr } => {
                println!("PrintStmt");
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
            BinOp(op, ref lhs, ref rhs) => {
                println!("BinOpExpr '{:?}'", op);
                self.0 += 1;
                self.print_expression(lhs);
                self.print_expression(rhs);
                self.0 -= 1;
            },
            UnOp(op, ref expr) => {
                println!("UnOpExpr '{:?}'", op);
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            },
            FuncCall(ref func, ref args) => {
                println!("FuncCall");
                self.0 += 1;
                self.print_expression(func);
                for arg in args {
                    self.print_expression(arg);
                }
                self.0 -= 1;
            },
            Paren(ref expr) => {
                println!("ParenExpr");
                self.0 += 1;
                self.print_expression(expr);
                self.0 -= 1;
            },
            Identifier(ref name) => {
                println!("Identifier '{}'", name);
            },
            Number(val) => {
                println!("IntLit '{}'", val);
            }
        }
    }
}
