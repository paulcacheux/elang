use ast::*;

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
        print!("({}:{}) ", span.0, span.1);
    }

    fn print_declaration(&mut self, decl: &Spanned<Declaration>) {
        self.print_tab();
        self.print_span(&decl.span);

        use self::Declaration::*;
        match decl.inner {
            Function { ref name, ref params, ref return_ty, ref stmt } => {
                println!("FunctionDecl '{}' '{}'", name, return_ty.inner);
                self.0 += 1;
                for param in params {
                    self.print_tab();
                    println!("ParamDecl '{}':'{}'", param.0, param.1.inner);
                }
                self.print_statement(stmt);
                self.0 -= 1;
            },
        }
    }

    fn print_statement(&mut self, stmt: &Spanned<Statement>) {
        self.print_tab();
        self.print_span(&stmt.span);

        use self::Statement::*;
        match stmt.inner {
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
                    ty.as_ref().map(|ty| ty.inner.to_string()).unwrap_or(String::from("undefined"))
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
            If { ref if_branch, ref elseif_branches, ref else_branch } => {
                println!("IfStmt");
                self.0 += 1;

                self.print_tab();
                println!("IfBranch");
                self.0 += 1;
                self.print_expression(&if_branch.0);
                self.print_statement(&if_branch.1);
                self.0 -= 1;

                for &(ref cond, ref stmt) in elseif_branches {
                    self.print_tab();
                    println!("ElseIfBranch");
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
            Assign(ref lhs, ref rhs) => {
                println!("AssignExpr");
                self.0 += 1;
                self.print_expression(lhs);
                self.print_expression(rhs);
                self.0 -= 1;
            },
            Subscript(ref array, ref index) => {
                println!("SubscriptExpr");
                self.0 += 1;
                self.print_expression(array);
                self.print_expression(index);
                self.0 -= 1;
            },
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
            Literal(ref lit) => {
                use self::Literal;
                match *lit {
                    Literal::Int(val) => println!("IntLit '{}'", val),
                    Literal::Double(val) => println!("DoubleLit '{}'", val),
                    Literal::Bool(val) => println!("BoolLit '{:?}'", val),
                }
            }
        }
    }
}
