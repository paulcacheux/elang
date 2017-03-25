use ir;
use ast;
use ast::{Span, Spanned};

pub fn build_translation_unit(tu: ast::TranslationUnit) -> Result<ir::TranslationUnit, SyntaxError> {
    let mut ir_builder = IRBuilder::new();

    let mut declarations = Vec::with_capacity(tu.declarations.len());
    for decl in tu.declarations {
        declarations.push(ir_builder.build_declaration(decl)?);
    }

    Ok(ir::TranslationUnit {
        declarations: declarations,
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxError {
    pub msg: String,
    pub span: Span,
}

struct SymbolTable {}

impl SymbolTable {
    fn new_scope(&mut self) {

    }

    fn end_scope(&mut self) {

    }

    fn insert(&mut self, name: String, ty: ir::Type) -> Result<(), SyntaxError> {
        // can return err if already decl in scope
        Ok(())
    }

    fn get_ty(&self, name: &String) -> Result<ir::Type, SyntaxError> {
        // can return err if not in scope
        Ok(ir::Type::Unit)
    }
}

struct IRBuilder {
    sym_table: SymbolTable,
    inside_loop: bool,
    current_ret_ty: ir::Type,
    temp_counter: usize,
}

impl IRBuilder {
    fn new() -> Self {
        IRBuilder {
            sym_table: SymbolTable {},
            inside_loop: false,
            current_ret_ty: ir::Type::Unit,
            temp_counter: 0,
        }
    }

    fn new_temp(&mut self) -> ir::Value {
        let v = ir::Value(self.temp_counter);
        self.temp_counter += 1;
        v
    }

    fn build_declaration(&mut self, decl: Spanned<ast::Declaration>) -> Result<ir::Declaration, SyntaxError> {
        match decl.inner {
            ast::Declaration::Function { name, params, return_ty, stmt } => {
                let mut param_names = Vec::with_capacity(params.len());
                let mut param_types = Vec::with_capacity(params.len());
                for (name, ty) in params {
                    param_names.push(name);
                    param_types.push(build_type(ty)?);
                }
                let ret_ty = build_type(return_ty)?;
                let ty = ir::Type::Function(Box::new(ret_ty.clone()), param_types.clone());

                self.current_ret_ty = ret_ty;
                self.sym_table.new_scope();
                self.sym_table.insert(name.clone(), ty.clone())?;
                for (name, ty) in param_names.iter().zip(param_types.iter()) {
                    self.sym_table.insert(name.clone(), ty.clone())?;
                }
                self.sym_table.new_scope();
                let stmts = self.build_statement(stmt)?;
                self.sym_table.end_scope();
                self.sym_table.end_scope();

                Ok(ir::Declaration::Function {
                    name: name,
                    ty: ty,
                    params: param_names,
                    stmt:
                        if stmts.len() == 1 {
                            stmts.into_iter().next().unwrap()
                        } else {
                            ir::Statement::Compound {
                                stmts: stmts
                            }
                        }
                })
            }
        }
    }

    fn build_statement(&mut self, stmt: Spanned<ast::Statement>) -> Result<Vec<ir::Statement>, SyntaxError> {
        match stmt.inner {
            ast::Statement::Compound { stmts } => {
                let mut ir_stmts = Vec::new();
                self.sym_table.new_scope();
                for stmt in stmts {
                    ir_stmts.extend(self.build_statement(stmt)?);
                }
                self.sym_table.end_scope();

                Ok(vec![ir::Statement::Compound {
                    stmts: ir_stmts
                }])
            },
            ast::Statement::Let { name, ty, expr } => {
                let expr_infos = self.build_expression(expr)?;
                let expr_infos = self.lvalue_to_rvalue(expr_infos);
                let mut stmts = expr_infos.stmts;

                let ty = if let Some(set_ty) = ty {
                    build_type(set_ty)?
                } else {
                    expr_infos.ty.clone()
                };

                if expr_infos.ty != ty {
                    return Err(SyntaxError {
                        msg: format!("Mismatching types in let."),
                        span: stmt.span,
                    })
                }

                self.sym_table.insert(name.clone(), ty)?;
                stmts.push(ir::Statement::VarDecl {
                    name: name,
                    value: expr_infos.value,
                });

                Ok(stmts)
            }
            ast::Statement::Loop { stmt } => {
                let was_inside_loop = self.inside_loop;
                self.inside_loop = true;
                self.sym_table.new_scope();
                let stmts = self.build_statement(*stmt)?;
                self.inside_loop = was_inside_loop;
                self.sym_table.end_scope();
                Ok(vec![ir::Statement::Loop {
                    stmt: Box::new(ir::Statement::Compound {
                        stmts: stmts
                    })
                }])
            },
            ast::Statement::While { cond, stmt: while_stmt } => {
                self.sym_table.new_scope();
                let cond_infos = self.build_expression(cond)?;
                let cond_infos = self.lvalue_to_rvalue(cond_infos);
                let mut stmts = cond_infos.stmts;

                if cond_infos.ty != ir::Type::Bool {
                    return Err(SyntaxError {
                        msg: format!("Expected 'bool' type in condition."),
                        span: stmt.span
                    })
                }

                let was_inside_loop = self.inside_loop;
                self.inside_loop = true;
                self.sym_table.new_scope();
                let while_stmts = self.build_statement(*while_stmt)?;
                self.inside_loop = was_inside_loop;
                self.sym_table.end_scope();
                self.sym_table.end_scope();

                stmts.push(ir::Statement::While {
                    cond: cond_infos.value,
                    stmt: Box::new(ir::Statement::Compound {
                        stmts: while_stmts
                    })
                });

                Ok(vec![
                    ir::Statement::Compound {
                        stmts: stmts
                    }
                ])
            },
            ast::Statement::If { if_branch, elseif_branches, else_branch } => {
                let mut stmts = Vec::new();
                self.sym_table.new_scope();
                let if_cond = self.build_expression(if_branch.0)?;
                let if_cond = self.lvalue_to_rvalue(if_cond);

                if if_cond.ty != ir::Type::Bool {
                    return Err(SyntaxError {
                        msg: format!("Expected 'bool' type in condition."),
                        span: stmt.span
                    })
                }

                stmts.extend(if_cond.stmts);

                self.sym_table.new_scope();
                let if_stmts = self.build_statement(*if_branch.1)?;
                self.sym_table.end_scope();

                self.sym_table.new_scope();
                let mut else_stmt = if let Some(else_stmt) = else_branch {
                    Some(Box::new(ir::Statement::Compound {
                        stmts: self.build_statement(*else_stmt)?
                    }))
                } else {
                    None
                };
                self.sym_table.end_scope();

                for (cond, stmt) in elseif_branches.into_iter().rev() {
                    self.sym_table.new_scope();
                    let cond = self.build_expression(cond)?;
                    let mut cond = self.lvalue_to_rvalue(cond);

                    if cond.ty != ir::Type::Bool {
                        return Err(SyntaxError {
                            msg: format!("Expected 'bool' type in condition."),
                            span: stmt.span
                        })
                    }

                    cond.stmts.push(ir::Statement::If {
                        cond: cond.value,
                        if_stmt: Box::new(ir::Statement::Compound {
                            stmts: self.build_statement(stmt)?
                        }),
                        else_stmt: else_stmt
                    });

                    else_stmt = Some(Box::new(ir::Statement::Compound {
                        stmts: cond.stmts
                    }));

                    self.sym_table.end_scope();
                }

                self.sym_table.end_scope();

                stmts.push(ir::Statement::If {
                    cond: if_cond.value,
                    if_stmt: Box::new(ir::Statement::Compound {
                        stmts: if_stmts
                    }),
                    else_stmt: else_stmt
                });

                Ok(vec![
                    ir::Statement::Compound {
                        stmts: stmts
                    }
                ])
            },
            ast::Statement::Break => {
                if self.inside_loop {
                    Ok(vec![ir::Statement::Break])
                } else {
                    Err(SyntaxError {
                        msg: format!("Break outside a loop"),
                        span: stmt.span
                    })
                }
            },
            ast::Statement::Continue => {
                if self.inside_loop {
                    Ok(vec![ir::Statement::Continue])
                } else {
                    Err(SyntaxError {
                        msg: format!("Continue outside a loop"),
                        span: stmt.span
                    })
                }
            },
            ast::Statement::Return { expr } => {
                let infos = self.build_expression(expr)?;
                let mut infos = self.lvalue_to_rvalue(infos);
                if infos.ty == self.current_ret_ty {
                    infos.stmts.push(ir::Statement::Return {
                        value: infos.value
                    });
                    Ok(infos.stmts)
                } else {
                    Err(SyntaxError {
                        msg: format!("Mismatching return type."), //TODO print expected ty
                        span: stmt.span,
                    })
                }
            },
            ast::Statement::Expression { expr } => {
                Ok(self.build_expression(expr)?.stmts)
            },
            ast::Statement::Print { expr } => {
                //TODO: check if printable (anything but fn ??)
                let infos = self.build_expression(expr)?;
                let mut infos = self.lvalue_to_rvalue(infos);
                infos.stmts.push(ir::Statement::Print {
                    value: infos.value
                });
                Ok(infos.stmts)
            },
        }
    }

    fn lvalue_to_rvalue(&mut self, infos: ExprInfos) -> ExprInfos {
        if let ir::Type::LValue(sub) = infos.ty {
            let mut stmts = infos.stmts;
            let value = self.new_temp();
            stmts.push(ir::Statement::Assign {
                dest: value,
                expr: ir::Expr::LValueToRValue(infos.value),
            });
            ExprInfos {
                stmts: stmts,
                value: value,
                ty: *sub,
            }
        } else {
            infos
        }
    }

    fn build_expression(&mut self, expr: Spanned<ast::Expression>) -> Result<ExprInfos, SyntaxError> {
        unimplemented!()
    }
}

fn build_type(parse_ty: Spanned<ast::ParseType>) -> Result<ir::Type, SyntaxError> {
    match parse_ty.inner {
        ast::ParseType::Unit => Ok(ir::Type::Unit),
        ast::ParseType::Array(sub) => Ok(ir::Type::Array(Box::new(build_type(*sub)?))),
        ast::ParseType::Ptr(sub) => Ok(ir::Type::Ptr(Box::new(build_type(*sub)?))),
        ast::ParseType::Lit(lit) => {
            match lit.as_str() {
                "int" => Ok(ir::Type::Int),
                "double" => Ok(ir::Type::Double),
                "bool" => Ok(ir::Type::Bool),
                other => Err(SyntaxError {
                    msg: format!("Unrecognized type '{}'.", other),
                    span: parse_ty.span
                }),
            }
        }
    }
}

struct ExprInfos {
    stmts: Vec<ir::Statement>,
    value: ir::Value,
    ty: ir::Type,
}
