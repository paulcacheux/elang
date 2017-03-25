use std::collections::HashMap;

use ir;
use super::tyck;
use ast;
use ast::{Span, Spanned};

pub fn build_translation_unit(tu: ast::TranslationUnit) -> Result<ir::TranslationUnit, SyntaxError> {
    let mut ir_builder = IRBuilder::new();

    ir_builder.sym_table.new_scope();
    let mut declarations = Vec::with_capacity(tu.declarations.len());
    for decl in tu.declarations {
        declarations.push(ir_builder.build_declaration(decl)?);
    }
    ir_builder.sym_table.end_scope();

    Ok(ir::TranslationUnit {
        declarations: declarations,
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxError {
    pub msg: String,
    pub span: Span,
}

struct SymbolTable {
    scopes: Vec<HashMap<String, ir::Type>>,
}

impl SymbolTable {
    fn new_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, ty: ir::Type) -> bool { // return false if already on scope
        let scope = self.scopes.last_mut().unwrap();
        scope.insert(name, ty).is_none()
    }

    fn get_ty(&self, name: &String) -> Option<ir::Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
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
            sym_table: SymbolTable { scopes: Vec::new() },
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
                if !self.sym_table.insert(name.clone(), ty.clone()) {
                    return Err(SyntaxError {
                        msg: format!("'{}' is already defined in this scope.", name),
                        span: stmt.span,
                    })
                }
                self.sym_table.new_scope();
                for (name, ty) in param_names.iter().zip(param_types.iter()) {
                    if !self.sym_table.insert(name.clone(), ty.clone()) {
                        return Err(SyntaxError {
                            msg: format!("'{}' is already defined as a parameter.", name),
                            span: stmt.span,
                        })
                    }
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

                if !self.sym_table.insert(name.clone(), ty.clone()) {
                    return Err(SyntaxError {
                        msg: format!("Error '{}' is already defined in this scope.", name),
                        span: stmt.span,
                    })
                }

                stmts.push(ir::Statement::VarDecl {
                    name: name,
                    ty: ty,
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
        match expr.inner {
            ast::Expression::Assign(dest, assign_expr) => {
                let mut stmts = Vec::new();

                let assign_infos = self.build_expression(*assign_expr)?;
                let assign_infos = self.lvalue_to_rvalue(assign_infos);
                stmts.extend(assign_infos.stmts);

                let dest_infos = self.build_expression(*dest)?;
                stmts.extend(dest_infos.stmts);

                if let ir::Type::LValue(ty) = dest_infos.ty {
                    if *ty == assign_infos.ty {
                        stmts.push(ir::Statement::LValueSet {
                            lvalue: dest_infos.value,
                            rvalue: assign_infos.value,
                        });

                        Ok(ExprInfos {
                            stmts: stmts,
                            value: assign_infos.value,
                            ty: *ty,
                        })
                    } else {
                        Err(SyntaxError {
                            msg: format!("Mismatching types in assignment."),
                            span: expr.span,
                        })
                    }
                } else {
                    Err(SyntaxError {
                        msg: format!("Not assignable."),
                        span: expr.span
                    })
                }

            },
            ast::Expression::Subscript(array, index) => {
                let mut stmts = Vec::new();

                let array_infos = self.build_expression(*array)?;
                let array_infos = self.lvalue_to_rvalue(array_infos);
                stmts.extend(array_infos.stmts);

                let index_infos = self.build_expression(*index)?;
                let index_infos = self.lvalue_to_rvalue(index_infos);
                stmts.extend(index_infos.stmts);

                if let ir::Type::Array(ty) = array_infos.ty {
                    if let ir::Type::Int = index_infos.ty {
                        let val = self.new_temp();
                        stmts.push(ir::Statement::Assign {
                            dest: val,
                            expr: ir::Expr::Subscript(array_infos.value, index_infos.value),
                        });

                        Ok(ExprInfos {
                            stmts: stmts,
                            value: val,
                            ty: *ty
                        })
                    } else {
                        Err(SyntaxError {
                            msg: format!("Index must be an int."),
                            span: expr.span,
                        })
                    }
                } else {
                    Err(SyntaxError {
                        msg: format!("Not an array."),
                        span: expr.span,
                    })
                }
            },
            ast::Expression::BinOp(opcode, lhs, rhs) => {
                let mut stmts = Vec::new();

                let lhs_infos = self.build_expression(*lhs)?;
                let lhs_infos = self.lvalue_to_rvalue(lhs_infos);
                stmts.extend(lhs_infos.stmts);

                let rhs_infos = self.build_expression(*rhs)?;
                let rhs_infos = self.lvalue_to_rvalue(rhs_infos);
                stmts.extend(rhs_infos.stmts);

                if let Some((op, ty)) = tyck::binop_tyck(opcode, &lhs_infos.ty, &rhs_infos.ty) {
                    let val = self.new_temp();

                    stmts.push(ir::Statement::Assign {
                        dest: val,
                        expr: ir::Expr::BinOp(op, lhs_infos.value, rhs_infos.value),
                    });

                    Ok(ExprInfos {
                        stmts: stmts,
                        value: val,
                        ty: ty,
                    })

                } else {
                    Err(SyntaxError {
                        msg: format!("'{:?}' is not defined for those types.", opcode),
                        span: expr.span,
                    })
                }
            },
            ast::Expression::UnOp(opcode, sub_expr) => {
                let mut stmts = Vec::new();

                let sub_infos = self.build_expression(*sub_expr)?;
                let sub_infos = self.lvalue_to_rvalue(sub_infos);
                stmts.extend(sub_infos.stmts);

                if let Some((op, ty)) = tyck::unop_tyck(opcode, &sub_infos.ty) {
                    let val = self.new_temp();

                    stmts.push(ir::Statement::Assign {
                        dest: val,
                        expr: ir::Expr::UnOp(op, sub_infos.value),
                    });

                    Ok(ExprInfos {
                        stmts: stmts,
                        value: val,
                        ty: ty,
                    })
                } else {
                    Err(SyntaxError {
                        msg: format!("'{:?}' is not defined for this type.", opcode),
                        span: expr.span,
                    })
                }
            },
            ast::Expression::FuncCall(func, params) => {
                let mut stmts = Vec::new();
                let func_infos = self.build_expression(*func)?;
                let func_infos = self.lvalue_to_rvalue(func_infos);
                stmts.extend(func_infos.stmts);

                let mut provided_param_types = Vec::with_capacity(params.len());
                let mut param_values = Vec::with_capacity(params.len());
                for param in params {
                    let param_infos = self.build_expression(param)?;
                    let param_infos = self.lvalue_to_rvalue(param_infos);
                    stmts.extend(param_infos.stmts);
                    provided_param_types.push(param_infos.ty);
                    param_values.push(param_infos.value);
                }

                if let ir::Type::Function(ret, param_types) = func_infos.ty {
                    if param_types != provided_param_types {
                        return Err(SyntaxError {
                            msg: format!("Mismatching types in function call."),
                            span: expr.span,
                        })
                    }

                    let val = self.new_temp();
                    stmts.push(ir::Statement::Assign {
                        dest: val,
                        expr: ir::Expr::FuncCall(func_infos.value, param_values),
                    });

                    Ok(ExprInfos {
                        stmts: stmts,
                        value: val,
                        ty: *ret
                    })

                } else {
                    Err(SyntaxError {
                        msg: format!("Error not callable."),
                        span: expr.span,
                    })
                }

            },
            ast::Expression::Paren(sub_expr) => {
                self.build_expression(*sub_expr)
            },
            ast::Expression::Identifier(id) => {
                let ty = if let Some(ty) = self.sym_table.get_ty(&id) {
                    ty
                } else {
                    return Err(SyntaxError {
                        msg: format!("'{}' is undefined here.", id),
                        span: expr.span,
                    })
                };

                let val = self.new_temp();

                Ok(ExprInfos {
                    stmts: vec![ir::Statement::Assign {
                        dest: val,
                        expr: ir::Expr::LoadVar(id),
                    }],
                    value: val,
                    ty: ir::Type::LValue(Box::new(ty.clone())),
                })
            },
            ast::Expression::Literal(literal) => {
                let val = self.new_temp();
                let ty = match &literal {
                    &ast::Literal::Int(_) => ir::Type::Int,
                    &ast::Literal::Double(_) => ir::Type::Double,
                    &ast::Literal::Bool(_) => ir::Type::Bool,
                };

                Ok(ExprInfos {
                    stmts: vec![ir::Statement::Assign {
                        dest: val,
                        expr: ir::Expr::Literal(literal),
                    }],
                    value: val,
                    ty: ty,
                })
            },
        }
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
