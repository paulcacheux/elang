use std::collections::HashMap;
use ir;
use ast;
use ast::{Span, Spanned};
use ir::tyck;


#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub msg: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    globals: HashMap<String, ir::Type>,
    locals: Vec<HashMap<String, (ir::LocalVarId, ir::Type)>>,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            globals: HashMap::new(),
            locals: Vec::new(),
        }
    }

    fn start_local_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    fn end_local_scope(&mut self) {
        self.locals.pop();
    }

    fn register_local(&mut self, name: String, ty: ir::Type, id: ir::LocalVarId) -> bool { // return false if already on scope
        self.locals.last_mut().unwrap().insert(name, (id, ty)).is_none()
    }

    fn register_global(&mut self, name: String, ty: ir::Type) -> bool {
        self.globals.insert(name, ty).is_none()
    }

    fn get_var(&self, name: &String) -> Option<(ir::Type, ir::Expression)> {
        for scope in self.locals.iter().rev() {
            if let Some(&(ref id, ref ty)) = scope.get(name) {
                return Some((ir::Type::LValue(Box::new(ty.clone())), ir::Expression::LocalVarLoad(id.clone())));
            }
        }
        if let Some(ty) = self.globals.get(name) {
            Some((ty.clone(), ir::Expression::GlobalLoad(name.clone())))
        } else {
            None
        }
    }
}

pub fn build_translation_unit(tu: ast::TranslationUnit) -> Result<ir::TranslationUnit, SyntaxError> {
    let mut symbol_table = SymbolTable::new();

    let mut declarations = Vec::with_capacity(tu.declarations.len());
    for decl in tu.declarations {
        declarations.push(build_declaration(decl, &mut symbol_table)?);
    }

    Ok(ir::TranslationUnit {
        declarations: declarations
    })
}

fn build_declaration(decl: Spanned<ast::Declaration>, symbol_table: &mut SymbolTable) -> Result<ir::Declaration, SyntaxError> {
    match decl.inner {
        ast::Declaration::ExternFunction { name, params, return_ty } => {
            let return_ty = build_type(return_ty)?;

            let mut param_types = Vec::with_capacity(params.len());
            for ty in params {
                param_types.push(build_type(ty)?);
            }

            let ty = ir::FunctionType { return_ty: Box::new(return_ty), params_ty: param_types };

            if !symbol_table.register_global(name.clone(), ir::Type::Function(ty.clone())) {
                return Err(SyntaxError {
                    msg: format!("'{}' function is already defined.", name),
                    span: decl.span,
                })
            }

            Ok(ir::Declaration::ExternFunction {
                name: name,
                ty: ty,
            })
        },
        ast::Declaration::Function { name, params, return_ty, stmt } => {
            let return_ty = build_type(return_ty)?;

            let mut param_names = Vec::with_capacity(params.len());
            let mut param_types = Vec::with_capacity(params.len());
            for (name, ty) in params {
                param_names.push(name);
                param_types.push(build_type(ty)?);
            }

            let ty = ir::FunctionType { return_ty: Box::new(return_ty), params_ty: param_types };

            if !symbol_table.register_global(name.clone(), ir::Type::Function(ty.clone())) {
                return Err(SyntaxError {
                    msg: format!("'{}' function is already defined.", name),
                    span: decl.span,
                })
            }

            let mut function_builder = FunctionBuilder::new(name, ty.clone(), symbol_table);
            function_builder.symbol_table.start_local_scope();
            for (index, (name, ty)) in param_names.into_iter().zip(ty.params_ty).enumerate() {
                if !function_builder.register_param(name.clone(), ty, Some(index)) {
                    return Err(SyntaxError {
                        msg: format!("'{}' is already defined.", name),
                        span: decl.span,
                    })
                }
            }

            build_compound_statement(&mut function_builder, stmt)?;
            function_builder.symbol_table.end_local_scope();

            Ok(function_builder.to_function())
        }
    }
}

fn build_compound_statement(fb: &mut FunctionBuilder, stmt: Spanned<ast::CompoundStatement>) -> Result<(), SyntaxError> {
    fb.symbol_table.start_local_scope();
    for s in stmt.inner.0 {
        build_statement(fb, s)?;
    }
    fb.symbol_table.end_local_scope();
    Ok(())
}

fn build_statement(fb: &mut FunctionBuilder, stmt: Spanned<ast::Statement>) -> Result<(), SyntaxError> {
    match stmt.inner {
        ast::Statement::Compound(c) => build_compound_statement(fb, c),
        ast::Statement::Let { name, ty, expr } => {
            let expr_value = build_expression(fb, expr)?;
            let expr_value = build_lvalue_to_rvalue(fb, expr_value);

            let ty = if let Some(ty) = ty {
                build_type(ty)?
            } else {
                expr_value.ty.clone()
            };

            if ty == expr_value.ty {
                if !fb.register_local_variable(name.clone(), ty.clone()) {
                    return Err(SyntaxError {
                        msg: format!("'{}' is already defined in this scope.", name),
                        span: stmt.span,
                    })
                }

                let (_, lval_expr) = fb.symbol_table.get_var(&name).unwrap(); //TODO optimize
                let lvalue = ir::Value { id: fb.new_temp_id(), ty: ir::Type::LValue(Box::new(ty)) };
                fb.add_statement(ir::Statement::Assign(lvalue.clone(), lval_expr));
                fb.add_statement(ir::Statement::LValueSet(lvalue, expr_value));
                Ok(())
            } else {
                Err(SyntaxError {
                    msg: format!("Mismatching assignment types."),
                    span: stmt.span,
                })
            }
        },
        ast::Statement::Loop { stmt } => {
            let continue_id = fb.bb_counter;
            fb.terminate_current(None);
            let stmt_index = fb.current_bb_index;
            fb.terminate_current(None);
            let break_id = fb.bb_counter;
            fb.terminate_current(Some(ir::Terminator::Jmp(ir::BasicBlockId(continue_id))));

            fb.current_bb_index = stmt_index;
            let old_loop_infos = fb.current_loop_info.clone();
            fb.current_loop_info = Some((ir::BasicBlockId(continue_id), ir::BasicBlockId(break_id)));
            build_compound_statement(fb, stmt)?;
            fb.current_loop_info = old_loop_infos;

            fb.cursor_to_end();
            Ok(())
        },
        ast::Statement::While { cond, stmt } => {
            let continue_id = fb.bb_counter;
            fb.terminate_current(None);
            let cond_index = fb.current_bb_index;
            fb.terminate_current(None);
            let stmt_index = fb.current_bb_index;
            fb.terminate_current(None);
            let break_id = fb.bb_counter;
            fb.terminate_current(Some(ir::Terminator::Jmp(ir::BasicBlockId(continue_id))));

            fb.symbol_table.start_local_scope();
            fb.current_bb_index = cond_index;
            let cond_value = build_expression(fb, cond)?; //TODO tyck
            fb.change_terminator(Some(ir::Terminator::Jz(cond_value, ir::BasicBlockId(break_id))));

            fb.current_bb_index = stmt_index;
            let old_loop_infos = fb.current_loop_info.clone();
            fb.current_loop_info = Some((ir::BasicBlockId(continue_id), ir::BasicBlockId(break_id)));
            build_compound_statement(fb, stmt)?;
            fb.current_loop_info = old_loop_infos;

            fb.symbol_table.end_local_scope();
            fb.cursor_to_end();
            Ok(())
        },
        ast::Statement::If { if_branch, elseif_branches, else_branch } => {
            let branches = vec![if_branch].into_iter().chain(elseif_branches);
            let mut finalizer_indexes = Vec::new();

            fb.symbol_table.start_local_scope();
            for branch in branches {
                fb.terminate_current(None);
                let cond_value = build_expression(fb, branch.0)?;
                let cond_index = fb.current_bb_index;
                fb.terminate_current(None);

                build_compound_statement(fb, branch.1)?;
                let else_index = fb.current_bb_index;
                finalizer_indexes.push(else_index);
                fb.terminate_current(None);
                let else_id = fb.bb_counter;

                fb.current_bb_index = cond_index;
                fb.change_terminator(Some(ir::Terminator::Jz(cond_value, ir::BasicBlockId(else_id))));

                fb.current_bb_index = else_index;
            }
            fb.terminate_current(None);

            if let Some(stmt) = else_branch {
                build_compound_statement(fb, stmt)?;
            }
            fb.terminate_current(None);
            finalizer_indexes.push(fb.current_bb_index);

            let end_index = fb.bb_counter;
            fb.terminate_current(None);

            for index in finalizer_indexes {
                fb.current_bb_index = index;
                fb.change_terminator(Some(ir::Terminator::Jmp(ir::BasicBlockId(end_index))));
            }

            fb.symbol_table.end_local_scope();

            fb.cursor_to_end();
            Ok(())
        },
        ast::Statement::Break => {
            if let Some((_, id)) = fb.current_loop_info.clone() {
                fb.terminate_current(Some(ir::Terminator::Jmp(id)));
                Ok(())
            } else {
                Err(SyntaxError {
                    msg: format!("Break outside loop."),
                    span: stmt.span,
                })
            }
        }
        ast::Statement::Continue => {
            if let Some((id, _)) = fb.current_loop_info.clone() {
                fb.terminate_current(Some(ir::Terminator::Jmp(id)));
                Ok(())
            } else {
                Err(SyntaxError {
                    msg: format!("Continue outside loop."),
                    span: stmt.span,
                })
            }
        }
        ast::Statement::Return { expr } => {
            let value = if let Some(expr) = expr {
                let value = build_expression(fb, expr)?;
                let value = build_lvalue_to_rvalue(fb, value);
                value
            } else {
                let value = ir::Value { id: fb.new_temp_id(), ty: ir::Type::Unit };
                fb.add_statement(ir::Statement::Assign(value.clone(), ir::Expression::Literal(ast::Literal::Unit)));
                value
            };

            if value.ty == *fb.ty.return_ty {
                fb.terminate_current(Some(ir::Terminator::Ret(value)));
                Ok(())
            } else {
                Err(SyntaxError {
                    msg: format!("Mismatching return type."),
                    span: stmt.span
                })
            }
        }
        ast::Statement::Expression { expr } => {
            build_expression(fb, expr)?;
            Ok(())
        }
    }
}

fn build_expression(fb: &mut FunctionBuilder, expr: Spanned<ast::Expression>) -> Result<ir::Value, SyntaxError> {
    match expr.inner {
        ast::Expression::Assign(lhs, rhs) => {
            let lhs_value = build_expression(fb, *lhs)?;
            let rhs_value = build_expression(fb, *rhs)?;
            let rhs_value = build_lvalue_to_rvalue(fb, rhs_value);

            if let ir::Type::LValue(sub) = lhs_value.ty.clone() {
                if *sub == rhs_value.ty.clone() {
                    fb.add_statement(ir::Statement::LValueSet(lhs_value, rhs_value.clone()));
                    Ok(rhs_value)
                } else {
                    Err(SyntaxError {
                        msg: format!("Mismtach assignment."),
                        span: expr.span,
                    })
                }
            } else {
                Err(SyntaxError {
                    msg: format!("Can't assign to a non-lvalue."),
                    span: expr.span,
                })
            }
        },
        ast::Expression::Subscript(array, index) => {
            let array_value = build_expression(fb, *array)?;
            let array_value = build_lvalue_to_rvalue(fb, array_value);
            let index_value = build_expression(fb, *index)?;
            let index_value = build_lvalue_to_rvalue(fb, index_value);

            if let ir::Type::Array(sub) = array_value.ty.clone() {
                if ir::Type::Int == index_value.ty {
                    let value = ir::Value { id: fb.new_temp_id(), ty: ir::Type::LValue(sub) };
                    fb.add_statement(ir::Statement::Assign(value.clone(), ir::Expression::ReadArray(array_value, index_value)));
                    Ok(value)
                } else {
                    Err(SyntaxError {
                        msg: format!("Index must be of int type."),
                        span: expr.span,
                    })
                }
            } else {
                Err(SyntaxError {
                    msg: format!("Subscript to a non-array."),
                    span: expr.span,
                })
            }
        },
        ast::Expression::BinOp(code, lhs, rhs) => {
            let lhs_value = build_expression(fb, *lhs)?;
            let lhs_value = build_lvalue_to_rvalue(fb, lhs_value);
            let rhs_value = build_expression(fb, *rhs)?;
            let rhs_value = build_lvalue_to_rvalue(fb, rhs_value);

            if let Some((op, ty)) = tyck::binop_tyck(code, &lhs_value.ty, &rhs_value.ty) {
                let value = ir::Value { id: fb.new_temp_id(), ty: ty };

                fb.add_statement(ir::Statement::Assign(value.clone(), ir::Expression::BinOp(op, lhs_value, rhs_value)));
                Ok(value)
            } else {
                Err(SyntaxError {
                    msg: format!("Operation mismatching for those types."),
                    span: expr.span,
                })
            }
        },
        ast::Expression::UnOp(code, sub) => {
            let sub_value = build_expression(fb, *sub)?;
            let sub_value = build_lvalue_to_rvalue(fb, sub_value);

            if let Some((op, ty)) = tyck::unop_tyck(code, &sub_value.ty) {
                let value = ir::Value { id: fb.new_temp_id(), ty: ty };

                fb.add_statement(
                    ir::Statement::Assign(
                        value.clone(),
                        ir::Expression::UnOp(op, sub_value)
                    )
                );
                Ok(value)
            } else {
                Err(SyntaxError {
                    msg: format!("Operation mismatching for those types."),
                    span: expr.span,
                })
            }
        },
        ast::Expression::FuncCall(func, params) => {
            let func_value = build_expression(fb, *func)?;
            let func_value = build_lvalue_to_rvalue(fb, func_value);

            if let ir::Type::Function(func_ty) = func_value.ty.clone() {
                let mut param_ty = Vec::new();
                let mut param_values = Vec::new();
                for param in params {
                    let param = build_expression(fb, param)?;
                    let param = build_lvalue_to_rvalue(fb, param);

                    param_ty.push(param.ty.clone());
                    param_values.push(param);
                }

                if param_ty == func_ty.params_ty {
                    let value = ir::Value { id: fb.new_temp_id(), ty: *func_ty.return_ty };
                    fb.add_statement(
                        ir::Statement::Assign(
                            value.clone(),
                            ir::Expression::FuncCall(func_value, param_values)
                        )
                    );
                    Ok(value)
                } else {
                    Err(SyntaxError {
                        msg: format!("Mismatching params."),
                        span: expr.span
                    })
                }
            } else {
                Err(SyntaxError {
                    msg: format!("Not callable."),
                    span: expr.span
                })
            }
        },
        ast::Expression::Paren(expr) => build_expression(fb, *expr),
        ast::Expression::Identifier(id) => {
            if let Some((ty, expr)) = fb.symbol_table.get_var(&id) {
                let value = ir::Value { id: fb.new_temp_id(), ty: ty };
                fb.add_statement(ir::Statement::Assign(value.clone(), expr));
                Ok(value)
            } else {
                Err(SyntaxError {
                    msg: format!("'{}' is not defined here.", id),
                    span: expr.span,
                })
            }
        },
        ast::Expression::Literal(lit) => {
            let ty = match lit {
                ast::Literal::Int(_) => ir::Type::Int,
                ast::Literal::Double(_) => ir::Type::Double,
                ast::Literal::Bool(_) => ir::Type::Bool,
                ast::Literal::Unit => ir::Type::Unit,
            };

            let value = ir::Value { id: fb.new_temp_id(), ty: ty };
            fb.add_statement(ir::Statement::Assign(value.clone(), ir::Expression::Literal(lit)));
            Ok(value)
        }
    }
}

fn build_lvalue_to_rvalue(fb: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    if let ir::Type::LValue(sub) = value.ty.clone() {
        let id = fb.new_temp_id();
        let rvalue = ir::Value { id: id, ty: *sub };
        fb.add_statement(
            ir::Statement::Assign(
                rvalue.clone(),
                ir::Expression::LValueLoad(value)
            )
        );
        rvalue
    } else {
        value
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

#[derive(Debug)]
struct FunctionBuilder<'a> {
    name: String,
    ty: ir::FunctionType,
    symbol_table: &'a mut SymbolTable,
    locals: Vec<ir::LocalVar>,
    local_counter: usize,
    basic_blocks: Vec<ir::BasicBlock>,
    current_bb_index: usize,
    bb_counter: usize,
    current_temp_id: usize,
    current_loop_info: Option<(ir::BasicBlockId, ir::BasicBlockId)> //(continue, break)
}

impl<'a> FunctionBuilder<'a> {
    fn new(name: String, ty: ir::FunctionType, st: &'a mut SymbolTable) -> Self {
        FunctionBuilder {
            name: name,
            ty: ty,
            symbol_table: st,
            locals: Vec::new(),
            local_counter: 0,
            basic_blocks: vec![ir::BasicBlock { id: ir::BasicBlockId(0), stmts: Vec::new(), terminator: None }],
            current_bb_index: 0,
            bb_counter: 1,
            current_temp_id: 0,
            current_loop_info: None
        }
    }

    fn to_function(self) -> ir::Declaration {
        ir::Declaration::Function {
            name: self.name,
            ty: self.ty,
            locals: self.locals,
            bbs: self.basic_blocks,
        }
    }

    fn new_temp_id(&mut self) -> usize {
        let id = self.current_temp_id;
        self.current_temp_id += 1;
        id
    }

    fn register_param(&mut self, name: String, ty: ir::Type, param_index: Option<usize>) -> bool {
        let res = self.symbol_table.register_local(name, ty.clone(), ir::LocalVarId(self.local_counter));
        self.locals.push(ir::LocalVar {
            id: ir::LocalVarId(self.local_counter),
            ty: ty,
            param_index: param_index,
        });
        self.local_counter += 1;
        res
    }

    fn register_local_variable(&mut self, name: String, ty: ir::Type) -> bool {
        self.register_param(name, ty, None)
    }

    fn cursor_to_end(&mut self) {
        self.current_bb_index = self.basic_blocks.len() - 1;
    }

    fn add_statement(&mut self, stmt: ir::Statement) {
        self.basic_blocks[self.current_bb_index].stmts.push(stmt);
    }

    fn change_terminator(&mut self, terminator: Option<ir::Terminator>) {
        self.basic_blocks[self.current_bb_index].terminator = terminator;
    }

    fn terminate_current(&mut self, terminator: Option<ir::Terminator>) {
        self.change_terminator(terminator);
        self.basic_blocks.insert(self.current_bb_index + 1, ir::BasicBlock {
            id: ir::BasicBlockId(self.bb_counter),
            stmts: Vec::new(),
            terminator: None
        });
        self.current_bb_index += 1;
        self.bb_counter += 1;
    }
}
