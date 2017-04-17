use ir;
use ast;
use span::{Spanned, Span};
use pipeline;

use ir::GlobalTable;
use semantic_error::{SemanticError, SemanticErrorKind};

mod typecheck_defs;
mod function_builder;
use self::function_builder::FunctionBuilder;

use rayon::prelude::*;

pub fn build_translation_unit(tu: ast::TranslationUnit,
                              globals_table: &mut GlobalTable,
                              options: &pipeline::CompileOptions)
                              -> Result<ir::TranslationUnit, SemanticError> {
    let mut declarations = Vec::new();

    for import in tu.imports {
        let path = pipeline::build_path(&import, options);
        let imported_tu = pipeline::process_path(path, options, globals_table);
        declarations.extend(imported_tu.declarations);
    }

    let mut predeclarations = Vec::with_capacity(tu.declarations.len());
    for decl in tu.declarations {
        if let Some(predecl) = register_declaration(decl, globals_table)? {
            predeclarations.push(predecl);
        }
    }

    declarations.reserve(predeclarations.len());
    let rdecls: Vec<_> = predeclarations
        .into_par_iter()
        .map(|predecl| build_predeclaration(predecl, globals_table))
        .collect();
    for rdecl in rdecls {
        declarations.push(rdecl?);
    }

    Ok(ir::TranslationUnit { declarations: declarations })
}

#[derive(Debug, Clone, PartialEq)]
pub enum PreDeclaration {
    ExternFunction { name: String, ty: ir::FunctionType },
    Function {
        name: String,
        param_names: Vec<Spanned<String>>,
        ty: ir::FunctionType,
        stmt: Spanned<ast::CompoundStatement>,
        span: Span,
    },
}

fn register_declaration(decl: Spanned<ast::Declaration>,
                        globals_table: &mut GlobalTable)
                        -> Result<Option<PreDeclaration>, SemanticError> {
    match decl.inner {
        ast::Declaration::ExternFunction {
            name,
            params,
            variadic,
            return_ty,
        } => {
            let return_ty = build_type(return_ty, globals_table)?;

            let mut param_types = Vec::with_capacity(params.len());
            for ty in params {
                param_types.push(build_type(ty, globals_table)?);
            }

            let ty = ir::FunctionType {
                return_ty: Box::new(return_ty),
                params_ty: param_types,
                variadic: variadic,
            };

            if !globals_table.register_global(name.clone(), ir::Type::Function(ty.clone())) {
                return Err(SemanticError {
                               kind: SemanticErrorKind::FunctionAlreadyDefined { name: name },
                               span: decl.span,
                           });
            }

            Ok(Some(PreDeclaration::ExternFunction { name: name, ty: ty }))
        }
        ast::Declaration::Function {
            name,
            params,
            return_ty,
            stmt,
        } => {
            let return_ty = build_type(return_ty, globals_table)?;

            let mut param_names = Vec::with_capacity(params.len());
            let mut param_types = Vec::with_capacity(params.len());
            for (name, ty) in params {
                param_names.push(name);
                param_types.push(build_type(ty, globals_table)?);
            }

            let ty = ir::FunctionType {
                return_ty: Box::new(return_ty),
                params_ty: param_types,
                variadic: false,
            };

            if !globals_table.register_global(name.clone(), ir::Type::Function(ty.clone())) {
                return Err(SemanticError {
                               kind: SemanticErrorKind::FunctionAlreadyDefined { name: name },
                               span: decl.span,
                           });
            }

            Ok(Some(PreDeclaration::Function {
                        name: name,
                        param_names: param_names,
                        ty: ty,
                        stmt: stmt,
                        span: decl.span,
                    }))
        }
        ast::Declaration::Struct { name, fields } => {
            let mut field_names = Vec::with_capacity(fields.len());
            let mut final_fields = Vec::with_capacity(fields.len());
            for (name, ty) in fields {
                let ty = build_type(ty, globals_table)?;
                if field_names.contains(&name.inner) {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::FieldAlreadyDefined {
                                       name: name.inner,
                                   },
                                   span: name.span,
                               });
                }
                final_fields.push((name.inner.clone(), ty));
                field_names.push(name.inner);
            }

            let struct_ty = ir::Type::Struct(ir::StructType { fields_ty: final_fields });

            if globals_table.register_ty(name.clone(), struct_ty) {
                Ok(None)
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::TypeAlreadyDefined { name: name },
                        span: decl.span,
                    })
            }
        }
    }
}

fn build_predeclaration(predecl: PreDeclaration,
                        globals_table: &GlobalTable)
                        -> Result<ir::Declaration, SemanticError> {
    match predecl {
        PreDeclaration::ExternFunction { name, ty } => {
            Ok(ir::Declaration::ExternFunction { name: name, ty: ty })
        }
        PreDeclaration::Function {
            name,
            param_names,
            ty,
            stmt,
            span,
        } => {
            let mut function_builder = FunctionBuilder::new(name, ty.clone(), globals_table);
            function_builder.symbol_table.start_local_scope();
            for (index, (name, ty)) in param_names.into_iter().zip(ty.params_ty).enumerate() {
                if !function_builder.register_param(name.inner.clone(), ty, Some(index)) {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::ParameterAlreadyDefined {
                                       name: name.inner,
                                   },
                                   span: name.span,
                               });
                }
            }

            build_compound_statement(&mut function_builder, stmt)?;

            if *function_builder.ty.return_ty == ir::Type::Unit {
                let value = function_builder.new_temp_value(ir::Type::Unit);
                function_builder.push_statement(
                    ir::Statement::Assign(
                        value.clone(),
                        ir::Expression::Literal(ir::Literal::Unit)
                    )
                );
                let useless_label = function_builder.new_label();
                function_builder.push_terminator_label(Some(ir::Terminator::Ret(value)),
                                                       useless_label);
            }

            function_builder.symbol_table.end_local_scope();

            function_builder.into_function(span)
        }
    }
}

fn build_compound_statement(fb: &mut FunctionBuilder,
                            stmt: Spanned<ast::CompoundStatement>)
                            -> Result<(), SemanticError> {
    fb.symbol_table.start_local_scope();
    for s in stmt.inner.0 {
        build_statement(fb, s)?;
    }
    fb.symbol_table.end_local_scope();
    Ok(())
}

fn build_statement(fb: &mut FunctionBuilder,
                   stmt: Spanned<ast::Statement>)
                   -> Result<(), SemanticError> {
    match stmt.inner {
        ast::Statement::Compound(c) => build_compound_statement(fb, c),
        ast::Statement::Let { name, ty, expr } => {
            let expr_value = build_expression(fb, expr)?;
            let expr_value = build_lvalue_to_rvalue(fb, expr_value);

            let ty = if let Some(ty) = ty {
                build_type(ty, fb.symbol_table.globals)?
            } else {
                expr_value.ty.clone()
            };

            if ty == expr_value.ty {
                if !fb.register_local_variable(name.clone(), ty.clone()) {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::LocalVariableAlreadyDefined {
                                       name: name,
                                   },
                                   span: stmt.span,
                               });
                }

                let (_, lval_expr) = fb.symbol_table.get_var(&name).unwrap(); //TODO optimize
                let lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(ty)));
                fb.push_statement(ir::Statement::Assign(lvalue.clone(), lval_expr));
                fb.push_statement(ir::Statement::LValueSet(lvalue, expr_value));
                Ok(())
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::MismatchingTypesAssignment {
                            expected: ty,
                            found: expr_value.ty,
                        },
                        span: stmt.span,
                    })
            }
        }
        ast::Statement::Loop { stmt } => {
            let continue_label = fb.new_label();
            fb.push_terminator_label(None, continue_label);
            let break_label = fb.new_label();

            let old_loop_info = fb.current_loop_info;
            fb.current_loop_info = Some((continue_label, break_label));
            build_compound_statement(fb, stmt)?;
            fb.current_loop_info = old_loop_info;

            fb.push_terminator_label(Some(ir::Terminator::Br(continue_label)), break_label);
            Ok(())
        }
        ast::Statement::While { cond, stmt } => {
            let error_span = cond.span;
            let continue_label = fb.new_label();
            fb.push_terminator_label(None, continue_label);
            fb.symbol_table.start_local_scope();
            let cond_value = build_expression(fb, cond)?;
            let cond_value = build_lvalue_to_rvalue(fb, cond_value);

            if cond_value.ty != ir::Type::Bool {
                return Err(SemanticError {
                               kind: SemanticErrorKind::MismatchingTypesCondition {
                                   found: cond_value.ty,
                               },
                               span: error_span,
                           });
            }

            let stmt_label = fb.new_label();
            let break_label = fb.new_label();
            fb.push_terminator_label(Some(ir::Terminator::BrCond(cond_value,
                                                                 stmt_label,
                                                                 break_label)),
                                     stmt_label);

            let old_loop_info = fb.current_loop_info;
            fb.current_loop_info = Some((continue_label, break_label));
            build_compound_statement(fb, stmt)?;
            fb.current_loop_info = old_loop_info;

            fb.push_terminator_label(Some(ir::Terminator::Br(continue_label)), break_label);
            fb.symbol_table.end_local_scope();
            Ok(())
        }
        ast::Statement::For {
            name,
            init_expr,
            cond_expr,
            step_expr,
            stmt: mut sub_stmt,
        } => {
            //TODO: maybe we need a phase b/ ast and ir
            let mut stmts = Vec::new();
            let init_span = init_expr.span;
            let step_span = step_expr.span;

            stmts.push(Spanned::new(ast::Statement::Let {
                                        name: name,
                                        ty: None,
                                        expr: init_expr,
                                    },
                                    init_span));
            sub_stmt
                .inner
                .0
                .push(Spanned::new(ast::Statement::Expression { expr: step_expr }, step_span));
            stmts.push(Spanned::new(ast::Statement::While {
                                        cond: cond_expr,
                                        stmt: sub_stmt,
                                    },
                                    stmt.span));

            build_compound_statement(fb, Spanned::new(ast::CompoundStatement(stmts), stmt.span))
        }
        ast::Statement::If {
            if_branch,
            elseif_branches,
            else_branch,
        } => {
            let branches = vec![if_branch].into_iter().chain(elseif_branches);
            let global_end_label = fb.new_label();

            fb.symbol_table.start_local_scope();
            for branch in branches {
                let error_span = branch.0.span;
                let cond_value = build_expression(fb, branch.0)?;
                let cond_value = build_lvalue_to_rvalue(fb, cond_value);

                if cond_value.ty != ir::Type::Bool {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::MismatchingTypesCondition {
                                       found: cond_value.ty,
                                   },
                                   span: error_span,
                               });
                }

                let if_label = fb.new_label();
                let else_label = fb.new_label();

                fb.push_terminator_label(Some(ir::Terminator::BrCond(cond_value,
                                                                     if_label,
                                                                     else_label)),
                                         if_label);
                build_compound_statement(fb, branch.1)?;
                fb.push_terminator_label(Some(ir::Terminator::Br(global_end_label)), else_label);
            }
            if let Some(branch) = else_branch {
                build_compound_statement(fb, branch)?;
            }

            fb.push_terminator_label(None, global_end_label);
            fb.symbol_table.end_local_scope();
            Ok(())
        }
        ast::Statement::Break => {
            if let Some((_, id)) = fb.current_loop_info {
                fb.push_terminator(Some(ir::Terminator::Br(id)));
                Ok(())
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::BreakOutsideLoop,
                        span: stmt.span,
                    })
            }
        }
        ast::Statement::Continue => {
            if let Some((id, _)) = fb.current_loop_info {
                fb.push_terminator(Some(ir::Terminator::Br(id)));
                Ok(())
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::ContinueOutsideLoop,
                        span: stmt.span,
                    })
            }
        }
        ast::Statement::Return { expr } => {
            let (value, error_span) = if let Some(expr) = expr {
                let error_span = expr.span;
                let value = build_expression(fb, expr)?;
                let value = build_lvalue_to_rvalue(fb, value);
                (value, error_span)
            } else {
                let value = fb.new_temp_value(ir::Type::Unit);
                fb.push_statement(ir::Statement::Assign(
                    value.clone(),
                    ir::Expression::Literal(ir::Literal::Unit)
                ));
                (value, stmt.span)
            };

            if value.ty == *fb.ty.return_ty {
                fb.push_terminator(Some(ir::Terminator::Ret(value)));
                Ok(())
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::MismatchingTypesReturn {
                            expected: *fb.ty.return_ty.clone(),
                            found: value.ty,
                        },
                        span: error_span,
                    })
            }
        }
        ast::Statement::Expression { expr } => {
            build_expression(fb, expr)?;
            Ok(())
        }
    }

}

fn build_expression(fb: &mut FunctionBuilder,
                    expr: Spanned<ast::Expression>)
                    -> Result<ir::Value, SemanticError> {
    match expr.inner {
        ast::Expression::Assign(op, lhs, rhs) => {
            let lhs_span = lhs.span;
            let lhs_value = build_expression(fb, *lhs)?;
            let rhs_value = build_expression(fb, *rhs)?;
            let rhs_value = build_lvalue_to_rvalue(fb, rhs_value);

            if let ir::Type::LValue(sub) = lhs_value.ty.clone() {

                if let Some(binop) = op {
                    let lhs_real_value = build_lvalue_to_rvalue(fb, lhs_value.clone());

                    if let Some((irop, ty)) = typecheck_defs::binop_tyck(binop,
                                                                         &lhs_real_value.ty,
                                                                         &rhs_value.ty) {
                        let value = fb.new_temp_value(ty);
                        fb.push_statement(ir::Statement::Assign(
                            value.clone(),
                            ir::Expression::BinOp(irop, lhs_real_value, rhs_value)
                        ));
                        fb.push_statement(ir::Statement::LValueSet(lhs_value, value.clone()));
                        Ok(value)
                    } else {
                        Err(SemanticError {
                                kind: SemanticErrorKind::BinaryOperationUndefined {
                                    op: binop,
                                    lhs_ty: lhs_real_value.ty,
                                    rhs_ty: rhs_value.ty,
                                },
                                span: expr.span,
                            })
                    }
                } else if *sub == rhs_value.ty.clone() {
                    fb.push_statement(ir::Statement::LValueSet(lhs_value, rhs_value.clone()));
                    Ok(rhs_value)
                } else {
                    Err(SemanticError {
                            kind: SemanticErrorKind::MismatchingTypesAssignment {
                                expected: *sub,
                                found: rhs_value.ty,
                            },
                            span: expr.span,
                        })
                }
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::NonAssignableExpression,
                        span: lhs_span,
                    })
            }
        }
        ast::Expression::Subscript(array, index) => {
            let array_span = array.span;
            let index_span = index.span;

            let array_value = build_expression(fb, *array)?;
            let array_value = build_lvalue_to_rvalue(fb, array_value);
            let index_value = build_expression(fb, *index)?;
            let index_value = build_lvalue_to_rvalue(fb, index_value);

            if let ir::Type::Ptr(sub) = array_value.ty.clone() {
                if ir::Type::Int == index_value.ty {
                    let new_ptr_value = fb.new_temp_value(array_value.ty.clone());
                    fb.push_statement(ir::Statement::Assign(
                        new_ptr_value.clone(),
                        ir::Expression::BinOp(ir::BinOpCode::PtrAdd, array_value, index_value)
                    ));
                    let value = fb.new_temp_value(ir::Type::LValue(sub));
                    fb.push_statement(ir::Statement::Assign(
                        value.clone(),
                        ir::Expression::UnOp(ir::UnOpCode::PtrDeref, new_ptr_value)
                    ));
                    Ok(value)
                } else {
                    Err(SemanticError {
                            kind: SemanticErrorKind::IndexNotInt { found: index_value.ty },
                            span: index_span,
                        })
                }
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::NonSubscriptableType { found: array_value.ty },
                        span: array_span,
                    })
            }
        }
        ast::Expression::BinOp(code, lhs, rhs) => {
            if code == ast::BinOpCode::LogicalAnd {
                let logical_result = fb.register_local_logical();
                let lhs_value = build_expression(fb, *lhs)?;
                let lhs_value = build_lvalue_to_rvalue(fb, lhs_value);
                let lhs_ty = lhs_value.ty.clone();

                let true_label = fb.new_label();
                let false_label = fb.new_label();
                let final_label = fb.new_label();

                fb.push_terminator_label(Some(ir::Terminator::BrCond(lhs_value,
                                                                     true_label,
                                                                     false_label)),
                                         true_label);

                let rhs_value = build_expression(fb, *rhs)?;
                let rhs_value = build_lvalue_to_rvalue(fb, rhs_value);
                let rhs_ty = rhs_value.ty.clone();
                let final_value_rhs = fb.new_temp_value(ir::Type::LValue(Box::new(ir::Type::Bool)));

                fb.push_statement(ir::Statement::Assign(
                    final_value_rhs.clone(),
                    ir::Expression::LocalVarLoad(logical_result)
                ));
                fb.push_statement(ir::Statement::LValueSet(final_value_rhs, rhs_value));

                fb.push_terminator_label(Some(ir::Terminator::Br(final_label)), false_label);

                let false_value = fb.new_temp_value(ir::Type::Bool);
                fb.push_statement(ir::Statement::Assign(
                    false_value.clone(),
                    ir::Expression::Literal(ir::Literal::Bool(false))
                ));
                let final_value_false =
                    fb.new_temp_value(ir::Type::LValue(Box::new(ir::Type::Bool)));
                fb.push_statement(ir::Statement::Assign(
                    final_value_false.clone(),
                    ir::Expression::LocalVarLoad(logical_result)
                ));
                fb.push_statement(ir::Statement::LValueSet(final_value_false, false_value));

                fb.push_terminator_label(Some(ir::Terminator::Br(final_label)), final_label);

                let return_lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(ir::Type::Bool)));
                fb.push_statement(ir::Statement::Assign(
                    return_lvalue.clone(),
                    ir::Expression::LocalVarLoad(logical_result)
                ));
                let return_value = fb.new_temp_value(ir::Type::Bool);
                fb.push_statement(ir::Statement::Assign(return_value.clone(),
                                                        ir::Expression::LValueLoad(return_lvalue)));

                if lhs_ty != ir::Type::Bool || rhs_ty != ir::Type::Bool {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::BinaryOperationUndefined {
                                       op: code,
                                       lhs_ty: lhs_ty,
                                       rhs_ty: rhs_ty,
                                   },
                                   span: expr.span,
                               });
                }

                Ok(return_value)
            } else if code == ast::BinOpCode::LogicalOr {
                let logical_result = fb.register_local_logical();
                let lhs_value = build_expression(fb, *lhs)?;
                let lhs_value = build_lvalue_to_rvalue(fb, lhs_value);
                let lhs_ty = lhs_value.ty.clone();

                let true_label = fb.new_label();
                let false_label = fb.new_label();
                let final_label = fb.new_label();

                fb.push_terminator_label(Some(ir::Terminator::BrCond(lhs_value,
                                                                     true_label,
                                                                     false_label)),
                                         true_label);

                let true_value = fb.new_temp_value(ir::Type::Bool);
                fb.push_statement(ir::Statement::Assign(
                    true_value.clone(),
                    ir::Expression::Literal(ir::Literal::Bool(true))
                ));
                let final_value_true =
                    fb.new_temp_value(ir::Type::LValue(Box::new(ir::Type::Bool)));
                fb.push_statement(ir::Statement::Assign(
                    final_value_true.clone(),
                    ir::Expression::LocalVarLoad(logical_result)
                ));
                fb.push_statement(ir::Statement::LValueSet(final_value_true, true_value));

                fb.push_terminator_label(Some(ir::Terminator::Br(final_label)), false_label);

                let rhs_value = build_expression(fb, *rhs)?;
                let rhs_value = build_lvalue_to_rvalue(fb, rhs_value);
                let rhs_ty = rhs_value.ty.clone();
                let final_value_rhs = fb.new_temp_value(ir::Type::LValue(Box::new(ir::Type::Bool)));

                fb.push_statement(ir::Statement::Assign(
                    final_value_rhs.clone(),
                    ir::Expression::LocalVarLoad(logical_result)
                ));
                fb.push_statement(ir::Statement::LValueSet(final_value_rhs, rhs_value));

                fb.push_terminator_label(Some(ir::Terminator::Br(final_label)), final_label);

                let return_lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(ir::Type::Bool)));
                fb.push_statement(ir::Statement::Assign(
                    return_lvalue.clone(),
                    ir::Expression::LocalVarLoad(logical_result)
                ));
                let return_value = fb.new_temp_value(ir::Type::Bool);
                fb.push_statement(ir::Statement::Assign(return_value.clone(),
                                                        ir::Expression::LValueLoad(return_lvalue)));

                if lhs_ty != ir::Type::Bool || rhs_ty != ir::Type::Bool {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::BinaryOperationUndefined {
                                       op: code,
                                       lhs_ty: lhs_ty,
                                       rhs_ty: rhs_ty,
                                   },
                                   span: expr.span,
                               });
                }

                Ok(return_value)
            } else {
                let lhs_value = build_expression(fb, *lhs)?;
                let lhs_value = build_lvalue_to_rvalue(fb, lhs_value);
                let rhs_value = build_expression(fb, *rhs)?;
                let rhs_value = build_lvalue_to_rvalue(fb, rhs_value);

                if let Some((op, ty)) = typecheck_defs::binop_tyck(code,
                                                                   &lhs_value.ty,
                                                                   &rhs_value.ty) {
                    let value = fb.new_temp_value(ty);

                    fb.push_statement(ir::Statement::Assign(value.clone(),
                                                            ir::Expression::BinOp(op,
                                                                                  lhs_value,
                                                                                  rhs_value)));
                    Ok(value)
                } else {
                    Err(SemanticError {
                            kind: SemanticErrorKind::BinaryOperationUndefined {
                                op: code,
                                lhs_ty: lhs_value.ty,
                                rhs_ty: rhs_value.ty,
                            },
                            span: expr.span,
                        })
                }
            }
        }
        ast::Expression::UnOp(code, sub) => {
            let mut sub_value = build_expression(fb, *sub)?;
            if code != ast::UnOpCode::AddressOf {
                sub_value = build_lvalue_to_rvalue(fb, sub_value);
            }

            if let Some((op, ty)) = typecheck_defs::unop_tyck(code, &sub_value.ty) {
                let value = fb.new_temp_value(ty);
                fb.push_statement(ir::Statement::Assign(value.clone(),
                                                        ir::Expression::UnOp(op, sub_value)));
                Ok(value)
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::UnaryOperationUndefined {
                            op: code,
                            expr_ty: sub_value.ty,
                        },
                        span: expr.span,
                    })
            }
        }
        ast::Expression::FuncCall(func, args) => {
            let func_value = build_expression(fb, *func)?;
            let func_value = build_ptrdecay(fb, func_value);

            fn func_ptr(ty: ir::Type) -> Option<ir::FunctionType> {
                if let ir::Type::Ptr(ty) = ty {
                    if let ir::Type::Function(ty) = *ty {
                        Some(ty)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }

            if let Some(func_ty) = func_ptr(func_value.ty.clone()) {
                let mut args_span = Vec::with_capacity(args.len());
                let mut args_ty = Vec::with_capacity(args.len());
                let mut args_values = Vec::with_capacity(args.len());
                for arg in args {
                    args_span.push(arg.span);
                    let arg = build_expression(fb, arg)?;
                    let arg = build_lvalue_to_rvalue(fb, arg);

                    args_ty.push(arg.ty.clone());
                    args_values.push(arg);
                }


                if (func_ty.variadic && args_ty.len() < func_ty.params_ty.len()) ||
                   (!func_ty.variadic && args_ty.len() != func_ty.params_ty.len()) {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::MismatchingParamLen {
                                       expected: func_ty.params_ty.len(),
                                       found: args_ty.len(),
                                   },
                                   span: expr.span,
                               });
                }

                for i in 0..func_ty.params_ty.len() {
                    if args_ty[i] != func_ty.params_ty[i] {
                        return Err(SemanticError {
                                       kind: SemanticErrorKind::MismatchingTypesArgument {
                                           expected: func_ty.params_ty[i].clone(),
                                           found: args_ty[i].clone(),
                                       },
                                       span: args_span[i],
                                   });
                    }
                }

                let value = fb.new_temp_value(*func_ty.return_ty);
                fb.push_statement(ir::Statement::Assign(value.clone(),
                                                        ir::Expression::FuncCall(func_value,
                                                                                 args_values)));
                Ok(value)
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::NonCallableType { found: func_value.ty },
                        span: expr.span,
                    })
            }
        }
        ast::Expression::Cast(sub_expr, target_ty) => {
            let expr_value = build_expression(fb, *sub_expr)?;
            let expr_value = build_lvalue_to_rvalue(fb, expr_value);
            let target_ty = build_type(target_ty, fb.symbol_table.globals)?;

            if let Some(code) = typecheck_defs::cast_tyck(&expr_value.ty, &target_ty) {
                let value = fb.new_temp_value(target_ty);
                fb.push_statement(ir::Statement::Assign(value.clone(),
                                                        ir::Expression::CastOp(code, expr_value)));

                Ok(value)
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::CastUndefined {
                            expr_ty: expr_value.ty,
                            target_ty: target_ty,
                        },
                        span: expr.span,
                    })
            }
        }
        ast::Expression::FieldAccess(_, _) => unimplemented!(),
        ast::Expression::Paren(expr) => build_expression(fb, *expr),
        ast::Expression::Identifier(id) => {
            if let Some((ty, expr)) = fb.symbol_table.get_var(&id) {
                let value = fb.new_temp_value(ty);
                fb.push_statement(ir::Statement::Assign(value.clone(), expr));
                Ok(value)
            } else {
                Err(SemanticError {
                        kind: SemanticErrorKind::IdentifierUndefined { name: id },
                        span: expr.span,
                    })
            }
        }
        ast::Expression::Literal(lit) => {
            let lit = build_literal(lit, expr.span)?;
            let ty = match lit {
                ir::Literal::Int(_) => ir::Type::Int,
                ir::Literal::Double(_) => ir::Type::Double,
                ir::Literal::Bool(_) => ir::Type::Bool,
                ir::Literal::Char(_) => ir::Type::Char,
                ir::Literal::Unit => ir::Type::Unit,
            };
            let value = fb.new_temp_value(ty);
            fb.push_statement(ir::Statement::Assign(value.clone(), ir::Expression::Literal(lit)));
            Ok(value)
        }
        ast::Expression::StringLiteral(val) => {
            let mut string = Vec::with_capacity(val.len());
            let mut slash = false;
            for c in val.chars() {
                if slash {
                    let c = match c {
                        '\'' | '\"' => c as u8,
                        'a' => b'\x07',
                        'b' => b'\x08',
                        'f' => b'\x0c',
                        'n' => b'\n',
                        'r' => b'\r',
                        't' => b'\t',
                        'v' => b'\x0b',
                        '0' => b'\0',
                        _ => {
                            return Err(SemanticError {
                                           kind: SemanticErrorKind::InvalidEscapeChar { c: c },
                                           span: expr.span,
                                       })
                        }
                    };
                    slash = false;

                    string.push(c);
                } else if c == '\\' {
                    slash = true;
                } else {
                    string.push(c as u8);
                }
            }
            string.push(b'\0');

            let mut values = Vec::with_capacity(string.len());
            for c in string {
                let value = fb.new_temp_value(ir::Type::Char);
                fb.push_statement(ir::Statement::Assign(
                    value.clone(),
                    ir::Expression::Literal(ir::Literal::Char(c))
                ));
                values.push(value);
            }

            Ok(build_array_with_values(fb, ir::Type::Char, values))
        }
        ast::Expression::ArrayFullLiteral(exprs) => {
            let mut values = Vec::with_capacity(exprs.len());
            let mut spans = Vec::with_capacity(exprs.len());
            for expr in exprs {
                spans.push(expr.span);
                let expr_value = build_expression(fb, expr)?;
                let expr_value = build_lvalue_to_rvalue(fb, expr_value);
                values.push(expr_value);
            }

            if values.is_empty() {
                return Err(SemanticError {
                               kind: SemanticErrorKind::EmptyArrayLiteral,
                               span: expr.span,
                           });
            }

            let expr_ty = values.first().unwrap().ty.clone();

            for i in 1..values.len() {
                if values[i].ty != values[0].ty {
                    return Err(SemanticError {
                                   kind: SemanticErrorKind::MismatchingTypesArrayLiteral {
                                       expected: values[0].ty.clone(),
                                       found: values[i].ty.clone(),
                                   },
                                   span: spans[i],
                               });
                }
            }

            Ok(build_array_with_values(fb, expr_ty, values))
        }
        ast::Expression::ArrayDefaultLiteral(expr, size) => {
            let expr_value = build_expression(fb, *expr)?;
            let expr_value = build_lvalue_to_rvalue(fb, expr_value);

            let ptr_ty = ir::Type::Ptr(Box::new(expr_value.ty.clone()));

            let array_id = fb.register_local_array(expr_value.ty.clone(), size as usize);
            let array_lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(expr_value.ty.clone())));
            fb.push_statement(ir::Statement::Assign(array_lvalue.clone(),
                                                    ir::Expression::LocalVarLoad(array_id)));
            let array_value = fb.new_temp_value(ptr_ty.clone());
            fb.push_statement(ir::Statement::Assign(array_value.clone(),
                                                    ir::Expression::UnOp(ir::UnOpCode::AddressOf,
                                                                         array_lvalue)));

            for i in 0..size {
                let index_value = fb.new_temp_value(ir::Type::Int);
                fb.push_statement(ir::Statement::Assign(
                    index_value.clone(),
                    ir::Expression::Literal(ir::Literal::Int(i))
                ));

                let ptr_value = fb.new_temp_value(ptr_ty.clone());
                fb.push_statement(ir::Statement::Assign(
                    ptr_value.clone(),
                    ir::Expression::BinOp(ir::BinOpCode::PtrAdd, array_value.clone(), index_value)
                ));

                let lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(expr_value.ty.clone())));
                fb.push_statement(ir::Statement::Assign(
                    lvalue.clone(),
                    ir::Expression::UnOp(ir::UnOpCode::PtrDeref, ptr_value)
                ));

                fb.push_statement(ir::Statement::LValueSet(lvalue, expr_value.clone()));
            }

            Ok(array_value)
        }
    }
}

fn build_array_with_values(fb: &mut FunctionBuilder,
                           expr_ty: ir::Type,
                           values: Vec<ir::Value>)
                           -> ir::Value {
    let ptr_ty = ir::Type::Ptr(Box::new(expr_ty.clone()));

    let array_id = fb.register_local_array(expr_ty.clone(), values.len());
    let array_lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(expr_ty.clone())));
    fb.push_statement(ir::Statement::Assign(array_lvalue.clone(),
                                            ir::Expression::LocalVarLoad(array_id)));
    let array_value = fb.new_temp_value(ptr_ty.clone());
    fb.push_statement(ir::Statement::Assign(array_value.clone(),
                                            ir::Expression::UnOp(ir::UnOpCode::AddressOf,
                                                                 array_lvalue)));

    for (i, value) in values.into_iter().enumerate() {
        let index_value = fb.new_temp_value(ir::Type::Int);
        fb.push_statement(ir::Statement::Assign(index_value.clone(),
                                                ir::Expression::Literal(ir::Literal::Int(i as
                                                                                         i64))));

        let ptr_value = fb.new_temp_value(ptr_ty.clone());
        fb.push_statement(ir::Statement::Assign(ptr_value.clone(),
                                                ir::Expression::BinOp(ir::BinOpCode::PtrAdd,
                                                                      array_value.clone(),
                                                                      index_value)));

        let lvalue = fb.new_temp_value(ir::Type::LValue(Box::new(expr_ty.clone())));
        fb.push_statement(ir::Statement::Assign(lvalue.clone(),
                                                ir::Expression::UnOp(ir::UnOpCode::PtrDeref,
                                                                     ptr_value)));

        fb.push_statement(ir::Statement::LValueSet(lvalue, value.clone()));
    }

    array_value
}

fn build_literal(lit: ast::Literal, span: Span) -> Result<ir::Literal, SemanticError> {
    match lit {
        ast::Literal::Unit => Ok(ir::Literal::Unit),
        ast::Literal::Int(val) => Ok(ir::Literal::Int(val)),
        ast::Literal::Double(val) => Ok(ir::Literal::Double(val)),
        ast::Literal::Bool(val) => Ok(ir::Literal::Bool(val)),
        ast::Literal::Char(val) => {
            let mut output = String::with_capacity(val.len());
            let mut slash = false;
            for c in val.chars() {
                if slash {
                    let c_value = match c {
                        '\'' | '\"' => c,
                        'a' => '\x07',
                        'b' => '\x08',
                        'f' => '\x0c',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        'v' => '\x0b',
                        '0' => '\0',
                        _ => {
                            return Err(SemanticError {
                                           kind: SemanticErrorKind::InvalidEscapeChar { c: c },
                                           span: span,
                                       })
                        }
                    };
                    output.push(c_value);
                    slash = false;
                } else if c == '\\' {
                    slash = true;
                } else {
                    output.push(c);
                }
            }
            if output.len() > 1 {
                Err(SemanticError {
                        kind: SemanticErrorKind::MultipleCharLiteral,
                        span: span,
                    })
            } else if output.is_empty() {
                Err(SemanticError {
                        kind: SemanticErrorKind::EmptyCharLiteral,
                        span: span,
                    })
            } else {
                Ok(ir::Literal::Char(output.chars().next().unwrap() as u8))
            }
        }
    }
}

fn build_lvalue_to_rvalue(fb: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    if let ir::Type::LValue(sub) = value.ty.clone() {
        let new_value = fb.new_temp_value(*sub);
        fb.push_statement(ir::Statement::Assign(new_value.clone(),
                                                ir::Expression::LValueLoad(value)));
        new_value
    } else {
        value
    }
}

fn build_ptrdecay(fb: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    if let Some(decay_ty) = value.ty.decay_type() {
        let ptr_ty = ir::Type::Ptr(Box::new(value.ty.clone()));
        let local_id = fb.register_local_unnamed(value.ty);
        let decay_lvalue = fb.new_temp_value(decay_ty);
        fb.push_statement(ir::Statement::Assign(decay_lvalue.clone(),
                                                ir::Expression::LocalVarLoad(local_id)));
        let ptr_value = fb.new_temp_value(ptr_ty.clone());
        fb.push_statement(ir::Statement::Assign(ptr_value.clone(),
                                                ir::Expression::UnOp(ir::UnOpCode::AddressOf,
                                                                     decay_lvalue)));
        ptr_value
    } else {
        value
    }
}

fn build_type(parse_ty: Spanned<ast::ParseType>,
              globals_table: &GlobalTable)
              -> Result<ir::Type, SemanticError> {
    match parse_ty.inner {
        ast::ParseType::Unit => Ok(ir::Type::Unit),
        ast::ParseType::Ptr(sub) => Ok(ir::Type::Ptr(Box::new(build_type(*sub, globals_table)?))),
        ast::ParseType::Lit(lit) => {
            let span = parse_ty.span;
            globals_table
                .get_type(&lit)
                .ok_or_else(|| {
                                SemanticError {
                                    kind: SemanticErrorKind::UndefinedType { name: lit },
                                    span: span,
                                }
                            })
        }
    }
}
