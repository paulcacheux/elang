use std::collections::{HashMap, HashSet};

use ir;
use ir::{GlobalTable, SymbolTable};
use semantic_error::{SemanticError, SemanticErrorKind};
use span::Span;

#[derive(Debug, Clone)]
enum Item {
    Statement(ir::Statement),
    TerminatorAndLabel(Option<ir::Terminator>, ir::BasicBlockId), // None if fallthrough
}

#[derive(Debug)]
pub struct FunctionBuilder<'a> {
    pub name: String,
    pub ty: ir::FunctionType,
    pub symbol_table: SymbolTable<'a>,
    pub current_loop_info: Option<(ir::BasicBlockId, ir::BasicBlockId)>, // (continue, break)
    locals: Vec<ir::LocalVar>,
    items: Vec<Item>,
    local_counter: usize,
    label_counter: usize,
    current_temp_id: usize,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(name: String, ty: ir::FunctionType, globals_table: &'a GlobalTable) -> Self {
        FunctionBuilder {
            name: name,
            ty: ty,
            symbol_table: SymbolTable::new(globals_table),
            current_loop_info: None,
            locals: Vec::new(),
            items: Vec::new(),
            local_counter: 0,
            label_counter: 1,
            current_temp_id: 0,
        }
    }

    pub fn into_function(self, span: Span) -> Result<ir::Declaration, SemanticError> {
        #[derive(Clone)]
        enum PanicTerminator {
            Real(ir::Terminator),
            Panic,
        }

        struct TempBasicBlock {
            id: ir::BasicBlockId,
            stmts: Vec<ir::Statement>,
            terminator: PanicTerminator,
        }

        let mut basic_blocks = Vec::new();
        let mut current_id = ir::BasicBlockId(0);
        let mut current_stmts = Vec::new();

        for item in self.items {
            match item {
                Item::Statement(s) => current_stmts.push(s),
                Item::TerminatorAndLabel(terminator, label) => {
                    let terminator = if let Some(real_ter) = terminator.clone() {
                        real_ter
                    } else {
                        ir::Terminator::Br(label)
                    };
                    basic_blocks.push(TempBasicBlock {
                                          id: current_id,
                                          stmts: current_stmts,
                                          terminator: PanicTerminator::Real(terminator),
                                      });
                    current_id = label;
                    current_stmts = Vec::new();
                }
            }
        }

        basic_blocks.push(TempBasicBlock {
                              id: current_id,
                              stmts: current_stmts,
                              terminator: PanicTerminator::Panic,
                          });

        // remove check panic
        let mut preds: HashMap<ir::BasicBlockId, HashSet<ir::BasicBlockId>> = HashMap::new();
        for bb in &basic_blocks {
            if let PanicTerminator::Real(ref terminator) = bb.terminator {
                match *terminator {
                    ir::Terminator::Br(id) => {
                        preds.entry(id).or_insert_with(HashSet::new).insert(bb.id);
                    }
                    ir::Terminator::Ret(_) => {}
                    ir::Terminator::BrCond(_, id1, id2) => {
                        preds.entry(id1).or_insert_with(HashSet::new).insert(bb.id);
                        preds.entry(id2).or_insert_with(HashSet::new).insert(bb.id);
                    }
                }
            }
        }

        let mut opened = Vec::new();
        opened.push(basic_blocks[basic_blocks.len() - 1].id);
        let mut panic_preds = HashSet::<ir::BasicBlockId>::new();

        while !opened.is_empty() {
            let id = opened.pop().unwrap();
            for pred in preds.get(&id).unwrap_or(&HashSet::new()) {
                if !panic_preds.contains(pred) {
                    opened.push(*pred);
                }
            }
            panic_preds.insert(id);
        }

        if panic_preds.contains(&ir::BasicBlockId(0)) {
            return Err(SemanticError {
                kind: SemanticErrorKind::NotAllPathsReturnAValue,
                span: span
            });
        } else {
            basic_blocks.pop();
        }

        basic_blocks.retain(|bb| {
                                preds.get(&bb.id).map(|s| s.len()).unwrap_or(0) != 0 || bb.id.0 == 0
                            });

        let real_bbs = basic_blocks
            .into_iter()
            .map(|bb| if let PanicTerminator::Real(term) = bb.terminator {
                     ir::BasicBlock {
                         id: bb.id,
                         stmts: bb.stmts,
                         terminator: term,
                     }
                 } else {
                     unreachable!()
                 })
            .collect();

        Ok(ir::Declaration::Function {
               name: self.name,
               ty: self.ty,
               locals: self.locals,
               bbs: real_bbs,
           })
    }

    pub fn new_temp_value(&mut self, ty: ir::Type) -> ir::Value {
        let id = self.current_temp_id;
        self.current_temp_id += 1;
        ir::Value { id: id, ty: ty }
    }

    pub fn new_label(&mut self) -> ir::BasicBlockId {
        let id = ir::BasicBlockId(self.label_counter);
        self.label_counter += 1;
        id
    }

    pub fn push_terminator_label(&mut self, terminator: Option<ir::Terminator>, id: ir::BasicBlockId) {
        self.items.push(Item::TerminatorAndLabel(terminator, id));
    }

    pub fn push_terminator(&mut self, terminator: Option<ir::Terminator>) {
        let label = self.new_label();
        self.push_terminator_label(terminator, label);
    }

    pub fn push_statement(&mut self, stmt: ir::Statement) {
        self.items.push(Item::Statement(stmt));
    }

    pub fn register_param(&mut self, name: String, ty: ir::Type, param_index: Option<usize>) -> bool {
        let res = self.symbol_table
            .register_local(name, ty.clone(), ir::LocalVarId(self.local_counter));
        self.locals
            .push(ir::LocalVar {
                      id: ir::LocalVarId(self.local_counter),
                      ty: ty,
                      size: 1,
                      param_index: param_index,
                  });
        self.local_counter += 1;
        res
    }

    pub fn register_local_variable(&mut self, name: String, ty: ir::Type) -> bool {
        self.register_param(name, ty, None)
    }

    pub fn register_local_logical(&mut self) -> ir::LocalVarId {
        self.register_local_unnamed(ir::Type::Bool)
    }

    pub fn register_local_array(&mut self, ty: ir::Type, size: usize) -> ir::LocalVarId {
        let id = ir::LocalVarId(self.local_counter);
        self.locals
            .push(ir::LocalVar {
                      id: id,
                      ty: ty,
                      size: size,
                      param_index: None,
                  });
        self.local_counter += 1;
        id
    }

    pub fn register_local_unnamed(&mut self, ty: ir::Type) -> ir::LocalVarId {
        let id = ir::LocalVarId(self.local_counter);
        self.locals
            .push(ir::LocalVar {
                      id: id,
                      ty: ty,
                      size: 1,
                      param_index: None,
                  });
        self.local_counter += 1;
        id
    }
}
