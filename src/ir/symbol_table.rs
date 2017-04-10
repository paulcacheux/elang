use std::collections::HashMap;

use ir;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    globals: HashMap<String, ir::Type>,
    locals: Vec<HashMap<String, (ir::LocalVarId, ir::Type)>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            globals: HashMap::new(),
            locals: Vec::new(),
        }
    }

    pub fn start_local_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    pub fn end_local_scope(&mut self) {
        self.locals.pop();
    }

    pub fn register_local(&mut self, name: String, ty: ir::Type, id: ir::LocalVarId) -> bool {
        // return false if already on scope
        self.locals
            .last_mut()
            .unwrap()
            .insert(name, (id, ty))
            .is_none()
    }

    pub fn register_global(&mut self, name: String, ty: ir::Type) -> bool {
        self.globals.insert(name, ty).is_none()
    }

    pub fn get_var(&self, name: &String) -> Option<(ir::Type, ir::Expression)> {
        for scope in self.locals.iter().rev() {
            if let Some(&(ref id, ref ty)) = scope.get(name) {
                return Some((ir::Type::LValue(Box::new(ty.clone())),
                             ir::Expression::LocalVarLoad(id.clone())));
            }
        }
        if let Some(ty) = self.globals.get(name) {
            Some((ty.clone(), ir::Expression::GlobalLoad(name.clone())))
        } else {
            None
        }
    }
}
