use std::collections::HashMap;

use ir;

#[derive(Debug, Clone)]
pub struct GlobalTable {
    globals: HashMap<String, ir::Type>,
}

impl GlobalTable {
    pub fn new() -> Self {
        GlobalTable {
            globals: HashMap::new(),
        }
    }

    pub fn register_global(&mut self, name: String, ty: ir::Type) -> bool {
        self.globals.insert(name, ty).is_none()
    }

    pub fn get_var(&self, name: &String) -> Option<(ir::Type, ir::Expression)> {
        if let Some(ty) = self.globals.get(name) {
            Some((ty.clone(), ir::Expression::GlobalLoad(name.clone())))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable<'a> {
    globals: &'a GlobalTable,
    locals: Vec<HashMap<String, (ir::LocalVarId, ir::Type)>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new(globals: &'a GlobalTable) -> Self {
        SymbolTable {
            globals: globals,
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

    pub fn get_var(&self, name: &String) -> Option<(ir::Type, ir::Expression)> {
        for scope in self.locals.iter().rev() {
            if let Some(&(ref id, ref ty)) = scope.get(name) {
                return Some((ir::Type::LValue(Box::new(ty.clone())),
                             ir::Expression::LocalVarLoad(*id)));
            }
        }
        self.globals.get_var(name)
    }
}
