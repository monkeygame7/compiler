use crate::{
    ast::{Idx, IdxVec},
    compilation::Type,
    idx,
    parsing::SyntaxToken,
};

idx!(FunctionId);
idx!(VariableId);

#[derive(Debug)]
pub struct Function {}

#[derive(Debug)]
pub struct VariableSymbol {
    pub id: VariableId,
    pub typ: Type,
    pub identifier: String,
}

#[derive(Debug)]
pub struct GlobalScope {
    pub functions: IdxVec<FunctionId, Function>,
    pub variables: IdxVec<VariableId, VariableSymbol>,
    pub globals: Vec<VariableId>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self {
            functions: IdxVec::new(),
            variables: IdxVec::new(),
            globals: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct LocalScope {
    variables: Vec<VariableId>,
    _function: Option<FunctionId>,
}

impl LocalScope {
    pub fn new(function: Option<FunctionId>) -> Self {
        Self {
            variables: Vec::new(),
            _function: function,
        }
    }
}

pub struct Scopes {
    local_scopes: Vec<LocalScope>,
    pub global_scope: GlobalScope,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            local_scopes: Vec::new(),
            global_scope: GlobalScope::new(),
        }
    }

    pub fn lookup_variable(&self, identifier: &str) -> Option<&VariableSymbol> {
        let local_vars = self
            .local_scopes
            .iter()
            .rev()
            .flat_map(|local| &local.variables);
        let global_vars = self.global_scope.globals.iter();
        local_vars
            .chain(global_vars)
            .map(|id| &self.global_scope.variables[*id])
            .filter(|var| var.identifier == identifier)
            .nth(0)
    }

    pub fn declare_variable(&mut self, identifier: &SyntaxToken, typ: Type) -> Option<VariableId> {
        let exists = self
            .local_scopes
            .last()
            .iter()
            .flat_map(|scope| scope.variables.iter())
            .map(|id| &self.global_scope.variables[*id])
            .any(|var| var.identifier == identifier.literal);

        if exists {
            None
        } else {
            Some(self._new_variable(&identifier.literal, typ))
        }
    }

    fn _new_variable(&mut self, identifier: &str, typ: Type) -> VariableId {
        let symbol = VariableSymbol {
            identifier: identifier.to_string(),
            id: VariableId::default(),
            typ,
        };

        let id = self.global_scope.variables.push(symbol);
        self.global_scope.variables[id].id = id;

        let destination = match self.local_scopes.last_mut() {
            Some(scope) => &mut scope.variables,
            None => &mut self.global_scope.globals,
        };
        destination.push(id);

        id
    }

    pub fn enter_scope(&mut self) {
        self.local_scopes.push(LocalScope::new(None));
    }

    pub fn exit_scope(&mut self) {
        self.local_scopes
            .pop()
            .expect("Tried to exit scope but no scope found");
    }
}
