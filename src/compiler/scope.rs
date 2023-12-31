use super::{
    ast::{parsing::SyntaxToken, ExprId, Idx, IdxVec},
    Signature, Type,
};
use crate::idx;

idx!(FunctionId);
idx!(VariableId);

#[derive(Debug)]
pub struct Function {
    pub id: FunctionId,
    pub name: String,
    pub params: Vec<VariableId>,
    pub body: ExprId,
    pub sig: Signature,
    pub var: VariableId,
}

#[derive(Debug)]
pub struct VariableSymbol {
    pub id: VariableId,
    pub is_mutable: bool,
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
    function: Option<FunctionId>,
}

impl LocalScope {
    pub fn new(function: Option<FunctionId>) -> Self {
        Self {
            variables: Vec::new(),
            function,
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

    pub fn lookup_function(&self, identifier: &str) -> Option<&Function> {
        self.global_scope
            .functions
            .iter()
            .filter(|func| func.name == identifier)
            .nth(0)
    }

    pub fn declare_function(
        &mut self,
        identifier: &SyntaxToken,
        return_type: Type,
        params: Vec<VariableId>,
        body: ExprId,
    ) -> FunctionId {
        let name = &identifier.literal;
        let params_sig = params
            .iter()
            .map(|id| &self.global_scope.variables[*id])
            .map(|v| v.typ.clone())
            .collect::<Vec<_>>();
        let sig = Signature {
            return_type,
            params: params_sig.into(),
        };
        // Functions are immutable
        let var = self._new_variable(identifier, Type::func(&sig), false);
        let func = Function {
            id: FunctionId::default(),
            name: name.to_owned(),
            params,
            body,
            sig,
            var,
        };

        let id = self.global_scope.functions.push(func);
        self.global_scope.functions[id].id = id;
        id
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

    pub fn declare_variable(
        &mut self,
        identifier: &SyntaxToken,
        typ: Type,
        is_mutable: bool,
    ) -> Option<VariableId> {
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
            Some(self._new_variable(&identifier, typ, is_mutable))
        }
    }

    pub fn create_unscoped_variable(
        &mut self,
        identifier: &SyntaxToken,
        typ: Type,
        is_mutable: bool,
    ) -> VariableId {
        let symbol = VariableSymbol {
            identifier: identifier.to_string(),
            is_mutable,
            id: VariableId::default(),
            typ,
        };
        let id = self.global_scope.variables.push(symbol);
        self.global_scope.variables[id].id = id;
        id
    }

    fn _new_variable(
        &mut self,
        identifier: &SyntaxToken,
        typ: Type,
        is_mutable: bool,
    ) -> VariableId {
        let id = self.create_unscoped_variable(identifier, typ, is_mutable);

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

    pub fn enter_function_scope(&mut self, func: FunctionId) {
        let mut scope = LocalScope::new(Some(func));
        let func = &self.global_scope.functions[func];
        for param in &func.params {
            scope.variables.push(*param);
        }
        self.local_scopes.push(scope);
    }

    pub fn get_current_function(&self) -> Option<FunctionId> {
        self.local_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.function)
            .nth(0)
    }

    pub fn exit_scope(&mut self) {
        self.local_scopes
            .pop()
            .expect("Tried to exit scope but no scope found");
    }

    pub fn exit_function_scope(&mut self) {
        self.local_scopes
            .pop()
            .expect("Tried to exit scope but no scope found")
            .function
            .expect("Tried to exit function scope, but not in a function");
    }
}
