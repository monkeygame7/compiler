use crate::{
    ast::lexer::SyntaxToken,
    compilation::Type,
    id::{Idx, IdxVec},
    idx,
};

idx!(FunctionId);
idx!(VariableId);

pub struct Function {}

pub struct VariableSymbol {
    pub typ: Type,
    pub identifier: String,
}

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

pub struct LocalScope {
    variables: Vec<VariableId>,
    function: Option<FunctionId>,
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            function: None,
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

    pub fn lookup_variable(&self, identifier: &str) -> Option<VariableId> {
        todo!();
    }

    pub fn lookup_type(&self, id: VariableId) -> Type {
        self.global_scope.variables[id].typ
    }

    pub fn declare_variable(&self, identifier: &SyntaxToken, typ: Type) -> VariableId {
        todo!()
    }
}
