use crate::{
    id::{Idx, IdxVec},
    idx,
};

idx!(FunctionId);
idx!(VariableId);

pub struct Function {}

pub struct VariableSymbol {}

pub struct GlobalScope {
    functions: IdxVec<FunctionId, Function>,
    variables: IdxVec<VariableId, VariableSymbol>,
    globals: Vec<VariableId>,
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
    global_scope: GlobalScope,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            local_scopes: Vec::new(),
            global_scope: GlobalScope::new(),
        }
    }
}
