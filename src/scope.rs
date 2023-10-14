use std::collections::HashMap;

use crate::ast::Type;

#[derive(Clone)]
pub struct VariableSymbol {
    identifier: String,
    type_: Type,
}

impl VariableSymbol {
    fn new(identifier: String, type_: Type) -> Self {
        Self { identifier, type_ }
    }
}

pub struct Scope<'a> {
    vars: HashMap<String, VariableSymbol>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            vars: HashMap::new(),
            parent,
        }
    }

    fn lookup_variable(&self, identifier: &str) -> Option<VariableSymbol> {
        self.vars.get(identifier).cloned().or_else(|| {
            self.parent
                .as_ref()
                .map(|p| p.lookup_variable(identifier))
                .flatten()
        })
    }

    fn declare_variable(&mut self, identifier: String, type_: Type) -> Option<VariableSymbol> {
        if self.vars.contains_key(&identifier) {
            None
        } else {
            let symbol = VariableSymbol::new(identifier.clone(), type_);
            self.vars.insert(identifier, symbol.clone());
            Some(symbol)
        }
    }
}
