mod scope;
mod types;
pub use scope::FunctionId;
pub use scope::VariableId;
pub use types::Signature;
pub use types::Type;
pub use types::Types;

use std::rc::Rc;

use crate::{
    ast::Ast,
    diagnostics::{DiagnosticBag, SourceText},
    parsing::{Lexer, Parser},
};

use self::{scope::GlobalScope, types::Resolver};

pub struct CompilationUnit {
    pub src: SourceText,
    pub ast: Ast,
    pub diagnostics: Rc<DiagnosticBag>,
    _scope: GlobalScope,
}

impl CompilationUnit {
    pub fn compile(text: &str, print_tree: bool) -> Result<Self, (SourceText, Rc<DiagnosticBag>)> {
        let src = SourceText::from(text).unwrap();
        let diagnostics = Rc::new(DiagnosticBag::new());

        let lexer = Lexer::new(&src);
        let mut ast = Parser::parse(lexer, diagnostics.clone());

        let mut resolver = Resolver::new(diagnostics.clone());
        ast.visit_mut(&mut resolver);

        if print_tree {
            ast.print();
        }

        if diagnostics.has_errors() {
            Err((src, diagnostics))
        } else {
            Ok(CompilationUnit {
                src,
                ast,
                diagnostics,
                _scope: resolver.scopes.global_scope,
            })
        }
    }
}
