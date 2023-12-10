pub mod ast;
pub mod diagnostics;
pub mod lowering;
mod scope;
mod types;
pub use ast::Ast;
use inkwell::context::Context;
use inkwell::module::Module;
pub use scope::Function;
pub use scope::FunctionId;
pub use scope::GlobalScope;
pub use scope::VariableId;
pub use types::Signature;
pub use types::Type;
pub use types::Types;

use std::rc::Rc;

use {
    ast::parsing::{Lexer, Parser},
    diagnostics::{DiagnosticBag, SourceText},
};

use types::Resolver;

use self::lowering::IRBuilder;

pub struct CompilationUnit {
    pub src: SourceText,
    pub ast: Ast,
    pub diagnostics: Rc<DiagnosticBag>,
    pub scope: GlobalScope,
}

pub struct Program<'ctx> {
    pub ir: Module<'ctx>,
}

impl<'ctx> Program<'ctx> {
    pub fn build(context: &'ctx Context, compilation: &CompilationUnit) -> Self {
        let module = IRBuilder::build(compilation, &context);
        Program { ir: module }
    }
}

pub fn compile(
    text: &str,
    print_tree: bool,
) -> Result<CompilationUnit, (SourceText, Rc<DiagnosticBag>)> {
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
            scope: resolver.scopes.global_scope,
        })
    }
}
