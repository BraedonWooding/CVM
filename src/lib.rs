#[macro_use]
extern crate enum_as_inner;

#[macro_use]
extern crate bitflags;

pub mod c_transpiler;
pub mod compiler;
mod logger;
pub use c_transpiler::*;
pub use compiler::ast::*;
pub use compiler::lexer::*;
pub use compiler::parser::*;
pub use compiler::type_check::*;
pub use compiler::type_infer::*;
