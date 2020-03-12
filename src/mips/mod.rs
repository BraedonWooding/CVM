// Represents a mips assembly parser
// It converts the mips to CVMs bytecode instruction set
// This isn't a SPIM like parser...
// It is purely just MIPS

pub use crate::{Span, Spanned};

pub type Ident = Spanned<String>;
pub type IdentList = Vec<Ident>;

pub mod modules;

pub struct MipsModule {
    pub supported_instructions: Vec<&'static str>,
    pub supported_directives: Vec<&'static str>,
    pub allow_symbolic_equate: bool,
}
