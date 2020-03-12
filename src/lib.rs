#[macro_use]
extern crate enum_as_inner;

#[macro_use]
extern crate bitflags;

extern crate libc;
extern crate num;

#[macro_use]
extern crate impls;

pub mod c_transpiler;
pub mod compiler;
pub mod mips;
pub mod interpreter;

mod logger;
pub use c_transpiler::*;
pub use compiler::ast::*;
pub use compiler::lexer::*;
pub use compiler::parser::*;
pub use compiler::type_check::*;
pub use compiler::type_infer::*;

#[derive(Clone, Default, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Spanned<T> {
        Spanned::<T> {
            span: span,
            inner: inner,
        }
    }

    pub fn transform<U, F: FnOnce(T) -> U>(self, capture: F) -> Spanned<U> {
        Spanned::new(capture(self.inner), self.span)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} at {:?}", self.inner, self.span)
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Clone, Default)]
pub struct Span {
    /// Represents the line point of this span
    pub line: usize,
    /// Represents the col point of this
    pub col: usize,
    /// Represents the byte offset range of this span
    pub byte_offset: (usize, usize),
}

impl Span {
    pub fn join(a: &Span, b: &Span) -> Span {
        use std::cmp;
        Span {
            line: cmp::min(a.line, b.line),
            col: cmp::min(a.col, b.col),
            byte_offset: (
                cmp::min(a.byte_offset.0, b.byte_offset.0),
                cmp::max(a.byte_offset.1, b.byte_offset.1),
            ),
        }
    }
}

impl std::cmp::PartialEq for Span {
    /*
     * Note: Span's are always equatable to each other irregardless of value
     *       this is for the benefit of Spanned.
     */
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl std::cmp::Eq for Span {}

impl std::hash::Hash for Span {
    /*
     * Note: Span's shouldn't be used in a hash algorithm since they always
     *       result in an identical hash value.
     *       This is to enable 2 spanned's to be equal
     */
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        state.finish();
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Line {}:{}", self.line, self.col)
    }
}
