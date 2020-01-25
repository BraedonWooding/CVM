pub mod lexer;
pub mod ast;
pub mod parser;
pub mod scope;
pub mod type_infer;
pub mod type_check;

pub type Ident = Spanned<String>;
pub type IdentList = Vec<Ident>;

#[derive(Clone, Default, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Spanned<T> {
        Spanned::<T> {
            span: span,
            inner: inner
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
    pub line: (usize, usize),
    pub col: (usize, usize)
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
    fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
        state.finish();
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Line {}:{} to Line {}:{}", self.line.0, self.col.0, self.line.1, self.col.1)
    }
}
