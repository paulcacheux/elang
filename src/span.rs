use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(pub usize, pub usize, pub usize);

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Spanned {
            inner: inner,
            span: span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
