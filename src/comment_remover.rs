use std::str::CharIndices;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub struct CommentRemover<'input> {
    chars: Peekable<CharIndices<'input>>,
}

impl<'input> CommentRemover<'input> {
    pub fn new(input: &'input str) -> Self {
        CommentRemover {
            chars: input.char_indices().peekable()
        }
    }

    fn eat_until_newline(&mut self) {
        loop {
            if let Some(&(_, '\n')) = self.chars.peek() {
                return
            }
            self.chars.next();
        }
    }
}

impl<'input> Iterator for CommentRemover<'input> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&(i, '/')) = self.chars.peek() {
            self.chars.next();
            if let Some(&(_, '/')) = self.chars.peek() {
                self.eat_until_newline();
                self.chars.next() // the \n
            } else {
                Some((i, '/'))
            }
        } else {
            self.chars.next()
        }
    }
}
