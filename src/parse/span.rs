use std::ops::Range;

pub struct Span {
    rng: Range<usize>,
}

impl Span {
    pub fn new<R: Into<Range<usize>>>(r: R) -> Self {
        Self { rng: r.into() }
    }

    pub fn text<'a>(&self, input: &'a str) -> &'a str {
        &input[self.rng.start..self.rng.end]
    }
}
