use std::{error::Error, fmt};

#[derive(Clone, Copy, Debug)]
pub enum ParseError {
    IncorrectToken,
    Other,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::IncorrectToken => f.write_str("Parser encountered incorrect token"),
            ParseError::Other => f.write_str("ICE"),
        }
    }
}
