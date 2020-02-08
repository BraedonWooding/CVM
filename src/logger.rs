extern crate log;
use log::{info, trace, warn};

use crate::compiler::ast::*;
use crate::compiler::lexer::*;
use crate::compiler::parser::*;

pub fn log_expected_token(msg: &str, found: &TokenKind) {
    warn!("Was expecting token of type {} found {:?}", msg, found);
}

pub fn log_expected_token_but_eof(msg: &str) {
    warn!(
        "Was expecting token of type {} found end of file instead",
        msg
    );
}
