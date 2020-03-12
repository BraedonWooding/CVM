use crate::*;
use ast::*;
use compiler::*;

extern crate log;
use log::warn;

use std::collections::hash_map::HashMap;

pub struct Transpiler<'a> {
    depth: usize,
    builder: String,
    alpha_types: bool,
    fresh_type_lookup: HashMap<usize, String>,
    type_definition_table: &'a TypeDefinitionTable,
}

