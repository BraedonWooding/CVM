use crate::*;
use mips::MipsModule;
use mips::modules::*;

pub fn load_mips32_v6_module() -> MipsModule {
    MipsModule {
        // NOTE: this may not be true due to kdata/ktext
        supported_directives: AllMipsDirectives::get_vec(),
        supported_instructions: vec![],
        allow_symbolic_equate: true
    }
}
