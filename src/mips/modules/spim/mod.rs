use crate::*;
use mips::MipsModule;

create_enum_str_map! {
    AllSpimDirectives {
        align,
        ascii,
        asciiz,
        byte,
        data,
        double,
        r#extern,
        float,
        globl,
        half,
        kdata,
        ktext,
        space,
        text,
        word,
    }
}

pub fn load_spim_module() -> MipsModule {
    MipsModule {
        supported_directives: AllSpimDirectives::get_vec(),
        supported_instructions: vec![],
        // does spim support this??
        allow_symbolic_equate: false
    }
}
