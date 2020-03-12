pub mod mips32_v6;
pub mod spim;

#[macro_export]
macro_rules! create_enum_str_map {
    ($($id:ident { $($opt_id:ident),* $(,)? })*) => {
        $(pub struct $id {}
          #[allow(dead_code)]
          #[allow(non_upper_case_globals)]
          impl $id {
            $(const $opt_id: &'static str = stringify!($opt_id);)*

            pub fn get_vec() -> Vec<&'static str> {
                return vec![$(stringify!($opt_id)),*]
            }
        })*
    }
}

create_enum_str_map! {
    AllMipsDirectives {
        aent,
        alias,
        align,
        arch,
        ascii,
        asciiz,
        asm0,
        bgnb,
        byte,
        comm,
        cpadd,
        cpload,
        cprestore,
        data,
        d_floating,
        double, // same as t_floating
        dword,
        edata,
        eflag,
        end,
        endb,
        endr,
        ent,
        err,
        extended, // same as x_floating
        r#extern,
        f_floating,
        file,
        float, // same as s_floating
        fmask,
        frame,
        g_floating,
        gpword,
        gjaldef,
        gjallive,
        gjsrlive,
        gjrlive,
        gjsrsaved,
        globl,
        gprel32,
        gretlive,
        half,
        lab,
        lcomm,
        lit4,
        lit8,
        livereg,
        loc,
        long,
        mask,
        noalias,
        option,
        prologue,
        quad,
        rconst,
        rdata,
        repeat,
        save_ra,
        sdata,
        set,
        s_floating,
        space,
        r#struct,
        text,
        t_floating,
        tune,
        ugen,
        verstamp,
        vreg,
        weakext,
        word,
        x_floating,

        // Confused about these (they aren't in official doc)
        kdata,
        ktext
    }
    AllMipsInstructions {

    }
}

pub use spim::load_spim_module;
pub use mips32_v6::load_mips32_v6_module;
