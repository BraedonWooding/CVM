extern crate cvm_lib;

use cvm_lib::compiler::*;
use cvm_lib::*;

macro_rules! constant {
    (Int $n:expr) => {
        Expr { kind: Spanned::new(ExprKind::Constant(ConstantKind::Int32($n)), Span::default()),
               type_annot: create_type!(Var "int") }
    };
    (Double $n:expr) => {
        Expr { kind: Spanned::new(ExprKind::Constant(ConstantKind::Flt64($n)), Span::default()),
               type_annot: create_type!(Var "double") }
    };
    (Str $n:expr) => {
        Expr { kind: Spanned::new(ExprKind::Constant(ConstantKind::Str($n)), Span::default()),
               type_annot: create_type!(Pointer (Var "char")) }
    };
    (Char $n:expr) => {
        Expr { kind: Spanned::new(ExprKind::Constant(ConstantKind::Char($n)), Span::default()),
               type_annot: create_type!(Var "char") }
    };
    (Null) => {
        Expr { kind: Spanned::new(ExprKind::Constant(ConstantKind::Null), Span::default()),
               type_annot: create_type!(Pointer (Var "void")) }
    };
    (Bool $n:expr) => {
        Expr { kind: Spanned::new(ExprKind::Constant(ConstantKind::Bool($n)), Span::default()),
               type_annot: create_type!(Var "bool") }
    };
}

macro_rules! test_type {
    { $($type:expr => $val:expr),+ $(,)? } => {
        let table = TypeDefinitionTable::load_type_definition_table();
        $({
            let ty = $type;
            let mut transpiler = Transpiler::new(false, &table);
            transpiler.transpile_type(&ty, TypeOpts::empty());
            assert_eq!(transpiler.get_output(), $val);
        });+
    };
}

#[test]
fn type_tests() {
    test_type! {
        create_type!(Var "int") => "int",
        create_type!(Pointer (Var "int")) => "int *",
        create_type!(Pointer (Pointer (Var "int"))) => "int **",
        create_type!(Array[constant!(Int 5)] (Var "int")) => "int [5]",
        create_type!(Array[constant!(Int 5)] (Pointer (Var "int"))) => "int *[5]",
        create_type!(Func (Var "int"), (Pointer (Var "double")),
                           (Array[constant!(Int 5)] (Pointer (Var "int")))
                    -> (Var "void")) => "void (*)(int, double *, int *[5])",
        create_type!(Pointer (Array[constant!(Int 3)] (Var "int"))) => "int (*)[3]",
        // NOTE: this test and the above one may seem weird...
        //       because there is no function name but that is what they are meant to be
        //       for example in a cast you don't include the function name
        //       the only case you do is declarations!
        // Also the array disappears because it's a function return type
        // and you can't return arrays in C
        create_type!(Func -> (Pointer (Array[constant!(Int 3)] (Var "int")))) => "int **(*)(void)",
    }
}
