extern crate cvm_lib;
use cvm_lib::*;

macro_rules! create_type {
    (Var $id:tt) => {
        Type::Var { id: $id.to_string(), gen_args: vec![] }
    };
    (Pointer ($($inner:tt)+)) => {
        Type::Pointer(Box::new(create_type!($($inner) +)))
    };
    (Array [$len:expr] ($($inner:tt)+)) => {
        Type::Array{inner: Box::new(create_type!($($inner) +)), len: Box::new($len)}
    };
    (Fresh $id:tt) => {
        Type::Fresh { id:$id }
    };
    (Func $(($($args:tt)+)),* -> $($ret:tt)+) => {
        Type::Func{name: None, args: vec![$(create_type!($($args)+)),*], ret: Some(Box::new(create_type!($($ret)+))), gen_args: vec![]}
    };
}

macro_rules! create_constant {
    (Int $n:expr) => {
        Expr { kind: ExprKind::Constant(ConstantKind::Int32($n)), type_annot: Some(create_type!(Var "int")) }
    };
    (Double $n:expr) => {
        Expr { kind: ExprKind::Constant(ConstantKind::Flt64($n)), type_annot: Some(create_type!(Var "double")) }
    };
}

macro_rules! test_type {
    { $($type:expr => $val:expr),+ } => {
        $({
            let ty = $type;
            let mut transpiler = Transpiler::new();
            transpiler.transpile_type(&ty);
            assert_eq!(transpiler.get_output(), $val);
        });+
    };
}

#[test]
fn type_tests() {
    test_type! {
        create_type!(Var "int") => "int",
        create_type!(Pointer (Var "int")) => "int*",
        create_type!(Pointer (Pointer (Var "int"))) => "int**",
        create_type!(Array[create_constant!(Int 5)] (Pointer (Var "int"))) => "int*[5]",
        create_type!(Func (Var "int"), (Pointer (Var "double")),
                           (Array[create_constant!(Int 5)] (Pointer (Var "int")))
                    -> Var "void") => "void(*func_TODO_NO_NAME)(int,double*,int*[5])",
        create_type!(Func -> Pointer (Array[create_constant!(Int 3)] (Var "int"))) => "int(*(*func_TODO_NO_NAME)(void))[3]"
    }
}
