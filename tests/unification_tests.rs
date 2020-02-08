extern crate cvm_lib;

use cvm_lib::compiler::ast::*;
use cvm_lib::compiler::scope::*;
use cvm_lib::compiler::type_check::*;
use cvm_lib::compiler::*;
use cvm_lib::*;

use std::collections::HashMap;

macro_rules! test_unify {
    { $(($($a:tt)*) U ($($b:tt)*) => [$($id:ident := ($($val:tt)*)),*]),+ $(,)? } => {
        $({
            let subst = vec![$((string_to_fresh_id(stringify!($id)), create_type!($($val)*))),*];
            let res = TypeCheck::try_unify(&create_type!($($a)*), &create_type!($($b)*));
            assert_eq!(res, subst);
        });*
    };
}

#[test]
fn unification_tests() {
    test_unify! {
        (Fresh a) U (Fresh b) => [a := (Fresh b)],
        (Fresh a) U (Var "int") => [a := (Var "int")],
        (Fresh a) U (Fresh a) => [],
        (Pointer (Fresh a)) U (Pointer (Var "int")) => [a := (Var "int")],
        (Var "int") U (Var "int") => [],
            (Func (Var "int"), (Pointer (Fresh c)), (Fresh c) -> (Fresh b))
        U
            (Func (Fresh c), (Fresh d), (Fresh b) -> (Fresh e))
        => [c := (Var "int"), d := (Pointer (Fresh c)), c := (Fresh b), b := (Fresh e)],
    }
}
