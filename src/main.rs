extern crate enum_as_inner;
extern crate lazy_static;
extern crate libc;
extern crate num;

use std::fs;

extern crate clap;
use clap::{App, SubCommand};

extern crate log;
extern crate simple_logger;

#[macro_use]
extern crate cvm_lib;
use cvm_lib::*;

fn main() -> std::io::Result<()> {
    simple_logger::init().unwrap();

    let matches = App::new("CVM")
        .version("0.1a")
        .author("Braedon Wooding <braedonww@gmail.com>")
        .about("C Virtual Simulation Language")
        .subcommand(SubCommand::with_name("build").args_from_usage(
            "
            -o --output=[FILE] 'Sets the output file to use'
            -c --out-c         'Compile to C'
            <INPUT>            'Sets the input file to use'
            ",
        ))
        .subcommand(SubCommand::with_name("dev").args_from_usage(
            "
            --tokens           'Get the output of the tokenizer'
            --ast              'Construct and print the AST'
            --basic-typed-ast  'Perform first step type assignment on AST'
            --typed-ast        'Fully resolve all types'
            --bytecode         'Print out the resultant bytecode'
            --transpile-c      'Transpile the AST to C'
            --alpha-types      'Use alphabetical type names for fresh type variables'
            --type-tables      'Print out all the type tables'
            <INPUT>            'Sets the input file to use'
            ",
        ))
        .get_matches();

    match matches.subcommand() {
        ("build", Some(_)) => {
            // let filename = matches.value_of("INPUT").unwrap();
            // TODO:
        }
        ("dev", Some(sub_matches)) => {
            let filename = sub_matches.value_of("INPUT").unwrap();
            let text = fs::read_to_string(filename)?;
            let lexer = Lexer::new(&text);
            if sub_matches.is_present("tokens") {
                println!("== Tokenizer Output Started ==");
                for tok in lexer.clone() {
                    println!("{:?}", tok);
                }
                println!("== Tokenizer Output Finished ==");
            }
            // most steps require the previous ones to exist
            // so we always generate each step... it is just more that
            // we print out the itermediaries at each point
            let (mut ast, mut stack) = match Parser::parse_program(lexer, filename) {
                (Some(ast), stack) => (ast, stack),
                (None, ..) => return Ok(()),
            };

            if sub_matches.is_present("ast") {
                println!("== AST Output Started ==");
                println!("{:?}", ast);
                println!("== AST Output Finished ==");
            }

            TypeInfer::type_infer_program(&mut ast, &mut stack);

            if sub_matches.is_present("basic-typed-ast") {
                println!("== Basic Typed AST Started ==");
                println!("{:#?}", ast);
                println!("== Basic Typed AST Finished ==");
            }

            let table = compiler::TypeDefinitionTable::load_type_definition_table();
            TypeCheck::type_check_program(&mut ast, &mut stack, &table);

            if sub_matches.is_present("typed-ast") {
                println!("== Fully Typed AST Started ==");
                println!("{:#?}", ast);
                println!("== Fully Typed AST Finished ==");
            }

            if sub_matches.is_present("type-tables") {
                println!("== Type Tables Started ==");
                println!("{:#?}", stack.fresh_type_env);
                println!("== Type Tables Finished ==");
            }

            if sub_matches.is_present("transpile-c") {
                println!("== Transpilation to C Started ==");
                let mut transpiler =
                    c_transpiler::Transpiler::new(sub_matches.is_present("alpha-types"), &table);
                transpiler.transpile_program(&ast);
                let output = transpiler.get_output();
                println!("== Transpilation to C Finished ==");
                println!("== Result of C Transpilation Started ==");
                println!("{}", output);
                println!("== Result of C Transpilation Finished ==");
            }

            // and so on...
            // TODO: Rest
        }
        _ => {}
    }

    Ok(())
}
