#[macro_use]
extern crate enum_as_inner;

mod compiler;
use compiler::lexer::Lexer;
use compiler::parser::Parser;

use std::fs::File;
use std::fs;

extern crate clap;
use clap::{Arg, App, SubCommand};

mod logger;

extern crate log;
extern crate simple_logger;

fn main() -> std::io::Result<()> {
    simple_logger::init().unwrap();

    let matches = App::new("CVM")
        .version("0.1a")
        .author("Braedon Wooding <braedonww@gmail.com>")
        .about("C Virtual Simulation Language")
        .subcommand(SubCommand::with_name("build")
            .args_from_usage(
                "-o --output=[FILE] 'Sets the output file to use'
                 -c --out-c         'Compile to C'
                 <INPUT>            'Sets the input file to use'")
        )
        .subcommand(SubCommand::with_name("dev")
            .args_from_usage(
                "--tokens           'Get the output of the tokenizer'
                 --ast              'Construct and print the AST'
                 --basic-typed-ast  'Perform first step type assignment on AST'
                 --typed-ast        'Fully resolve all types'
                 --bytecode         'Print out the resultant bytecode'
                 <INPUT>            'Sets the input file to use'")
        )
        .get_matches();

    match matches.subcommand() {
        ("build", Some(_)) => {
            // let filename = matches.value_of("INPUT").unwrap();
            // TODO:
        },
        ("dev", Some(sub_matches)) => {
            let filename = matches.value_of("INPUT").unwrap();
            let text = fs::read_to_string(filename)?;
            let lexer = Lexer::new(&text);
            if sub_matches.is_present("tokens") {
                println!("== Tokenizer Output Started ==");
                for tok in lexer.clone() { println!("{:?}", tok); }
                println!("== Tokenizer Output Finished ==");
            }
            // most steps require the previous ones to exist
            // so we always generate each step... it is just more that
            // we print out the itermediaries at each point
            let ast = Parser::parse_program(lexer);

            if sub_matches.is_present("ast") {
                println!("== AST Output Started ==");
                println!("{:?}", ast);
                println!("== AST Output Started ==");
            }

            // and so on...
            // TODO: Rest
        }
        _ => {}
    }

    Ok(())
}
