mod compiler;
use compiler::lexer::Lexer;

use std::fs::File;
use std::fs;

extern crate clap;
use clap::{Arg, App, SubCommand};

fn main() -> std::io::Result<()> {
    let matches = App::new("CVM")
        .version("0.1a")
        .author("Braedon Wooding <braedonww@gmail.com>")
        .about("C Simulation Language")
        .args_from_usage(
            "<INPUT>          'Sets the input file to use'")
        .get_matches();

    // safe unwrap
    let filename = matches.value_of("INPUT").unwrap();

    println!("Printing out the tokens for {}", filename);
    let text = fs::read_to_string(filename)?;
    let lexer = Lexer::new(&text);
    for tok in lexer {
        println!("{:?}", tok);
    }

    Ok(())
}
