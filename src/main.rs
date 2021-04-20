use std::io::{self, Read};

mod types;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

// Using `lrlex_mod!` brings the lexer for `calc.l` into scope. By default the
// module name will be `calc_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("lang.l");
// Using `lrpar_mod!` brings the parser for `calc.y` into scope. By default the
// module name will be `calc_y` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("lang.y");

mod executer;

mod analyzer;

fn main() -> anyhow::Result<()> {
    // Get the `LexerDef` for the `calc` language.
    let lexerdef = lang_l::lexerdef();
    let args: Vec<String> = std::env::args().collect();

    let mut buffer: String;
    if args.len() == 1 {
      buffer = String::new();
      io::stdin().read_to_string(&mut buffer)?;
    } else {
      buffer = std::fs::read_to_string(&args[1]).unwrap();
    }

    // Now we create a lexer with the `lexer` method with which
    // we can lex an input.
    let lexer = lexerdef.lexer(&buffer);
    // Pass the lexer to the parser and lex and parse the input.
    let (res, errs) = lang_y::parse(&lexer);
    for e in &errs {
        println!("{}", e.pp(&lexer, &lang_y::token_epp));
    }
    if errs.len() != 0 {
        return Ok(());
    }

    match res {
        Some(r) => {
            match r {
                Ok(tree) => {
                    //execute(&tree).unwrap()
                    // dbg!(&tree);
                    // dbg!(analyzer::Analyzer::typecheck_program(&tree))?;
                    analyzer::Analyzer::typecheck_program(&tree)?;
                    println!("Program parsed with no errors");
                }
                Err(e) => eprintln!("Parsing Error: {:?}", e),
            }
        }
        _ => eprintln!("Unable to evaluate expression."),
    }

    Ok(())
}
