use std::io::{self, Read};
use std::path::Path;

mod compiler;
mod types;

use compiler::Compiler;

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
                    let typecheck_result = analyzer::Analyzer::typecheck_program(&tree, &buffer);
                    
                    if let Err(errors) = &typecheck_result {
                        println!("{} errors", errors.len());
                        for error in errors.iter() {
                            println!("Error: {}", error);
                        }
                        return Ok(());
                    }
                    
                    let type_cache = typecheck_result.unwrap();

                    let mut c = Compiler::new(type_cache);
                    let program = c.compile_program(&tree).unwrap();

                    std::fs::write(format!("{}.c", Path::new(&args[1]).file_name().unwrap().to_str().unwrap()), program)?;
                }
                Err(e) => eprintln!("Parsing Error: {:?}", e),
            }
        }
        _ => eprintln!("Unable to evaluate expression."),
    }

    Ok(())
}
