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

mod analyzer;

fn main() -> anyhow::Result<()> {
    let lexerdef = lang_l::lexerdef();
    let args: Vec<String> = std::env::args().collect();

    let mut buffer: String;
    let filename: &str;
    if args.len() == 1 {
        filename = "<stdin>";
        buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
    } else {
        filename = &args[1];
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
        Some(r) => match r {
            Ok(tree) => {
                let typecheck_result = analyzer::Analyzer::typecheck_program(&tree);

                if let Err(errors) = &typecheck_result {
                    for error in errors.iter() {
                        pretty_print_error(&lexer, filename, error);
                    }
                    println!(
                        "{}",
                        Style::new().bold().paint(format!(
                            "Compilation failed. {} errors found.",
                            errors.len()
                        ))
                    );
                    return Ok(());
                }

                let type_cache = typecheck_result.unwrap();

                let mut c = Compiler::new(type_cache);
                let program = c.compile_program(&tree).unwrap();

                std::fs::write(
                    format!(
                        "{}.c",
                        Path::new(&args[1]).file_name().unwrap().to_str().unwrap()
                    ),
                    program,
                )?;
            }
            Err(e) => eprintln!("Parsing Error: {:?}", e),
        },
        _ => eprintln!("Unable to evaluate expression."),
    }

    Ok(())
}

use ansi_term::{Color, Style};
use lrpar::NonStreamingLexer;

fn pretty_print_error<'input, Lexer>(
    lexer: &Lexer,
    filename: &str,
    error: &analyzer::LocSemanticError,
) where
    Lexer: NonStreamingLexer<'input, u32>,
{
    let ((start_line, start_column), (end_line, end_column)) = lexer.line_col(error.location);
    let code = lexer.span_lines_str(error.location);

    println!(
        "{location} {msg}: {error}",
        location = Style::new()
            .bold()
            .paint(format!("{}:{}:{}:", filename, start_line, start_column)),
        msg = Color::Red.bold().paint("error"),
        error = error.data,
    );

    for (line_offset, line_text) in code.lines().enumerate() {
        let line_number = line_offset + start_line;

        println!(
            "{} {}",
            Color::Blue.bold().paint(format!("{:>5} |", line_number)),
            line_text
        );

        let start_offset = if line_number == start_line {
            start_column - 1
        } else {
            0
        };

        let end_offset = if line_number == end_line {
            end_column - 1
        } else {
            line_text.len()
        };

        println!(
            "{} {}",
            Color::Blue.bold().paint(format!("{:>5} |", "")),
            Color::Red.bold().paint(format!(
                "{}{}",
                &" ".repeat(start_offset),
                &"^".repeat(end_offset - start_offset)
            ))
        );
    }
}
