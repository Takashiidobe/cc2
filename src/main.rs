use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;
use std::process;

use cc2::{CodeGenerator, Lexer, Parser, Preprocessor};

#[derive(ClapParser, Debug)]
#[command(name = "cc2")]
#[command(about = "A C compiler written in Rust", long_about = None)]
struct Args {
    #[arg(help = "Input C source file")]
    input: PathBuf,

    #[arg(short, long, help = "Output assembly file")]
    output: Option<PathBuf>,

    #[arg(long, help = "Print preprocessed source")]
    preprocess_only: bool,

    #[arg(long, help = "Print tokens (lexer output)")]
    lex_only: bool,

    #[arg(long, help = "Print AST (parser output)")]
    parse_only: bool,
}

fn main() {
    let args = Args::parse();

    let source = match fs::read_to_string(&args.input) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", args.input.display(), e);
            process::exit(1);
        }
    };

    // Preprocess the source code
    let mut preprocessor = Preprocessor::new();
    let preprocessed_source = match preprocessor.preprocess(&source) {
        Ok(processed) => processed,
        Err(e) => {
            eprintln!("Preprocessor error: {}", e);
            process::exit(1);
        }
    };

    if args.preprocess_only {
        print!("{}", preprocessed_source);
        return;
    }

    let mut lexer = Lexer::new(&preprocessed_source);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            process::exit(1);
        }
    };

    if args.lex_only {
        for token in &tokens {
            println!("{}", token);
        }
        return;
    }

    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            process::exit(1);
        }
    };

    if args.parse_only {
        println!("{}", ast);
        return;
    }

    let mut codegen = CodeGenerator::new();
    let assembly = match codegen.generate(&ast) {
        Ok(asm) => asm,
        Err(e) => {
            eprintln!("Code generation error: {}", e);
            process::exit(1);
        }
    };

    let output_path = args.output.unwrap_or_else(|| {
        let mut path = args.input.clone();
        path.set_extension("s");
        path
    });

    if let Err(e) = fs::write(&output_path, assembly) {
        eprintln!(
            "Error writing output file '{}': {}",
            output_path.display(),
            e
        );
        process::exit(1);
    }

    println!(
        "Compiled {} to {}",
        args.input.display(),
        output_path.display()
    );
}
