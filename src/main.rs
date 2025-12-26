use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;
use std::process;

use cc2::{format_error, format_simple_error, CodeGenerator, Lexer, Parser, Preprocessor, SourceLocation};

#[derive(ClapParser, Debug)]
#[command(name = "cc2")]
#[command(about = "A C compiler written in Rust", long_about = None)]
struct Args {
    #[arg(help = "Input C source file(s)", required = true)]
    inputs: Vec<PathBuf>,

    #[arg(short, long, help = "Output file (executable or assembly)")]
    output: Option<PathBuf>,

    #[arg(short = 'c', help = "Compile to object files only, do not link")]
    compile_only: bool,

    #[arg(short = 'I', long = "include", help = "Add include search path")]
    include_paths: Vec<String>,

    #[arg(long = "isystem", help = "Add system include search path")]
    system_include_paths: Vec<String>,

    #[arg(long = "nostdinc", help = "Do not search standard system directories")]
    no_std_includes: bool,

    #[arg(long, help = "Print preprocessed source")]
    preprocess_only: bool,

    #[arg(long, help = "Print tokens (lexer output)")]
    lex_only: bool,

    #[arg(long, help = "Print AST (parser output)")]
    parse_only: bool,
}

/// Extract location from error message like "... at 3:5"
fn extract_location(error_msg: &str) -> Option<SourceLocation> {
    // Look for "at line:column" pattern at the end
    if let Some(at_pos) = error_msg.rfind(" at ") {
        let location_str = &error_msg[at_pos + 4..];
        let parts: Vec<&str> = location_str.split(':').collect();
        if parts.len() == 2 {
            if let (Ok(line), Ok(col)) = (parts[0].parse(), parts[1].parse()) {
                return Some(SourceLocation::new(line, col));
            }
        }
    }
    None
}

/// Strip location from error message
fn strip_location(error_msg: &str) -> &str {
    if let Some(at_pos) = error_msg.rfind(" at ") {
        &error_msg[..at_pos]
    } else {
        error_msg
    }
}

fn compile_file_to_assembly(input: &PathBuf, args: &Args) -> Result<String, String> {
    let source = fs::read_to_string(input)
        .map_err(|e| format!("Error reading file '{}': {}", input.display(), e))?;

    // Preprocess the source code
    let mut preprocessor = if args.no_std_includes {
        Preprocessor::new_no_std()
    } else {
        Preprocessor::new()
    };

    // Add user-specified include paths
    for path in &args.include_paths {
        preprocessor.add_include_path(path.clone());
    }

    // Add system include paths
    for path in &args.system_include_paths {
        preprocessor.add_system_include_path(path.clone());
    }

    // Set the current file for resolving relative includes
    preprocessor.set_current_file(input.display().to_string());

    let preprocessed_source = preprocessor
        .preprocess(&source)
        .map_err(|e| format_simple_error(&format!("Preprocessor error in '{}': {}", input.display(), e)))?;

    let mut lexer = Lexer::new(&preprocessed_source);
    let tokens = lexer
        .tokenize()
        .map_err(|e| format_simple_error(&format!("Lexer error in '{}': {}", input.display(), e)))?;

    let mut parser = Parser::new(tokens);
    let ast = parser
        .parse()
        .map_err(|e| {
            // Try to extract location and format with context
            if let Some(location) = extract_location(&e) {
                let message = strip_location(&e);
                format_error(
                    &input.display().to_string(),
                    &preprocessed_source,
                    location,
                    message,
                )
            } else {
                format_simple_error(&format!("Parser error in '{}': {}", input.display(), e))
            }
        })?;

    let mut codegen = CodeGenerator::new();
    let assembly = codegen
        .generate(&ast)
        .map_err(|e| format_simple_error(&format!("Code generation error in '{}': {}", input.display(), e)))?;

    Ok(assembly)
}

fn compile_to_object(input: &PathBuf, args: &Args) -> Result<PathBuf, String> {
    // Compile to assembly
    let assembly = compile_file_to_assembly(input, args)?;

    // Write assembly to temporary file
    let mut asm_path = input.clone();
    asm_path.set_extension("s");
    fs::write(&asm_path, assembly).map_err(|e| {
        format!(
            "Error writing assembly file '{}': {}",
            asm_path.display(),
            e
        )
    })?;

    // Assemble to object file
    let mut obj_path = input.clone();
    obj_path.set_extension("o");

    let status = process::Command::new("as")
        .args(&["-o", obj_path.to_str().unwrap(), asm_path.to_str().unwrap()])
        .status()
        .map_err(|e| format!("Failed to run assembler: {}", e))?;

    if !status.success() {
        return Err(format!("Assembler failed for '{}'", input.display()));
    }

    // Clean up assembly file
    let _ = fs::remove_file(&asm_path);

    Ok(obj_path)
}

fn link_objects(object_files: &[PathBuf], output: &PathBuf) -> Result<(), String> {
    let mut cmd = process::Command::new("gcc");
    cmd.arg("-o").arg(output);

    for obj in object_files {
        cmd.arg(obj);
    }

    let status = cmd
        .status()
        .map_err(|e| format!("Failed to run linker: {}", e))?;

    if !status.success() {
        return Err("Linker failed".to_string());
    }

    Ok(())
}

fn main() {
    let args = Args::parse();

    if args.inputs.is_empty() {
        eprintln!("Error: No input files specified");
        process::exit(1);
    }

    // Handle single-file modes (preprocess-only, lex-only, parse-only)
    if args.preprocess_only || args.lex_only || args.parse_only {
        if args.inputs.len() > 1 {
            eprintln!(
                "Error: Only one input file allowed with --preprocess-only, --lex-only, or --parse-only"
            );
            process::exit(1);
        }

        let input = &args.inputs[0];
        let source = match fs::read_to_string(input) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading file '{}': {}", input.display(), e);
                process::exit(1);
            }
        };

        let mut preprocessor = if args.no_std_includes {
            Preprocessor::new_no_std()
        } else {
            Preprocessor::new()
        };

        for path in &args.include_paths {
            preprocessor.add_include_path(path.clone());
        }

        for path in &args.system_include_paths {
            preprocessor.add_system_include_path(path.clone());
        }

        // Set the current file for resolving relative includes
        preprocessor.set_current_file(input.display().to_string());

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
    }

    // Single file compilation to assembly (classic mode)
    if args.inputs.len() == 1 && !args.compile_only {
        let input = &args.inputs[0];
        match compile_file_to_assembly(input, &args) {
            Ok(assembly) => {
                let output_path = args.output.clone().unwrap_or_else(|| {
                    let mut path = input.clone();
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

                println!("Compiled {} to {}", input.display(), output_path.display());
            }
            Err(e) => {
                eprintln!("{}", e);
                process::exit(1);
            }
        }
        return;
    }

    // Multi-file compilation or -c mode
    let mut object_files = Vec::new();

    for input in &args.inputs {
        match compile_to_object(input, &args) {
            Ok(obj_path) => {
                println!("Compiled {} to {}", input.display(), obj_path.display());
                object_files.push(obj_path);
            }
            Err(e) => {
                eprintln!("{}", e);
                process::exit(1);
            }
        }
    }

    // If -c flag, we're done (object files created)
    if args.compile_only {
        return;
    }

    // Link object files
    let output_path = args
        .output
        .clone()
        .unwrap_or_else(|| PathBuf::from("a.out"));

    match link_objects(&object_files, &output_path) {
        Ok(()) => {
            println!(
                "Linked {} files to {}",
                object_files.len(),
                output_path.display()
            );
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }

    // Clean up object files after linking
    for obj in &object_files {
        let _ = fs::remove_file(obj);
    }
}
