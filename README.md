# cc2 - A C Compiler in Rust

A learning project to build a C compiler from scratch in Rust, following an incremental development approach.

## Project Status

Currently in **Phase 0** - Basic infrastructure is set up.

See [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) for the full roadmap.

## Features

The compiler is being built incrementally through several phases:

- **Phase 0** (COMPLETE): Project setup and basic infrastructure
- **Phase 1** (TODO): Minimal integer calculator - compile `int main() { return 42; }`
- **Phase 2** (TODO): Variables and function calls
- **Phase 3** (TODO): Control flow (if/else, loops)
- **Phase 4** (TODO): Pointers and arrays
- **Phase 5** (TODO): Structs and complex types
- **Phase 6** (TODO): Preprocessor and multi-file compilation

## Building

```bash
cargo build
```

## Testing

```bash
cargo test
```

## Usage

```bash
# Compile a C file to assembly
cc2 input.c

# Specify output file
cc2 input.c -o output.s

# View lexer output (tokens)
cc2 input.c --lex-only

# View parser output (AST)
cc2 input.c --parse-only
```

## Project Structure

```
cc2/
├── src/
│   ├── main.rs      # CLI interface
│   ├── lib.rs       # Library exports
│   ├── lexer.rs     # Tokenization
│   ├── parser.rs    # AST construction
│   ├── ast.rs       # AST node definitions
│   └── codegen.rs   # x86-64 assembly generation
├── tests/
│   ├── integration_test.rs  # Integration tests
│   └── fixtures/            # Test input files
└── IMPLEMENTATION_PLAN.md   # Detailed roadmap
```

## Architecture

The compiler follows a traditional multi-phase design:

1. **Lexer**: Converts source text into tokens
2. **Parser**: Builds an Abstract Syntax Tree (AST) from tokens using recursive descent
3. **Code Generator**: Emits x86-64 assembly (Linux System V ABI)

Currently targeting x86-64 Linux. Future phases may add support for other platforms.

## Development Workflow

For each phase:
1. Write failing tests for new features
2. Implement features incrementally
3. Ensure all tests pass
4. Refactor and document

## References

- [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) - Detailed implementation plan
- [Crafting Interpreters](https://craftinginterpreters.com/) - Parser design
- [chibicc](https://github.com/rui314/chibicc) - Reference C compiler
- [C11 Standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf)
- [System V ABI](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)

## License

MIT
