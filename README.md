# cc2 - A C Compiler in Rust

A learning project to build a C compiler from scratch in Rust, following an incremental development approach with test-driven development.

## Project Status

**Currently in Phase 6** - Extended type system complete! The compiler now supports:
- ✅ **Phase 0-5**: Complete (arithmetic, variables, functions, control flow, pointers, arrays, structs)
- ✅ **Phase 6**: Extended types (unsigned integers, multiple sizes, bitwise ops, enums, unions)
- 🔄 **Phase 7**: Documentation update (in progress)
- 📋 **Phase 8+**: Preprocessor and standard library (planned)

See [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) for the detailed roadmap.

## Features

### ✅ Implemented

The cc2 compiler can compile C programs with the following features:

**Basic Features:**
- Integer arithmetic (`+`, `-`, `*`, `/`, `%`)
- Variables and assignments
- Function definitions and calls (System V ABI)
- Return statements

**Control Flow:**
- If/else statements
- While loops
- For loops
- Comparison operators (`<`, `>`, `<=`, `>=`, `==`, `!=`)
- Logical operators (`&&`, `||`, `!`)

**Types and Operators:**
- Multiple integer types: `char`, `short`, `int`, `long`
- Unsigned variants: `unsigned char`, `unsigned short`, `unsigned int`, `unsigned long`
- Pointers and pointer arithmetic
- Arrays and array indexing
- Structs with member access (`.` and `->`)
- Unions with member access
- Enums with auto-incrementing values
- `sizeof` operator
- Bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`)
- Compound assignments (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`)

**Advanced Features:**
- Generalized lvalue assignments (supports `arr[i] = x`, `ptr->field = y`, `*ptr = z`)
- Type-aware code generation (sign extension, zero extension)
- Proper struct/union alignment and padding
- Enum constants as compile-time values

### 📋 Planned

- Preprocessor (`#include`, `#define`, `#ifdef`, etc.)
- Multi-file compilation
- Standard library functions (`printf`, `malloc`, etc.)
- String literals
- Type qualifiers (`const`, `volatile`)
- Storage classes (`static`, `extern`)

## Quick Start

### Building

```bash
cargo build --release
```

### Running Tests

```bash
# Run all tests
cargo test

# Run integration tests only
cargo test --test codegen
cargo test --test parse
cargo test --test tokenize

# Accept snapshot updates
cargo insta accept
```

### Usage

```bash
# Compile a C file to assembly
./target/release/cc2 input.c -o output.S

# Then assemble and link with gcc
gcc output.S -o program
./program
```

## Example

```c
// fibonacci.c
int fib(int n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    return fib(10);  // Returns 55
}
```

```bash
$ ./target/release/cc2 fibonacci.c -o fibonacci.S
Compiled fibonacci.c to fibonacci.S

$ gcc fibonacci.S -o fibonacci
$ ./fibonacci
$ echo $?
55
```

## Project Structure

```
cc2/
├── src/
│   ├── main.rs           # CLI interface
│   ├── lib.rs            # Library exports
│   ├── lexer.rs          # Tokenization
│   ├── lexer_test.rs     # Lexer unit tests
│   ├── parser.rs         # AST construction
│   ├── parser_test.rs    # Parser unit tests
│   ├── ast.rs            # AST node definitions
│   ├── symbol_table.rs   # Symbol table for variables
│   ├── codegen.rs        # x86-64 assembly generation
│   └── codegen_test.rs   # Codegen unit tests
├── tests/
│   ├── codegen.rs        # Integration tests (full pipeline)
│   ├── parse.rs          # Parser integration tests
│   ├── tokenize.rs       # Lexer integration tests
│   ├── files/            # Test C programs (30+ files)
│   └── snapshots/        # Insta snapshot files
├── docs/
│   └── dev/
│       ├── tests.md      # Testing conventions
│       ├── architecture.md  # Architecture documentation
│       └── features.md   # Feature documentation
└── IMPLEMENTATION_PLAN.md # Detailed roadmap
```

## Architecture

The compiler follows a traditional multi-phase design:

```
Source Code (.c)
    ↓
┌─────────┐
│  Lexer  │  Tokenization
└─────────┘
    ↓
┌─────────┐
│ Parser  │  AST Construction (Recursive Descent)
└─────────┘
    ↓
┌──────────────┐
│ Symbol Table │  Track variables, enums, types
└──────────────┘
    ↓
┌────────────┐
│  Codegen   │  Emit x86-64 Assembly (System V ABI)
└────────────┘
    ↓
Assembly (.S)
```

### Key Design Choices

- **Recursive Descent Parser**: Simple, extensible, good error messages
- **Direct Assembly**: No LLVM - generates x86-64 assembly directly for learning
- **System V ABI**: Linux calling convention (parameters in registers, 16-byte stack alignment)
- **Test-Driven**: 30+ integration tests comparing output with GCC/Clang
- **Snapshot Testing**: AST and assembly output validated with `insta`

## Testing

The project has comprehensive test coverage:

- **30+ Integration Tests**: Full C programs in `tests/files/`
- **Comparison Testing**: Output compared with GCC/Clang for correctness
- **Snapshot Testing**: Parser and codegen output validated
- **Unit Tests**: Individual component testing in `src/*_test.rs`

### Test Organization

Tests are organized by feature:
- `return_literal.c`, `arithmetic.c` - Phase 1 (arithmetic)
- `variables.c`, `function_call.c` - Phase 2 (variables, functions)
- `if_else.c`, `while_loop.c`, `for_loop.c` - Phase 3 (control flow)
- `pointers_arrays.c`, `array_sum.c`, `pointer_math.c` - Phase 4 (pointers/arrays)
- `structs.c`, `struct_ops.c` - Phase 5 (structs)
- `enum.c`, `union.c`, `bitwise_ops.c` - Phase 6 (extended types)

## Development

### Workflow

1. **Plan**: Create beads issues for features
2. **Test First**: Write failing integration test
3. **Implement**: Build feature incrementally
4. **Test**: Ensure all tests pass
5. **Refactor**: Clean up code
6. **Document**: Update documentation

### Running the Compiler

```bash
# Development build
cargo run -- tests/files/arithmetic.c -o /tmp/test.S

# Release build
cargo build --release
./target/release/cc2 input.c -o output.S
```

### Issue Tracking

This project uses [beads](https://github.com/beads-tool/beads) for issue tracking:

```bash
# See available work
bd ready

# View issue details
bd show cc2-xxx

# Update issue status
bd update cc2-xxx --status=in_progress

# Close issue
bd close cc2-xxx
```

## Documentation

- [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) - Detailed implementation roadmap
- [docs/dev/architecture.md](docs/dev/architecture.md) - Architecture deep dive
- [docs/dev/features.md](docs/dev/features.md) - Feature documentation
- [docs/dev/tests.md](docs/dev/tests.md) - Testing conventions

## References

- [Crafting Interpreters](https://craftinginterpreters.com/) - Parser design patterns
- [chibicc](https://github.com/rui314/chibicc) - Reference C compiler implementation
- [C11 Standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) - Language specification
- [System V ABI](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf) - x86-64 calling convention
- [Writing a C Compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html) - Tutorial series

## Performance

The compiler is designed for correctness and learning, not performance. However, it's reasonably fast:

- Compiles simple programs in milliseconds
- All 30+ integration tests run in under 1 second
- Memory usage is minimal for typical programs

## Contributing

This is a learning project, but improvements are welcome! Please:

1. Follow the existing code style
2. Add tests for new features
3. Update documentation
4. Create a beads issue for significant changes

## License

MIT

## Acknowledgments

- Inspired by [chibicc](https://github.com/rui314/chibicc) by Rui Ueyama
- Test infrastructure using `datatest-stable` and `insta`
- Issue tracking with [beads](https://github.com/beads-tool/beads)
