# Testing Conventions

This document outlines the testing conventions and infrastructure for the cc2 compiler project.

## Table of Contents

- [Overview](#overview)
- [Unit Tests](#unit-tests)
- [Integration Tests](#integration-tests)
- [Snapshot Testing](#snapshot-testing)
- [Test Files](#test-files)
- [Running Tests](#running-tests)
- [Writing New Tests](#writing-new-tests)

## Overview

The cc2 compiler has comprehensive test coverage using three testing approaches:

1. **Unit Tests**: Test individual components (lexer, parser, codegen) in isolation
2. **Integration Tests**: Test the full compilation pipeline with real C programs
3. **Snapshot Testing**: Validate parser and codegen output using `insta` crate

All tests run automatically with `cargo test` and are designed to be:
- **Fast**: Complete test suite runs in under 1 second
- **Deterministic**: Same input always produces same output
- **Comprehensive**: 30+ test files covering all implemented features

## Unit Tests

Unit tests for a module are in separate `*_test.rs` files, NOT inline with `#[cfg(test)]` modules.

**Rationale:** This keeps implementation files clean and readable. You can read the implementation without having to scroll through test code.

### File Naming Convention

| Implementation | Unit Tests |
|---------------|------------|
| `src/lexer.rs` | `src/lexer_test.rs` |
| `src/parser.rs` | `src/parser_test.rs` |
| `src/codegen.rs` | `src/codegen_test.rs` |
| `src/ast.rs` | `src/ast_test.rs` |

### Structure

Each `*_test.rs` file should:
1. Import the module being tested
2. Use `#[cfg(test)]` to mark the entire file as test code
3. Organize tests logically by functionality
4. Test edge cases and error conditions

**Example:**

```rust
// src/lexer_test.rs
#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    #[test]
    fn test_simple_tokens() {
        let input = "int main() { return 42; }";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens[0], Token::Int);
        assert_eq!(tokens[1], Token::Identifier("main".to_string()));
        // ... more assertions
    }

    #[test]
    fn test_operators() {
        let input = "+ - * / %";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens[0], Token::Plus);
        assert_eq!(tokens[1], Token::Minus);
        // ... more assertions
    }
}
```

### Current Unit Tests

- **lexer_test.rs**: Tokenization of keywords, operators, literals, identifiers
- **parser_test.rs**: AST construction for expressions, statements, declarations
- **codegen_test.rs**: Assembly generation for specific constructs

## Integration Tests

Integration tests are in the `tests/` directory and test the full compilation pipeline.

### Test Infrastructure

The project uses two key crates for integration testing:

1. **datatest-stable**: Automatically discovers and runs test files
2. **insta**: Snapshot testing for AST and assembly output

### Integration Test Files

Three main integration test files:

| Test File | Purpose |
|-----------|---------|
| `tests/tokenize.rs` | Tests lexer output (token streams) |
| `tests/parse.rs` | Tests parser output (AST snapshots) |
| `tests/codegen.rs` | Tests full compilation (assembly + execution) |

**Example from tests/codegen.rs:**

```rust
use cc2::{compile, tokenize, parse};
use datatest_stable::harness;
use std::fs;
use std::process::Command;

fn test_codegen(path: &std::path::Path) -> datatest_stable::Result<()> {
    // Read C source file
    let source = fs::read_to_string(path)?;

    // Compile with cc2
    let tokens = tokenize(&source)?;
    let ast = parse(&tokens)?;
    let assembly = compile(&ast)?;

    // Write assembly to temp file
    let asm_path = "/tmp/test.S";
    fs::write(asm_path, assembly)?;

    // Assemble and link with GCC
    let output = Command::new("gcc")
        .args(&[asm_path, "-o", "/tmp/test"])
        .output()?;
    assert!(output.status.success());

    // Execute and check exit code
    let result = Command::new("/tmp/test").output()?;
    let exit_code = result.status.code().unwrap();

    // Compare with GCC compilation
    let gcc_output = Command::new("gcc")
        .args(&[path.to_str().unwrap(), "-o", "/tmp/test_gcc"])
        .output()?;
    assert!(gcc_output.status.success());

    let gcc_result = Command::new("/tmp/test_gcc").output()?;
    let gcc_exit_code = gcc_result.status.code().unwrap();

    // Verify cc2 and GCC produce same result
    assert_eq!(exit_code, gcc_exit_code);

    Ok(())
}

// Automatically discover and run all .c files in tests/files/
harness!(test_codegen, "tests/files", r"^.*\.c$");
```

### Test Discovery

The `datatest-stable` harness automatically:
1. Finds all `.c` files in `tests/files/`
2. Runs the test function on each file
3. Reports results for each test individually

This means adding a new test is as simple as creating a new `.c` file in `tests/files/`!

### Comparison with GCC/Clang

Integration tests compare cc2's output with GCC to ensure correctness:

1. **Compile with cc2**: Generate assembly from C source
2. **Assemble with GCC**: Use GCC to assemble cc2's output
3. **Execute**: Run the resulting binary
4. **Compare with GCC**: Compile the same C source with GCC and compare exit codes

This ensures that cc2 generates correct code that produces the same results as GCC.

## Snapshot Testing

The project uses `insta` for snapshot testing of parser and codegen output.

### How Snapshot Testing Works

1. **First run**: Generate output and save as snapshot
2. **Subsequent runs**: Compare output against saved snapshot
3. **Changes detected**: Review with `cargo insta review`
4. **Accept changes**: Run `cargo insta accept`

### Snapshot Files

Snapshots are stored in `tests/snapshots/`:

```
tests/snapshots/
├── tokenize__arithmetic.c.snap
├── tokenize__enum.c.snap
├── parse__arithmetic.c.snap
├── parse__enum.c.snap
├── codegen__arithmetic.c.snap
└── codegen__enum.c.snap
```

### Example: Parser Snapshot Test

```rust
// tests/parse.rs
use cc2::{tokenize, parse};
use datatest_stable::harness;
use std::fs;

fn test_parse(path: &std::path::Path) -> datatest_stable::Result<()> {
    let source = fs::read_to_string(path)?;
    let tokens = tokenize(&source)?;
    let ast = parse(&tokens)?;

    // Snapshot test: compare AST with saved snapshot
    insta::assert_debug_snapshot!(ast);

    Ok(())
}

harness!(test_parse, "tests/files", r"^.*\.c$");
```

### Reviewing Snapshot Changes

When parser or codegen output changes:

```bash
# Run tests (will fail if snapshots don't match)
cargo test

# Review snapshot differences
cargo insta review

# Accept all changes
cargo insta accept
```

The `cargo insta review` command shows a nice diff and lets you:
- Accept individual changes
- Reject individual changes
- Skip and decide later

## Test Files

Test files are organized in `tests/files/` by feature and phase.

### Test File Organization

| Phase | Feature | Test Files |
|-------|---------|------------|
| 1 | Arithmetic | `return_literal.c`, `arithmetic.c`, `add_5_2.c` |
| 2 | Variables & Functions | `variables.c`, `function_call.c` |
| 3 | Control Flow | `if_else.c`, `while_loop.c`, `for_loop.c`, `comparisons.c`, `logical_ops.c`, `control_mix.c` |
| 4 | Pointers & Arrays | `pointers_arrays.c`, `array_sum.c`, `pointer_math.c`, `sizeof.c` |
| 5 | Structs | `structs.c`, `struct_ops.c` |
| 6 | Extended Types | `unsigned_ops.c`, `int_types.c`, `bitwise_ops.c`, `compound_assign.c`, `enum.c`, `union.c` |

### Writing Test Files

Each test file should:

1. **Be self-contained**: No external dependencies or includes
2. **Have a main function**: Return an integer exit code
3. **Test one feature**: Focus on a specific capability
4. **Use exit code**: Return value indicates success/failure
5. **Be deterministic**: Same input always produces same output

**Example test file:**

```c
// tests/files/enum.c
// Tests: Enum declarations, auto-increment, explicit values

enum Color {
    RED,        // 0
    GREEN,      // 1
    BLUE        // 2
};

enum Status {
    OK = 0,
    ERROR = 1,
    PENDING = 5,
    DONE        // 6 (auto-incremented from 5)
};

int main() {
    enum Color c = RED;
    enum Status s = PENDING;
    int x = GREEN;
    int y = DONE;

    // Returns 12 (0 + 5 + 1 + 6)
    return c + s + x + y;
}
```

### Test File Naming Convention

- Use descriptive names: `pointer_math.c`, not `test3.c`
- Group related tests: `unsigned_ops.c`, `unsigned_compare.c`
- Indicate the feature: `bitwise_ops.c`, `compound_assign.c`

## Running Tests

### Run All Tests

```bash
# Run all tests (unit + integration + snapshots)
cargo test
```

Output:
```
running 35 tests
test lexer_tests::test_simple_tokens ... ok
test parser_tests::test_function ... ok
test codegen::arithmetic_c ... ok
test codegen::enum_c ... ok
...
test result: ok. 35 passed; 0 failed
```

### Run Specific Test Categories

```bash
# Run only unit tests
cargo test --lib

# Run only integration tests
cargo test --test codegen
cargo test --test parse
cargo test --test tokenize

# Run specific test file
cargo test --test codegen -- arithmetic
```

### Run with Output

```bash
# Show output from tests
cargo test -- --nocapture

# Show test names as they run
cargo test -- --nocapture --test-threads=1
```

### Snapshot Testing Commands

```bash
# Accept all new/changed snapshots
cargo insta accept

# Review snapshots interactively
cargo insta review

# Test and auto-accept (useful during development)
cargo insta test --review
```

## Writing New Tests

### Adding a Unit Test

1. Open or create `src/<module>_test.rs`
2. Add a new `#[test]` function
3. Test specific functionality
4. Run `cargo test` to verify

**Example:**

```rust
// src/parser_test.rs
#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    #[test]
    fn test_new_feature() {
        let input = "/* your test input */";
        let tokens = tokenize(input).unwrap();
        let ast = parse(&tokens).unwrap();

        // Assert expected AST structure
        match &ast {
            AstNode::Program(nodes) => {
                assert_eq!(nodes.len(), 1);
                // ... more assertions
            }
            _ => panic!("Expected Program node"),
        }
    }
}
```

### Adding an Integration Test

1. Create a new `.c` file in `tests/files/`
2. Write a self-contained C program
3. Return an integer exit code
4. Run `cargo test` - the test is automatically discovered!
5. Accept snapshots with `cargo insta accept`

**Example:**

```c
// tests/files/my_new_feature.c
// Tests: Some new feature

int main() {
    // Test code here
    int result = /* test logic */;

    // Return expected value
    return result;  // e.g., 42
}
```

Then run:

```bash
cargo test
cargo insta review  # Review AST and assembly snapshots
cargo insta accept  # Accept if correct
```

### Test-Driven Development

The recommended workflow:

1. **Write failing test**: Create test file that uses new feature
2. **Run test**: `cargo test` - should fail
3. **Implement feature**: Add lexer/parser/codegen support
4. **Run test**: `cargo test` - should pass
5. **Accept snapshots**: `cargo insta accept`
6. **Refactor**: Clean up code, tests still pass

This ensures:
- Features are testable
- Regressions are caught
- Code stays working

## Test Coverage

### Current Test Coverage

- **30+ integration tests**: Full C programs testing all features
- **Unit tests**: Core components (lexer, parser, codegen)
- **Snapshot tests**: AST and assembly validation
- **Comparison tests**: Output verified against GCC

### Coverage Goals

- Every public function has unit tests
- Every compiler phase has integration tests
- Every feature has at least one test file
- Edge cases and error conditions are tested
- Tests are fast (< 1 second total)

### Checking Coverage

```bash
# Install cargo-tarpaulin
cargo install cargo-tarpaulin

# Generate coverage report
cargo tarpaulin --out Html

# Open coverage report
open tarpaulin-report.html
```

## Debugging Tests

### Viewing Compiler Output

```bash
# Compile a test file manually
cargo run -- tests/files/arithmetic.c -o /tmp/test.S

# View generated assembly
cat /tmp/test.S

# Assemble and run
gcc /tmp/test.S -o /tmp/test
/tmp/test
echo $?  # Print exit code
```

### Debugging Failed Tests

When a test fails:

1. **Check the error message**: What assertion failed?
2. **View snapshots**: `cargo insta review` shows differences
3. **Compile manually**: Run cc2 on the test file
4. **Compare with GCC**: Compile with GCC and compare output
5. **Add debug output**: Use `println!` in codegen/parser
6. **Isolate the issue**: Create minimal test case

### Common Test Failures

**Snapshot mismatch:**
```
snapshot assertion for 'codegen__enum_c' failed
```
→ Run `cargo insta review` to see changes

**Execution mismatch:**
```
assertion failed: `(left == right)`
  left: `12`,
 right: `11`
```
→ cc2 and GCC produced different exit codes, check assembly

**Compilation error:**
```
Error: Undefined variable 'x'
```
→ Bug in symbol table or parser

## Continuous Integration

Tests run automatically on every commit via GitHub Actions (future).

**Planned CI workflow:**

```yaml
# .github/workflows/test.yml
name: Test
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - run: cargo test
      - run: cargo insta test --review
```

## Best Practices

1. **Test one thing**: Each test should verify one feature
2. **Use descriptive names**: `test_enum_auto_increment` not `test1`
3. **Keep tests small**: Short, focused tests are easier to debug
4. **Test edge cases**: Zero, negative, maximum values
5. **Test errors**: Invalid input should produce errors
6. **Keep tests fast**: Avoid slow operations
7. **Make tests deterministic**: No random values or timing dependencies
8. **Document test intent**: Add comments explaining what's being tested

## Summary

The cc2 test infrastructure provides:

- **Automated testing**: `datatest-stable` discovers tests automatically
- **Snapshot validation**: `insta` ensures output consistency
- **GCC comparison**: Verifies correctness against reference compiler
- **Fast feedback**: All tests run in under 1 second
- **Easy additions**: Just drop a `.c` file in `tests/files/`

This makes it easy to:
- Add new features with confidence
- Catch regressions early
- Verify correctness
- Maintain code quality
