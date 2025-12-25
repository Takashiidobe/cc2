# Testing Conventions

This document outlines the testing conventions for the cc2 compiler project.

## Unit Tests

Unit tests for a module should be in a separate `*_test.rs` file, NOT inline with `#[cfg(test)]` modules.

**Rationale:** This keeps implementation files clean and readable. You can read the implementation without having to scroll through test code.

### File Naming Convention

- Implementation: `src/lexer.rs`
- Unit tests: `src/lexer_test.rs`

- Implementation: `src/parser.rs`
- Unit tests: `src/parser_test.rs`

- Implementation: `src/ast.rs`
- Unit tests: `src/ast_test.rs`

- Implementation: `src/codegen.rs`
- Unit tests: `src/codegen_test.rs`

### Structure

Each `*_test.rs` file should:
1. Import the module being tested
2. Use `#[cfg(test)]` to mark the entire file as test code
3. Organize tests logically by functionality

Example:

```rust
// src/lexer_test.rs
#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    #[test]
    fn test_simple_tokens() {
        // test implementation
    }

    #[test]
    fn test_operators() {
        // test implementation
    }
}
```

## Integration Tests

Integration tests go in the `tests/` directory at the project root.

- `tests/integration_test.rs` - Main integration test suite
- `tests/fixtures/` - Test input files (`.c` files, expected outputs, etc.)

### Integration Test Structure

Integration tests should:
1. Use `assert_cmd` to test the CLI interface
2. Test end-to-end compilation of C programs
3. Verify compiler output against expected results

### Future: Comparison with Reference Compiler

Later in development, we will add tests that compare cc2's output against the actual `cc` compiler to ensure correctness:

```rust
// Future implementation
#[test]
fn test_against_reference_compiler() {
    // Compile with cc2
    let cc2_output = compile_with_cc2("test.c");

    // Compile with cc
    let cc_output = compile_with_cc("test.c");

    // Compare assembly outputs or binary behavior
    assert_eq!(cc2_output, cc_output);
}
```

## Test Organization by Phase

Tests should be organized to reflect the compiler's incremental phases:

### Phase 1: Minimal Integer Calculator
- `tests/phase1/return_literal.c`
- `tests/phase1/arithmetic.c`

### Phase 2: Variables and Functions
- `tests/phase2/variables.c`
- `tests/phase2/function_calls.c`

(And so on for subsequent phases)

## Running Tests

```bash
# Run all tests (unit + integration)
cargo test

# Run only unit tests
cargo test --lib

# Run only integration tests
cargo test --test '*'

# Run tests for specific module
cargo test lexer

# Run with output visible
cargo test -- --nocapture
```

## Test Coverage Goals

- Every public function should have unit tests
- Every compiler phase should have integration tests
- Edge cases and error conditions should be tested
- Tests should be fast and deterministic
