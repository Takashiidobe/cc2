# C Compiler in Rust - Incremental Implementation Plan

This document outlines the incremental approach to building a C compiler in Rust. Each phase builds on the previous one, ensuring we always have a working (if limited) compiler at each step.

## Architecture Overview

The compiler follows a traditional multi-phase design:

1. **Lexer (Tokenizer)**: Converts source text into tokens
2. **Parser**: Builds an Abstract Syntax Tree (AST) from tokens
3. **Semantic Analyzer**: Type checking and semantic validation
4. **Code Generator**: Emits x86-64 assembly (targeting Linux ABI initially)

## Design Decisions

### Language: Rust
- Memory safety without garbage collection
- Strong type system helps prevent compiler bugs
- Excellent pattern matching for AST traversal
- Growing ecosystem for compiler tools

### Target Architecture: x86-64
- Start with Linux System V ABI
- Direct assembly generation (no LLVM initially for learning purposes)
- Can add other backends later

### Parsing Strategy: Recursive Descent
- Simple to implement and understand
- Easy to extend incrementally
- Good error messages possible
- Sufficient for C's grammar

### Testing Strategy
- Test-driven development with .c input files
- Compare output with GCC/Clang for correctness
- Integration tests that compile and run programs
- Unit tests for individual components

## Implementation Phases

### Phase 0: Project Setup and Basic Infrastructure
**Priority: P1** | **Beads ID: cc2-786**

Set up the Rust project structure and basic tooling.

**Deliverables:**
- Cargo project with proper structure (src/lexer, src/parser, src/codegen)
- CLI interface for the compiler
- Test infrastructure
- Documentation setup
- CI/CD basic configuration

**Success Criteria:**
- `cargo build` succeeds
- Can invoke `cc2 input.c` (even if it does nothing yet)
- Tests can run with `cargo test`

---

### Phase 1: Minimal Integer Calculator
**Priority: P1** | **Beads ID: cc2-0oj** | **Depends on: Phase 0**

Build the absolute minimum viable compiler that can compile:
```c
int main() { return 42; }
```

**Components:**
- **Lexer**: Recognize tokens: `int`, `main`, `(`, `)`, `{`, `}`, `return`, integers, `;`
- **Parser**: Parse function definition with single return statement
- **AST**: Minimal tree structure (Function, ReturnStmt, IntLiteral)
- **Codegen**: Emit x86-64 assembly with proper prologue/epilogue

**Extended to handle:**
```c
int main() { return 2 + 3 * 4; }  // Arithmetic expressions
```

**New Components:**
- Lexer: operators `+`, `-`, `*`, `/`
- Parser: Expression parsing with operator precedence
- AST: BinaryOp nodes
- Codegen: Register allocation for expressions

**Success Criteria:**
- Can compile and run programs that return integer literals
- Can compile and run programs with arithmetic expressions
- Correct operator precedence (3 + 4 * 5 = 23, not 35)

---

### Phase 2: Variables and Function Calls
**Priority: P2** | **Beads ID: cc2-7ep** | **Depends on: Phase 1**

Add support for local variables and function calls:
```c
int add(int a, int b) {
    return a + b;
}

int main() {
    int x = 5;
    int y = 3;
    return add(x, y);
}
```

**Components:**
- **Lexer**: Identifiers
- **Parser**: Variable declarations, function parameters, function calls
- **AST**: VarDecl, Identifier, FunctionCall nodes
- **Semantic Analysis**: Symbol table, scope management
- **Codegen**: Stack frame management, calling convention (System V ABI)

**Success Criteria:**
- Can compile programs with local variables
- Can compile programs with multiple functions
- Function arguments passed correctly
- Return values work correctly

---

### Phase 3: Control Flow
**Priority: P2** | **Beads ID: cc2-oq5** | **Depends on: Phase 2**

Add conditional statements and loops:
```c
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    int result = 1;
    for (int i = 2; i <= n; i++) {
        result = result * i;
    }
    return result;
}
```

**Components:**
- **Lexer**: Keywords `if`, `else`, `while`, `for`, comparison operators `<`, `>`, `<=`, `>=`, `==`, `!=`
- **Parser**: If statements, while loops, for loops
- **AST**: IfStmt, WhileStmt, ForStmt, ComparisonOp nodes
- **Codegen**: Labels and jumps, condition code handling

**Success Criteria:**
- Can compile if/else statements
- Can compile while loops
- Can compile for loops
- Comparison operators work correctly
- Can implement algorithms like factorial, fibonacci

---

### Phase 4: Pointers and Arrays
**Priority: P2** | **Beads ID: cc2-2h8** | **Depends on: Phase 3**

Add pointer operations and arrays:
```c
int sum_array(int* arr, int len) {
    int total = 0;
    for (int i = 0; i < len; i++) {
        total = total + arr[i];
    }
    return total;
}

int main() {
    int numbers[5] = {1, 2, 3, 4, 5};
    return sum_array(numbers, 5);
}
```

**Components:**
- **Lexer**: Operators `&`, `*` (unary), `[`, `]`
- **Parser**: Pointer types, array declarations, array indexing
- **Type System**: Pointer types, array types, type compatibility
- **AST**: AddressOf, Dereference, ArrayIndex, ArrayInit nodes
- **Codegen**: Pointer arithmetic, array layout in memory

**Success Criteria:**
- Can take address of variables (&)
- Can dereference pointers (*)
- Can declare and use arrays
- Array indexing works correctly
- Can pass arrays to functions

---

### Phase 5: Structs and Complex Types
**Priority: P2** | **Beads ID: cc2-ki5** | **Depends on: Phase 4**

Add struct definitions and member access:
```c
struct Point {
    int x;
    int y;
};

int main() {
    struct Point p = {10, 20};
    struct Point* ptr = &p;
    return ptr->x + ptr->y;
}
```

**Components:**
- **Lexer**: Keywords `struct`, operators `.`, `->`
- **Parser**: Struct definitions, struct literals, member access
- **Type System**: Struct types, member offset calculation
- **AST**: StructDef, StructLiteral, MemberAccess nodes
- **Codegen**: Struct layout, member offset calculation, sizeof

**Success Criteria:**
- Can define structs
- Can declare struct variables
- Can access members with `.`
- Can access members through pointers with `->`
- sizeof works for structs

---

### Phase 6: Preprocessor and Multi-file Compilation
**Priority: P3** | **Beads ID: cc2-cph** | **Depends on: Phase 5**

Add preprocessor and ability to compile multiple files:
```c
// math.h
#ifndef MATH_H
#define MATH_H

int add(int a, int b);

#endif

// math.c
#include "math.h"

int add(int a, int b) {
    return a + b;
}

// main.c
#include "math.h"

int main() {
    return add(5, 3);
}
```

**Components:**
- **Preprocessor**: #include, #define, #ifdef/#ifndef/#endif, #if/#else
- **Linker**: Combine multiple object files
- **Build System**: Compile multiple source files
- **Standard Library**: Basic subset (printf, malloc, etc.)

**Success Criteria:**
- Can process #include directives
- Can process #define macros
- Can compile multiple .c files into one executable
- Can link with minimal libc functions
- Can compile simple real-world programs

---

## Development Workflow

For each phase:

1. **Plan**: Review phase requirements, identify unknowns
2. **Test First**: Write failing integration tests for the phase
3. **Implement**: Build components incrementally
4. **Test**: Ensure tests pass, add more tests
5. **Document**: Update docs with what was learned
6. **Refactor**: Clean up before moving to next phase

## Testing Strategy

Each phase should have:

- **Unit tests**: For lexer tokens, parser nodes, etc.
- **Integration tests**: Full .c files that compile and run
- **Regression tests**: Ensure previous phases still work

Example test structure:
```
tests/
  phase1/
    return_literal.c
    arithmetic.c
  phase2/
    variables.c
    function_calls.c
  phase3/
    if_else.c
    loops.c
```

## References and Resources

- [Crafting Interpreters](https://craftinginterpreters.com/) - Parser and interpreter design
- [C11 Standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) - Language specification
- [System V ABI](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf) - Calling convention
- [chibicc](https://github.com/rui314/chibicc) - Excellent reference C compiler
- [Writing a C Compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html) - Tutorial series

## Notes

- Start simple, add complexity gradually
- Each phase should produce a working compiler
- Test-driven development helps catch bugs early
- Don't optimize prematurely - correctness first
- Document decisions and learnings as we go
