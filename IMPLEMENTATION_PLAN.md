# C Compiler in Rust - Implementation Plan

This document outlines the incremental approach to building a C compiler in Rust. Each phase builds on the previous one, ensuring we always have a working (if limited) compiler at each step.

## Architecture Overview

The compiler follows a traditional multi-phase design:

1. **Lexer (Tokenizer)**: Converts source text into tokens
2. **Parser**: Builds an Abstract Syntax Tree (AST) from tokens using recursive descent
3. **Symbol Table**: Tracks variables, functions, types, and enum constants
4. **Code Generator**: Emits x86-64 assembly (targeting Linux System V ABI)

## Design Decisions

### Language: Rust
- Memory safety without garbage collection
- Strong type system helps prevent compiler bugs
- Excellent pattern matching for AST traversal
- Growing ecosystem for compiler tools

### Target Architecture: x86-64
- Linux System V ABI (32-bit int, 8-byte pointers)
- Direct assembly generation (no LLVM for learning purposes)
- Can add other backends later

### Parsing Strategy: Recursive Descent
- Simple to implement and understand
- Easy to extend incrementally
- Good error messages possible
- Sufficient for C's grammar

### Testing Strategy
- Test-driven development with .c input files
- Compare output with GCC/Clang using `datatest-stable` for correctness
- Integration tests that compile and run programs
- Snapshot testing with `insta` for parser and codegen output
- Unit tests for individual components

## Implementation Status

### ✅ Phase 0: Project Setup and Basic Infrastructure
**Priority: P1** | **Beads ID: cc2-786** | **Status: COMPLETE**

Set up the Rust project structure and basic tooling.

**Deliverables:**
- ✅ Cargo project with proper structure (src/lexer, src/parser, src/codegen)
- ✅ CLI interface for the compiler
- ✅ Test infrastructure with datatest-stable and insta
- ✅ Documentation setup
- ✅ Integration with beads for issue tracking

**Success Criteria:**
- ✅ `cargo build` succeeds
- ✅ Can invoke `cc2 input.c -o output.S`
- ✅ Tests run with `cargo test`

---

### ✅ Phase 1: Minimal Integer Calculator
**Priority: P1** | **Beads ID: cc2-0oj** | **Status: COMPLETE**

Build the minimum viable compiler that can compile integer arithmetic.

**Implemented Features:**
- ✅ Compile `int main() { return 42; }`
- ✅ Binary operators: `+`, `-`, `*`, `/`, `%`
- ✅ Operator precedence (3 + 4 * 5 = 23)
- ✅ Parenthesized expressions
- ✅ Integer literals

**Components:**
- **Lexer**: Tokens for `int`, `main`, `(`, `)`, `{`, `}`, `return`, integers, `;`, arithmetic operators
- **Parser**: Function definitions, return statements, expression parsing with precedence
- **AST**: Function, ReturnStmt, IntLiteral, BinaryOp nodes
- **Codegen**: x86-64 assembly with proper prologue/epilogue, register allocation

**Test Files:**
- `return_literal.c`
- `arithmetic.c`
- `add_5_2.c`

---

### ✅ Phase 2: Variables and Function Calls
**Priority: P2** | **Beads ID: cc2-7ep** | **Status: COMPLETE**

Add support for local variables and function calls.

**Implemented Features:**
- ✅ Local variable declarations
- ✅ Variable assignment
- ✅ Multiple functions
- ✅ Function parameters (up to 6 via registers)
- ✅ Function calls with arguments
- ✅ System V ABI calling convention

**Components:**
- **Lexer**: Identifiers
- **Parser**: Variable declarations, function parameters, function calls
- **AST**: VarDecl, Assignment, Variable, FunctionCall nodes
- **Symbol Table**: Track variables and their stack offsets
- **Codegen**: Stack frame management, parameter passing via registers

**Test Files:**
- `variables.c`
- `function_call.c`

---

### ✅ Phase 3: Control Flow
**Priority: P2** | **Beads ID: cc2-oq5** | **Status: COMPLETE**

Add conditional statements and loops.

**Implemented Features:**
- ✅ If statements
- ✅ If-else statements
- ✅ While loops
- ✅ For loops (with variable initialization)
- ✅ Comparison operators: `<`, `>`, `<=`, `>=`, `==`, `!=`
- ✅ Logical operators: `&&`, `||`, `!`

**Components:**
- **Lexer**: Keywords `if`, `else`, `while`, `for`, comparison and logical operators
- **Parser**: If statements, while loops, for loops
- **AST**: IfStmt, WhileStmt, ForStmt, ComparisonOp, LogicalOp nodes
- **Codegen**: Labels and jumps, condition code handling

**Test Files:**
- `if_else.c`
- `while_loop.c`
- `for_loop.c`
- `comparisons.c`
- `compare_logic.c`
- `logical_ops.c`
- `control_mix.c`

---

### ✅ Phase 4: Pointers and Arrays
**Priority: P2** | **Beads ID: cc2-2h8** | **Status: COMPLETE**

Add pointer operations and arrays.

**Implemented Features:**
- ✅ Pointer types
- ✅ Address-of operator (`&`)
- ✅ Dereference operator (`*`)
- ✅ Array declarations
- ✅ Array initialization with `{...}`
- ✅ Array indexing `arr[i]`
- ✅ Pointer arithmetic
- ✅ `sizeof` operator (for types and expressions)

**Components:**
- **Lexer**: Operators `&`, `*` (unary), `[`, `]`, `sizeof`
- **Parser**: Pointer types, array declarations, array indexing, sizeof
- **Type System**: Pointer types, array types, type compatibility
- **AST**: AddressOf, Dereference, ArrayIndex, ArrayInit, SizeOfType, SizeOfExpr nodes
- **Codegen**: Pointer arithmetic, array layout in memory, element size calculation

**Test Files:**
- `pointers_arrays.c`
- `array_sum.c`
- `pointer_math.c`
- `sizeof.c`

---

### ✅ Phase 5: Structs and Complex Types
**Priority: P2** | **Beads ID: cc2-ki5** | **Status: COMPLETE**

Add struct definitions and member access.

**Implemented Features:**
- ✅ Struct definitions
- ✅ Struct variables
- ✅ Struct initialization with `{...}`
- ✅ Member access with `.`
- ✅ Member access through pointers with `->`
- ✅ Nested structs
- ✅ Struct alignment and padding
- ✅ sizeof works for structs

**Components:**
- **Lexer**: Keywords `struct`, operators `.`, `->`
- **Parser**: Struct definitions, struct literals, member access
- **Type System**: Struct types, member offset calculation, alignment
- **AST**: StructDef, StructInit, MemberAccess nodes, StructField type
- **Codegen**: Struct layout with proper alignment, member offset calculation

**Test Files:**
- `structs.c`
- `struct_ops.c`

---

### ✅ Phase 6: Extended Type System
**Priority: P2** | **Status: COMPLETE**

Expand type system with unsigned types, multiple integer sizes, enums, and unions.

#### ✅ Phase 6.1: Unsigned Integers
**Beads ID: cc2-2h8.1** | **Status: COMPLETE**

**Implemented Features:**
- ✅ `unsigned int`, `unsigned char`, `unsigned short`, `unsigned long`
- ✅ Unsigned arithmetic
- ✅ Unsigned comparisons
- ✅ Type-aware code generation

**Test Files:**
- `unsigned_ops.c`
- `unsigned_compare.c`
- `unsigned_ptr_index.c`
- `unsigned_sizes.c`

#### ✅ Phase 6.2: Signed Integer Types
**Beads ID: cc2-2h8.2** | **Status: COMPLETE**

**Implemented Features:**
- ✅ `short` (2 bytes)
- ✅ `long` (8 bytes)
- ✅ Sign extension for signed types
- ✅ Zero extension for unsigned types

**Test Files:**
- `int_types.c`

#### ✅ Phase 6.3: Bitwise and Shift Operators
**Beads ID: cc2-2h8.3** | **Status: COMPLETE**

**Implemented Features:**
- ✅ Bitwise AND (`&`), OR (`|`), XOR (`^`), NOT (`~`)
- ✅ Left shift (`<<`), right shift (`>>`)
- ✅ Proper operator precedence

**Test Files:**
- `bitwise_ops.c`
- `shift_mod.c`

#### ✅ Phase 6.4: Compound Assignment Operators
**Beads ID: cc2-2h8.4** | **Status: COMPLETE**

**Implemented Features:**
- ✅ `+=`, `-=`, `*=`, `/=`, `%=`
- ✅ `&=`, `|=`, `^=`
- ✅ `<<=`, `>>=`
- ✅ Assignment expressions (value is the assigned value)
- ✅ Type truncation on assignment

**Test Files:**
- `compound_assign.c`
- `assignment_truncation.c`

#### ✅ Phase 6.5: Enums
**Beads ID: cc2-5nz** | **Status: COMPLETE**

**Implemented Features:**
- ✅ Enum declarations
- ✅ Enumerators with auto-incrementing values
- ✅ Explicit enumerator values
- ✅ Enum constants replaced with integers at compile time
- ✅ Enum types (4-byte signed int)

**Test Files:**
- `enum.c`

#### ✅ Phase 6.6: Unions
**Beads ID: cc2-7ho** | **Status: COMPLETE**

**Implemented Features:**
- ✅ Union declarations
- ✅ Union member access
- ✅ All members at offset 0
- ✅ Size is maximum of all member sizes
- ✅ Proper alignment

**Bonus Feature:**
- ✅ Generalized lvalue assignments (supports member access, array indexing, dereferences)

**Test Files:**
- `union.c`

---

### 📋 Phase 7: Documentation Update
**Priority: P2** | **Beads ID: cc2-zco** | **Status: IN PROGRESS**

Update all project documentation to reflect current implementation.

**Tasks:**
- ✅ Update IMPLEMENTATION_PLAN.md with completed phases
- 🔄 Update README.md with current features
- 🔄 Create docs/dev/architecture.md
- 🔄 Create docs/dev/features.md
- 🔄 Update docs/dev/tests.md

---

### 📋 Phase 8: Preprocessor (Future)
**Priority: P3** | **Status: PLANNED**

Add preprocessor and ability to compile multiple files.

**Planned Features:**
- `#include` directives
- `#define` macros
- `#ifdef`/`#ifndef`/`#endif`
- `#if`/`#else`
- Multi-file compilation
- Linker integration

---

### 📋 Phase 9: Standard Library (Future)
**Priority: P3** | **Status: PLANNED**

Add support for standard library functions.

**Planned Features:**
- `printf`, `scanf`
- `malloc`, `free`
- String functions (`strlen`, `strcpy`, etc.)
- File I/O

---

## Development Workflow

For each phase:

1. **Plan**: Review phase requirements, create beads issues
2. **Test First**: Write failing integration tests for the phase
3. **Implement**: Build components incrementally
4. **Test**: Ensure tests pass, add more tests
5. **Document**: Update docs with what was learned
6. **Refactor**: Clean up before moving to next phase

## Testing Infrastructure

The project uses multiple testing approaches:

### Integration Tests (`tests/codegen.rs`, `tests/parse.rs`, `tests/tokenize.rs`)
- Test full compilation pipeline
- Compare output with GCC/Clang
- Use `datatest-stable` to run all `.c` files in `tests/files/`
- Snapshot testing with `insta` for AST and assembly output

### Unit Tests (`src/*_test.rs`)
- Test individual components (lexer, parser, etc.)
- Located in separate `*_test.rs` files (not inline)
- Fast and focused

### Test Files (`tests/files/*.c`)
- Real C programs that should compile and run
- Organized by feature/phase
- Each test file is automatically run by integration tests

## Current Capabilities

The cc2 compiler can currently compile C programs with:

- ✅ Integer arithmetic and expressions
- ✅ Variables and assignments (including lvalue assignments)
- ✅ Functions with parameters and return values
- ✅ Control flow (if/else, while, for)
- ✅ Pointers and pointer arithmetic
- ✅ Arrays and array indexing
- ✅ Structs with member access
- ✅ Unions with member access
- ✅ Enums
- ✅ Multiple integer types (char, short, int, long, signed/unsigned variants)
- ✅ Bitwise and shift operators
- ✅ Logical operators
- ✅ Compound assignment operators
- ✅ sizeof operator

## References and Resources

- [Crafting Interpreters](https://craftinginterpreters.com/) - Parser and interpreter design
- [C11 Standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) - Language specification
- [System V ABI](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf) - Calling convention
- [chibicc](https://github.com/rui314/chibicc) - Excellent reference C compiler
- [Writing a C Compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html) - Tutorial series

## Notes

- Start simple, add complexity gradually
- Each phase produces a working compiler
- Test-driven development catches bugs early
- Don't optimize prematurely - correctness first
- Document decisions and learnings as we go
