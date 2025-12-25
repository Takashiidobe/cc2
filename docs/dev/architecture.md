# cc2 Architecture

This document provides a deep dive into the architecture and implementation details of the cc2 C compiler.

## Table of Contents

- [Overview](#overview)
- [Compilation Pipeline](#compilation-pipeline)
- [Lexer](#lexer)
- [Parser](#parser)
- [Symbol Table](#symbol-table)
- [Code Generator](#code-generator)
- [Type System](#type-system)
- [Memory Layout](#memory-layout)

## Overview

cc2 is a C compiler written in Rust that compiles C source code to x86-64 assembly. It follows a traditional multi-phase compiler design:

```
.c source → Lexer → Parser → Symbol Table → Codegen → .S assembly
```

The compiler is designed for:
- **Learning**: Clear, readable code with extensive comments
- **Correctness**: Comprehensive test suite comparing with GCC/Clang
- **Incrementality**: Each phase builds on the previous, always keeping the compiler working

## Compilation Pipeline

### Phase 1: Lexical Analysis (Lexer)

**File**: `src/lexer.rs`

The lexer scans the source code and produces a sequence of tokens.

**Token Types:**
- Keywords: `int`, `return`, `if`, `else`, `while`, `for`, `struct`, `union`, `enum`, etc.
- Identifiers: Variable and function names
- Literals: Integer literals (decimal only currently)
- Operators: `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `~`, `<<`, `>>`, etc.
- Punctuation: `(`, `)`, `{`, `}`, `[`, `]`, `;`, `,`, `.`, `->`

**Implementation Details:**
- Uses `chars().peekable()` for lookahead
- Single-pass character scanning
- Keyword recognition via hashmap lookup
- Tracks line and column for error messages (future)

**Example:**
```c
int main() { return 42; }
```
→ `[Int, Identifier("main"), OpenParen, CloseParen, OpenBrace, Return, IntLiteral(42), Semicolon, CloseBrace, Eof]`

### Phase 2: Syntax Analysis (Parser)

**File**: `src/parser.rs`

The parser consumes tokens and builds an Abstract Syntax Tree (AST) using recursive descent parsing.

**Parsing Strategy:**
- **Recursive Descent**: Each grammar rule is a method
- **Operator Precedence**: Handled via precedence climbing
  - Multiplication/Division/Modulo (highest)
  - Addition/Subtraction
  - Shift operators
  - Comparison operators
  - Equality operators
  - Bitwise AND
  - Bitwise XOR
  - Bitwise OR
  - Logical AND
  - Logical OR
  - Assignment (lowest)

**Key Parser Methods:**
- `parse_function()`: Function definitions
- `parse_statement()`: Statements (if, while, for, return, var decl, expr)
- `parse_expression()`: Entry point for expressions → parse_assignment
- `parse_assignment()`: Assignment operators
- `parse_logical_or()` → ... → `parse_multiplicative()`: Precedence levels
- `parse_unary()`: Unary operators (`!`, `-`, `~`, `*`, `&`, `sizeof`)
- `parse_primary()`: Literals, identifiers, function calls, member access, array indexing

**AST Structure** (`src/ast.rs`):
```rust
pub enum AstNode {
    Program(Vec<AstNode>),
    Function { name, return_type, params, body },
    VarDecl { name, var_type, init },
    Assignment { target, value },  // target can be any lvalue
    IfStatement { condition, then_branch, else_branch },
    WhileLoop { condition, body },
    ForLoop { init, condition, increment, body },
    StructDef { name, fields },
    UnionDef { name, fields },
    EnumDef { name, enumerators },
    BinaryOp { op, left, right },
    UnaryOp { op, operand },
    FunctionCall { name, args },
    Variable(String),
    MemberAccess { base, member, through_pointer },
    ArrayIndex { array, index },
    AddressOf(Box<AstNode>),
    Dereference(Box<AstNode>),
    IntLiteral(i64),
    // ... more variants
}
```

### Phase 3: Symbol Table

**File**: `src/symbol_table.rs`

Tracks variables and their stack locations during compilation.

**Symbol Information:**
- Variable name
- Type (from AST)
- Stack offset (negative from `%rbp`)

**Stack Frame Layout:**
```
Higher addresses
┌──────────────┐
│ Return addr  │ ← Pushed by call
├──────────────┤
│ Old %rbp     │ ← %rbp points here
├──────────────┤
│ Local var 1  │ -4(%rbp)   (int)
├──────────────┤
│ Local var 2  │ -8(%rbp)   (int)
├──────────────┤
│ Padding      │ (for 16-byte alignment)
└──────────────┘ ← %rsp
Lower addresses
```

**Alignment Rules:**
- Variables aligned to their size (1, 2, 4, or 8 bytes)
- Stack frame aligned to 16 bytes (System V ABI requirement)
- Structs/unions have alignment based on largest member

### Phase 4: Code Generation

**File**: `src/codegen.rs`

Generates x86-64 assembly following the System V ABI.

**Register Usage:**
- `%rax`: Return value, expression evaluation, accumulator
- `%rcx`: Temporary storage, second operand for binary ops
- `%rdi`, `%rsi`, `%rdx`, `%rcx`, `%r8`, `%r9`: First 6 integer arguments
- `%rbp`: Frame pointer
- `%rsp`: Stack pointer

**Calling Convention (System V ABI):**
- Parameters 1-6 passed in registers
- Additional parameters on stack (not implemented yet)
- Return value in `%rax` (or `%eax` for 32-bit)
- Caller saves `%rax`, `%rcx`, `%rdx`, `%rdi`, `%rsi`, `%r8`-`%r11`
- Callee saves `%rbx`, `%rbp`, `%r12`-`%r15`
- Stack must be 16-byte aligned before `call`

**Function Prologue:**
```asm
main:
    pushq %rbp           # Save old frame pointer
    movq %rsp, %rbp      # Set up new frame pointer
    subq $16, %rsp       # Allocate stack space (aligned to 16)
```

**Function Epilogue:**
```asm
.L0:                     # Function end label
    movq %rbp, %rsp      # Restore stack pointer
    popq %rbp            # Restore frame pointer
    ret                  # Return to caller
```

**Expression Evaluation:**

Uses a stack-based approach with `%rax` as the accumulator:

```c
a + b * c
```

Generates:
```asm
# Evaluate a
movslq -4(%rbp), %rax    # a → %rax
pushq %rax               # Save a

# Evaluate b * c
movslq -8(%rbp), %rax    # b → %rax
pushq %rax               # Save b
movslq -12(%rbp), %rax   # c → %rax
popq %rcx                # b → %rcx
imull %ecx, %eax         # b * c → %eax

# Add
popq %rcx                # a → %rcx
addl %ecx, %eax          # a + (b * c) → %eax
```

**Type-Aware Code Generation:**

Different instructions based on type:
- `movb` / `movsbq` / `movzbq`: char (8-bit)
- `movw` / `movswq` / `movzwq`: short (16-bit)
- `movl` / `cltq`: int (32-bit)
- `movq`: long, pointers (64-bit)

Sign extension vs zero extension:
- Signed: `movsbq`, `movswq`, `cltq` (sign-extend)
- Unsigned: `movzbq`, `movzwq`, `movl %eax, %eax` (zero-extend)

## Type System

**File**: `src/ast.rs`

```rust
pub enum Type {
    Int,         // 4 bytes, signed
    UInt,        // 4 bytes, unsigned
    Char,        // 1 byte, signed
    UChar,       // 1 byte, unsigned
    Short,       // 2 bytes, signed
    UShort,      // 2 bytes, unsigned
    Long,        // 8 bytes, signed
    ULong,       // 8 bytes, unsigned
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Struct(String),
    Union(String),
    Enum(String),  // Treated as int
}
```

**Type Sizes:**
- Char: 1 byte
- Short: 2 bytes
- Int: 4 bytes
- Long: 8 bytes
- Pointer: 8 bytes
- Array: element_size * length
- Struct: sum of members + padding
- Union: max of all members
- Enum: 4 bytes (int)

## Memory Layout

### Struct Layout

**Example:**
```c
struct Point {
    char c;      // 1 byte
    // 3 bytes padding
    int x;       // 4 bytes
    short s;     // 2 bytes
    // 2 bytes padding
    long y;      // 8 bytes
};  // Total: 24 bytes (aligned to 8)
```

**Layout Algorithm:**
1. For each field in order:
   - Align offset to field's alignment
   - Place field at current offset
   - Advance offset by field size
2. Align final size to struct's alignment (max of all field alignments)

**In Memory:**
```
Offset  Field
0       c (1 byte)
1-3     padding
4-7     x (4 bytes)
8-9     s (2 bytes)
10-15   padding
16-23   y (8 bytes)
```

### Union Layout

**Example:**
```c
union Data {
    int i;       // 4 bytes
    char c;      // 1 byte
    long l;      // 8 bytes
};  // Total: 8 bytes (max size, aligned to 8)
```

**Layout Algorithm:**
1. All fields at offset 0
2. Size = max of all field sizes
3. Alignment = max of all field alignments
4. Final size aligned to alignment

**In Memory:**
```
Offset  All Fields
0-7     i overlaps c overlaps l
```

### Enum Constants

Enums are compile-time constants:

```c
enum Color { RED, GREEN, BLUE };
int x = RED;
```

Compiled as:
```asm
movq $0, %rax        # RED = 0
movl %eax, -4(%rbp)  # x = 0
```

No runtime representation - enum constants are replaced with their integer values during code generation.

## Stack Frame Details

**Example Function:**
```c
int add(int a, int b) {
    int sum = a + b;
    return sum;
}
```

**Stack Frame:**
```
┌──────────────┐ ← Caller's frame
│ Return addr  │
├──────────────┤
│ Old %rbp     │ ← %rbp
├──────────────┤
│ a            │ -4(%rbp)  (param copied from %edi)
├──────────────┤
│ b            │ -8(%rbp)  (param copied from %esi)
├──────────────┤
│ sum          │ -12(%rbp) (local var)
├──────────────┤
│ padding      │ -16(%rbp) (16-byte alignment)
└──────────────┘ ← %rsp
```

**Generated Code:**
```asm
add:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp            # Allocate aligned frame
    movl %edi, -4(%rbp)       # Copy param a
    movl %esi, -8(%rbp)       # Copy param b
    movslq -4(%rbp), %rax     # Load a
    pushq %rax
    movslq -8(%rbp), %rax     # Load b
    popq %rcx
    addl %ecx, %eax           # a + b
    movl %eax, -12(%rbp)      # Store to sum
    movslq -12(%rbp), %rax    # Load sum for return
    jmp .L0
.L0:
    movq %rbp, %rsp
    popq %rbp
    ret
```

## Error Handling

Currently minimal error handling:
- Parser returns `Result<AstNode, String>`
- Errors are string messages
- No error recovery (stops on first error)

Future improvements:
- Better error messages with line/column numbers
- Error recovery to report multiple errors
- Warning system

## Testing Strategy

**Unit Tests** (`src/*_test.rs`):
- Test individual components
- Fast, focused tests
- No external dependencies

**Integration Tests** (`tests/*.rs`):
- Full compilation pipeline
- Compare with GCC/Clang output
- Snapshot testing for AST and assembly

**Snapshot Testing** (using `insta`):
- Parser output snapshots
- Assembly output snapshots
- Easy to review changes with `cargo insta review`

## Future Enhancements

**Planned:**
- Preprocessor support
- Multi-file compilation
- Optimization passes
- Better error messages
- More target architectures

**Not Planned:**
- LLVM integration (learning project)
- Advanced optimizations
- C++ support
