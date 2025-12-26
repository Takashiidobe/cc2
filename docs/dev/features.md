# Feature Documentation

This document provides detailed documentation for all implemented features in cc2.

## Table of Contents

- [Data Types](#data-types)
- [Operators](#operators)
- [Control Flow](#control-flow)
- [Functions](#functions)
- [Pointers and Arrays](#pointers-and-arrays)
- [Structs and Unions](#structs-and-unions)
- [Enums](#enums)

## Data Types

### Integer Types

cc2 supports all standard C integer types:

 | Type             | Size    | Range (Signed)    | Range (Unsigned) |
 |------------------|---------|-------------------|------------------|
 | `char`           | 1 byte  | -128 to 127       | -                |
 | `unsigned char`  | 1 byte  | -                 | 0 to 255         |
 | `short`          | 2 bytes | -32,768 to 32,767 | -                |
 | `unsigned short` | 2 bytes | -                 | 0 to 65,535      |
 | `int`            | 4 bytes | -2³¹ to 2³¹-1     | -                |
 | `unsigned int`   | 4 bytes | -                 | 0 to 2³²-1       |
 | `long`           | 8 bytes | -2⁶³ to 2⁶³-1     | -                |
 | `unsigned long`  | 8 bytes | -                 | 0 to 2⁶⁴-1       |

**Examples:**
```c
char c = -1;
unsigned char uc = 255;
short s = -32000;
unsigned short us = 60000;
int i = -100000;
unsigned int ui = 4000000000;
long l = -9000000000;
unsigned long ul = 18000000000000000000;
```

### Pointers

Pointers store memory addresses (8 bytes on x86-64).

**Examples:**
```c
int x = 42;
int *ptr = &x;      // Pointer to int
int **ptr2 = &ptr;  // Pointer to pointer to int

// Pointer arithmetic
int arr[5];
int *p = arr;
p++;                // Points to arr[1]
p += 2;             // Points to arr[3]
```

### Arrays

Arrays are contiguous sequences of elements.

**Examples:**
```c
int arr[5];                     // Uninitialized array
int arr2[3] = {1, 2, 3};       // Initialized array
int arr3[5] = {1, 2};          // Partial init (rest are 0)

// Multidimensional arrays
int matrix[3][4];               // 3x4 matrix
```

### Structs

Structs group related data with proper alignment and padding.

**Examples:**
```c
struct Point {
    int x;
    int y;
};

struct Point p = {10, 20};
p.x = 30;

struct Point *ptr = &p;
ptr->y = 40;
```

### Unions

Unions allow multiple fields to share the same memory location.

**Examples:**
```c
union Data {
    int i;
    char c;
    long l;
};

union Data d;
d.i = 42;       // Write as int
char x = d.c;   // Read as char (shares memory!)
```

### Enums

Enums define named integer constants.

**Examples:**
```c
enum Color { RED, GREEN, BLUE };        // RED=0, GREEN=1, BLUE=2
enum Status { OK=0, ERROR=1, PENDING=5, DONE }; // DONE=6

enum Color c = GREEN;
int x = RED;    // Enums are just ints
```

## Operators

### Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Modulo | `a % b` |
| `-` (unary) | Negation | `-a` |

**Examples:**
```c
int a = 10, b = 3;
int sum = a + b;      // 13
int diff = a - b;     // 7
int prod = a * b;     // 30
int quot = a / b;     // 3 (integer division)
int rem = a % b;      // 1
int neg = -a;         // -10
```

### Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `<` | Less than | `a < b` |
| `>` | Greater than | `a > b` |
| `<=` | Less than or equal | `a <= b` |
| `>=` | Greater than or equal | `a >= b` |
| `==` | Equal to | `a == b` |
| `!=` | Not equal to | `a != b` |

**Examples:**
```c
if (a < b) { ... }
if (a >= 0) { ... }
if (x == 42) { ... }
```

### Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Logical AND | `a && b` |
| `||` | Logical OR | `a || b` |
| `!` | Logical NOT | `!a` |

**Examples:**
```c
if (a > 0 && a < 10) { ... }
if (x == 0 || y == 0) { ... }
if (!done) { ... }
```

### Bitwise Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `a & b` |
| `|` | Bitwise OR | `a | b` |
| `^` | Bitwise XOR | `a ^ b` |
| `~` | Bitwise NOT | `~a` |
| `<<` | Left shift | `a << 2` |
| `>>` | Right shift | `a >> 2` |

**Examples:**
```c
int flags = 0;
flags |= 0x01;        // Set bit 0
flags &= ~0x02;       // Clear bit 1
int mask = ~0xFF;     // Invert bits
int shifted = x << 3; // Multiply by 8
```

### Assignment Operators

| Operator | Description | Equivalent |
|----------|-------------|------------|
| `=` | Assignment | - |
| `+=` | Add and assign | `a = a + b` |
| `-=` | Subtract and assign | `a = a - b` |
| `*=` | Multiply and assign | `a = a * b` |
| `/=` | Divide and assign | `a = a / b` |
| `%=` | Modulo and assign | `a = a % b` |
| `&=` | AND and assign | `a = a & b` |
| `|=` | OR and assign | `a = a | b` |
| `^=` | XOR and assign | `a = a ^ b` |
| `<<=` | Left shift and assign | `a = a << b` |
| `>>=` | Right shift and assign | `a = a >> b` |

**Examples:**
```c
int x = 10;
x += 5;     // x = 15
x *= 2;     // x = 30
x >>= 1;    // x = 15
```

**Lvalue Assignments:**

cc2 supports generalized lvalue assignments:

```c
// Simple variables
x = 5;

// Array elements
arr[i] = 10;

// Struct members
p.x = 20;
ptr->y = 30;

// Dereferences
*ptr = 40;

// Compound assignments work with all lvalues
arr[i] += 5;
p.x *= 2;
*ptr -= 10;
```

### Pointer Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Address-of | `&x` |
| `*` | Dereference | `*ptr` |
| `->` | Pointer member access | `ptr->field` |
| `.` | Direct member access | `s.field` |

**Examples:**
```c
int x = 42;
int *ptr = &x;      // Get address
int y = *ptr;       // Dereference

struct Point p;
p.x = 10;           // Direct access

struct Point *ptr = &p;
ptr->y = 20;        // Pointer access
```

### sizeof Operator

Returns the size of a type or expression in bytes.

**Examples:**
```c
sizeof(int)          // 4
sizeof(long)         // 8
sizeof(int *)        // 8
sizeof(arr)          // array_length * element_size

int x;
sizeof(x)            // 4
sizeof(x + 1)        // 4
```

## Control Flow

### If Statements

```c
if (condition) {
    // executed if condition is true
}

if (condition) {
    // executed if condition is true
} else {
    // executed if condition is false
}

// Nested if-else
if (x < 0) {
    result = -1;
} else if (x == 0) {
    result = 0;
} else {
    result = 1;
}
```

### While Loops

```c
while (condition) {
    // repeated while condition is true
}

// Example: sum 1 to 10
int sum = 0;
int i = 1;
while (i <= 10) {
    sum += i;
    i++;
}
```

### For Loops

```c
for (initialization; condition; increment) {
    // loop body
}

// Example: factorial
int result = 1;
for (int i = 1; i <= n; i++) {
    result *= i;
}
```

## Functions

### Function Definitions

```c
int add(int a, int b) {
    return a + b;
}

// No parameters
int get_constant() {
    return 42;
}

// Multiple statements
int max(int a, int b) {
    if (a > b) {
        return a;
    }
    return b;
}
```

### Function Calls

```c
int result = add(5, 3);
int x = get_constant();
int y = max(10, 20);

// Function calls in expressions
int z = add(1, 2) + add(3, 4);
```

### Parameter Passing

**Up to 6 integer parameters** passed via registers:
1. `%rdi` / `%edi`
2. `%rsi` / `%esi`
3. `%rdx` / `%edx`
4. `%rcx` / `%ecx`
5. `%r8` / `%r8d`
6. `%r9` / `%r9d`

Return values in `%rax` / `%eax`.

## Pointers and Arrays

### Pointer Arithmetic

```c
int arr[5] = {10, 20, 30, 40, 50};
int *p = arr;

p++;           // Points to arr[1]
p += 2;        // Points to arr[3]
p--;           // Points to arr[2]

int *end = arr + 5;  // One past end
```

### Array Indexing

```c
int arr[5] = {1, 2, 3, 4, 5};

arr[0] = 10;
int x = arr[2];

// Equivalent to pointer arithmetic
*(arr + 2) = 30;
```

### Pointer Comparison

```c
int *p1 = arr;
int *p2 = arr + 3;

if (p1 < p2) { ... }
if (p1 == arr) { ... }
```

## Structs and Unions

### Struct Alignment and Padding

Structs are aligned to the largest member alignment:

```c
struct Example {
    char c;      // 1 byte
    // 3 bytes padding
    int i;       // 4 bytes
    char c2;     // 1 byte
    // 3 bytes padding
};  // Total: 12 bytes
```

### Nested Structs

```c
struct Inner {
    int x;
    int y;
};

struct Outer {
    struct Inner inner;
    int z;
};

struct Outer o;
o.inner.x = 10;
o.z = 20;
```

### Union Memory Sharing

All union members start at offset 0:

```c
union Value {
    int i;
    long l;
    char c;
};

union Value v;
v.i = 0x12345678;
// v.c is now 0x78 (on little-endian)
```

## Limitations and Future Work

### Current Limitations

- **No string literals** (only integer constants)
- **No standard library** (can't call `printf`, `malloc`, etc. yet)
- **No floating point** (only integers)
- **No variadic functions** (`printf`-style)
- **No goto** or labels
- **No switch/case**
- **No typedef**
- **No const/volatile**
- **No static/extern**
- **Max 6 function parameters** (register limit)

### Planned Features

See [IMPLEMENTATION_PLAN.md](../../IMPLEMENTATION_PLAN.md) for the roadmap.
