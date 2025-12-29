// Test command-line macro definitions via -D flag
// This test should be run with: -D FOO -D BAR=42

#ifdef FOO
int foo_defined = 1;
#else
int foo_defined = 0;
#endif

#ifndef BAR
int bar_value = 0;
#else
int bar_value = BAR;
#endif

int main() {
    return foo_defined + bar_value;  // Should return 43 (1 + 42)
}
