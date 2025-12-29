// Test stdlib.h standard library functions
#include <stdlib.h>

int main() {
    int result = 0;

    // Test atoi
    int n = atoi("123");
    if (n == 123) {
        result = result + 1;
    }

    // Test atol
    long ln = atol("456");
    if (ln == 456) {
        result = result + 1;
    }

    // Test abs
    int a = abs(-42);
    if (a == 42) {
        result = result + 1;
    }

    // Test labs
    long la = labs(-100);
    if (la == 100) {
        result = result + 1;
    }

    // Test EXIT_SUCCESS and EXIT_FAILURE macros
    if (EXIT_SUCCESS == 0) {
        result = result + 1;
    }
    if (EXIT_FAILURE == 1) {
        result = result + 1;
    }

    // Should return 6 if all tests pass
    return result;
}
