// Test stdio.h standard library functions
#include <stdio.h>

int main() {
    int result = 0;

    // Test putchar
    putchar('A');
    putchar('\n');
    result = result + 1;

    // Test puts
    puts("Hello");
    result = result + 1;

    // Test printf (simple)
    printf("Test\n");
    result = result + 1;

    // Test EOF macro
    if (EOF == -1) {
        result = result + 1;
    }

    // Test SEEK constants
    if (SEEK_SET == 0 && SEEK_CUR == 1 && SEEK_END == 2) {
        result = result + 1;
    }

    // Should return 5 if all tests pass
    return result;
}
