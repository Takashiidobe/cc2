// Test assert.h standard header

#include <assert.h>

// Provide a stub implementation of __assert_fail for testing
void __assert_fail(char *assertion, char *file, int line) {
    // In a real implementation, this would print an error and abort
    // For testing, we'll just return
    return;
}

int main() {
    // Test that assert expands correctly
    // With NDEBUG not defined, these should expand to function calls

    // This assertion should pass (not call __assert_fail)
    assert(1 == 1);
    assert(2 + 2 == 4);

    // Test with a true condition
    int x = 5;
    assert(x == 5);

    return 0;
}
