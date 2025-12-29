// Test line continuation with backslash

// Multi-line macro definition
#define LONG_MACRO \
    10 + \
    20 + \
    30

// Multi-line function-like macro
#define MAX(a, b) \
    ((a) > (b) ? \
     (a) : \
     (b))

// Multi-line preprocessor directive
#define MULTILINE_FUNC(x) \
    do { \
        x = x * 2; \
    } while (0)

int main() {
    int x = LONG_MACRO;  // Should be 10 + 20 + 30 = 60
    int y = MAX(5, 10);   // Should be 10

    // Test that continuation works in regular code too
    int z = 1 + \
            2 + \
            3;  // Should be 6

    return x + y + z;  // Should return 76
}
