// Test variadic macros with __VA_ARGS__
#define LOG(fmt, ...) printf(fmt, __VA_ARGS__)
#define SUM1(a, ...) (a)
#define SUM2(a, b, ...) (a + b)
#define SUM3(a, b, c, ...) (a + b + c)

int printf(char *fmt, ...);

int main() {
    // Variadic macro with multiple arguments
    LOG("%d %d %d\n", 10, 20, 30);  // Should print "10 20 30"

    // Variadic macro with one variadic arg
    LOG("%d\n", 42);  // Should print "42"

    // Using variadic args selectively
    int x = SUM1(5, 10, 15);
    printf("%d\n", x);  // Should print 5 (only uses first arg)

    int y = SUM2(5, 10, 15);
    printf("%d\n", y);  // Should print 15 (uses first two args)

    int z = SUM3(5, 10, 15, 20, 25);
    printf("%d\n", z);  // Should print 30 (uses first three args)

    return 0;
}
