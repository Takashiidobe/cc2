// Test token pasting operator (##)
#define CONCAT(a, b) a##b
#define VAR(n) var##n

int printf(char *fmt, ...);

int main() {
    // Basic token pasting - concatenate identifiers
    int xy = 42;
    int result = CONCAT(x, y);
    printf("%d\n", result);  // Should print 42

    // Token pasting to form variable names
    int var1 = 10;
    int var2 = 20;
    int var3 = 30;

    int a = VAR(1);
    int b = VAR(2);
    int c = VAR(3);

    printf("%d\n", a + b + c);  // Should print 60

    // Token pasting with numbers
    #define NUM(x) 1##x
    int n = NUM(23);
    printf("%d\n", n);  // Should print 123

    return 0;
}
