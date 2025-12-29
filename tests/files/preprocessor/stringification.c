// Test stringification operator (#)
#define STR(x) #x
#define XSTR(x) STR(x)  // Double expansion

int printf(char *fmt, ...);

int main() {
    // Basic stringification
    char *s1 = STR(hello);
    printf("%s\n", s1);  // Should print "hello"

    // Stringification with spaces (should be normalized)
    char *s2 = STR(hello   world);
    printf("%s\n", s2);  // Should print "hello world"

    // Stringification of numbers
    char *s3 = STR(123);
    printf("%s\n", s3);  // Should print "123"

    // Stringification of expressions
    char *s4 = STR(1 + 2);
    printf("%s\n", s4);  // Should print "1 + 2"

    return 0;
}
