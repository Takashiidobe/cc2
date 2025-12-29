// Test printf family functions from glibc
int printf(char *fmt, ...);
int sprintf(char *buf, char *fmt, ...);

int main() {
    // Test basic printf
    printf("Hello, World!\n");

    // Test printf with integers
    printf("Number: %d\n", 42);
    printf("Negative: %d\n", -10);

    // Test printf with multiple arguments
    printf("Sum: %d + %d = %d\n", 10, 20, 30);

    // Test sprintf
    char buffer[100];
    sprintf(buffer, "Value: %d", 123);
    printf("sprintf result: %s\n", buffer);

    // Return success
    return 0;
}
