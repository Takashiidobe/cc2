// Test comma operator
int main() {
    int a = 1;
    int b = 2;
    int c;

    // Basic comma operator - result is rightmost expression
    c = (a++, b++, a + b);

    // Comma with assignment
    int x;
    int y;
    x = (y = 5, y * 2);

    // Multiple comma operators
    int result = (10, 20, 30);

    return 0;
}
