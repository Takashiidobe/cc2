// Test variadic function with va_* builtins
int sum_ints(int count, ...) {
    va_list args;
    va_start(args, count);

    int sum = 0;
    int i = 0;
    while (i < count) {
        int val = va_arg(args, int);
        sum = sum + val;
        i = i + 1;
    }

    va_end(args);
    return sum;
}

int main() {
    // Test: sum_ints(3, 10, 20, 30) should return 60
    int result = sum_ints(3, 10, 20, 30);
    return result; // Should return 60
}
