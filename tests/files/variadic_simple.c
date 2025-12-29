// Test variadic function with va_* builtins
int sum_ints(int count, ...) {
    va_list args;
    va_start(args, count);

    int sum = 0;
    // Note: va_arg currently returns 0 (stub implementation)
    // Full implementation would extract actual arguments
    int val = va_arg(args, int);
    sum = sum + val;

    va_end(args);
    return sum;
}

int main() {
    int result = sum_ints(2, 10, 20);
    return 0;
}
