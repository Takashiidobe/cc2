// Test function pointers (using indirect calls)
int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

int main() {
    // Test indirect call through dereference
    // In C, &add gives us the function address, and (*&add) dereferences it
    int result1 = (*&add)(3, 4);
    int result2 = (*&multiply)(5, 6);

    return 0;
}
