// Test: Uninitialized global variable

int x;

int main() {
    x = 42;
    return x;  // Should return 42
}
