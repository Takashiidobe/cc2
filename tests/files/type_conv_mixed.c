// Test: mixed type arithmetic

int main() {
    char c = 10;
    short s = 20;
    int i = 30;

    // All promote to int
    return c + s + i;  // Should be 60
}
