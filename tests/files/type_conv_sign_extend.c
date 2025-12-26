// Test: Proper sign extension of char

char g = 128;  // 128 as signed char = -128

int main() {
    // If properly sign-extended: -128 < 0 = true = 1
    // If zero-extended: 128 < 0 = false = 0
    return g < 0;
}
