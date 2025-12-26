// Test: short to int promotion

int main() {
    short s = -100;
    int i = 50;
    return s + i;  // Should be -50, modulo 256 = 206
}
