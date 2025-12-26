// Test: unsigned char to int promotion

int main() {
    unsigned char c = 255;
    int i = 10;
    return c + i;  // Should be 265, but will wrap to 9 (265 % 256)
}
