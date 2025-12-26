// Test: global char variable promotion

char g = 250;  // Will be treated as -6 when sign-extended from byte

int main() {
    int i = 10;
    return g + i;  // Should be 4 if sign-extended (-6 + 10 = 4)
}
