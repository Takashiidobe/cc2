// Test: Arithmetic with character literals

int main() {
    char a = 'a';
    char b = 'b';
    char c = 'c';

    // 'a' = 97, 'b' = 98, 'c' = 99
    // Sum would be 294, but exit codes are 8-bit
    // 294 % 256 = 38
    return a + b + c;
}
