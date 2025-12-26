// Test: Null character literal

int main() {
    char null_char = '\0';
    char a = 'A';

    // Should return 65 since null_char is 0
    return null_char + a;
}
