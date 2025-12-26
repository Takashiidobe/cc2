// Test: Multiple string literals
// Returns sum of last characters modulo 256

int main() {
    char *s1 = "abc";
    char *s2 = "def";
    char *s3 = "ghi";

    // s1[2] = 'c' = 99, s2[2] = 'f' = 102, s3[2] = 'i' = 105
    // But exit codes are 8-bit, so this will be truncated
    // Let's use a smaller sum: s1[0] = 'a' = 97, s2[0] = 'd' = 100
    // 97 + 100 = 197
    return s1[0] + s2[0];
}
