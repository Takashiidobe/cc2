// Test: Mixed escape sequences

int main() {
    char hex = '\x0A';     // Newline in hex (10)
    char octal = '\012';   // Newline in octal (10)
    char basic = '\n';     // Newline basic (10)

    // All should be 10
    return hex + octal + basic;  // 10 + 10 + 10 = 30
}
