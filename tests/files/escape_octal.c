// Test: Octal escape sequences

int main() {
    char a = '\101';  // 'A' in octal (64 + 1 = 65)
    char b = '\102';  // 'B' in octal (64 + 2 = 66)

    // Octal 101 = 65, octal 102 = 66
    return a + b;  // 65 + 66 = 131
}
