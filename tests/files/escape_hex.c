// Test: Hexadecimal escape sequences

int main() {
    char a = '\x41';  // 'A' in hex
    char b = '\x42';  // 'B' in hex

    // 0x41 = 65, 0x42 = 66
    return a + b;  // 65 + 66 = 131
}
