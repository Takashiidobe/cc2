// Test: Character literals with escape sequences

int main() {
    char newline = '\n';
    char tab = '\t';
    char backslash = '\\';

    // newline = 10, tab = 9, backslash = 92
    // But exit codes are 8-bit, so we'll use smaller values
    return newline + tab;  // 10 + 9 = 19
}
