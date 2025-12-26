// Test: String literals with escape sequences

int main() {
    char *s1 = "hello\nworld";  // Contains newline
    char *s2 = "tab\there";     // Contains tab

    // s1[5] should be '\n' = 10
    // s2[3] should be '\t' = 9
    // Return 10 + 9 = 19
    return s1[5] + s2[3];
}
