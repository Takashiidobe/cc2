// Test string literal concatenation (C89 feature)
int main() {
    // Basic concatenation
    char *s1 = "hello" "world";

    // Concatenation with whitespace
    char *s2 = "foo"   "bar";

    // Concatenation across multiple lines
    char *s3 = "first"
               "second"
               "third";

    // Test that the strings are properly concatenated
    // "helloworld" should have length 10
    int len1 = 0;
    while (s1[len1] != 0) {
        len1 = len1 + 1;
    }

    // "foobar" should have length 6
    int len2 = 0;
    while (s2[len2] != 0) {
        len2 = len2 + 1;
    }

    // "firstsecondthird" should have length 16
    int len3 = 0;
    while (s3[len3] != 0) {
        len3 = len3 + 1;
    }

    // Return sum of lengths: 10 + 6 + 16 = 32
    return len1 + len2 + len3;
}
