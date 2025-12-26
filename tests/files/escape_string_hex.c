// Test: Hex escape in string literals

int main() {
    char *str = "\x48\x65\x6C\x6C\x6F";  // "Hello" in hex

    // H=72, e=101, l=108, l=108, o=111
    // Sum would be 500, but we'll just return first char
    return str[0];  // Should be 'H' = 72
}
