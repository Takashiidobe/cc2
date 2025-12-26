// Test: String literal pointer arithmetic

int main() {
    char *str = "world";
    char *ptr = str + 1;  // Points to "orld"

    // ptr[0] should be 'o' = 111
    return ptr[0];
}
