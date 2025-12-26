// Test: Designated initializer for union

union Value {
    int i;
    char c;
};

int main() {
    union Value v = {.i = 42};
    return v.i;  // Should be 42
}
