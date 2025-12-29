// Test register storage class
int main() {
    register int count = 10;
    register int result = 0;

    // register is a hint to put variable in a register
    // It doesn't change semantics, just performance
    for (register int i = 0; i < count; i = i + 1) {
        result = i;
    }

    return result + 1;  // Should be 10 (9 + 1)
}
