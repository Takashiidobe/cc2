// Test auto storage class
int main() {
    auto int x = 5;
    auto int y = 10;
    int z = 15;  // auto is the default for local variables

    // auto variables have automatic storage duration
    // They are created when the block is entered and destroyed when it exits
    auto int sum = x + y + z;

    return sum;  // Should be 30
}
