// Test: break in for loop
// Should break when i == 7

int main() {
    int sum = 0;

    for (int i = 0; i < 10; i++) {
        if (i == 7) {
            break;
        }
        sum += i;
    }

    // sum should be 0 + 1 + 2 + 3 + 4 + 5 + 6 = 21
    return sum;
}
