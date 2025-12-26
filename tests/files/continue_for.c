// Test: continue in for loop
// Should skip even numbers

int main() {
    int sum = 0;

    for (int i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            continue;
        }
        sum += i;
    }

    // sum should be 1 + 3 + 5 + 7 + 9 = 25
    return sum;
}
