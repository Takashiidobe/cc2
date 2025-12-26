// Test: continue in while loop
// Should skip i == 3

int main() {
    int i = 0;
    int sum = 0;

    while (i < 5) {
        i++;
        if (i == 3) {
            continue;
        }
        sum += i;
    }

    // sum should be 1 + 2 + 4 + 5 = 12
    return sum;
}
