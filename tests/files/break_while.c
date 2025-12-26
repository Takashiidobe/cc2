// Test: break in while loop
// Should break out of loop when i == 5

int main() {
    int i = 0;
    int sum = 0;

    while (i < 10) {
        if (i == 5) {
            break;
        }
        sum += i;
        i++;
    }

    // sum should be 0 + 1 + 2 + 3 + 4 = 10
    return sum;
}
