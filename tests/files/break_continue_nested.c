// Test: break and continue in nested loops
// Tests that break/continue affect only the innermost loop

int main() {
    int sum = 0;

    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (j == 3) {
                break;  // Break inner loop when j == 3
            }
            if (j % 2 == 0) {
                continue;  // Skip even j values
            }
            sum += i * 10 + j;
        }
    }

    // For each i (0-4):
    //   j iterates: 0 (skip), 1 (add), 2 (skip), 3 (break)
    //   So we add: i*10 + 1 for each i
    // sum = 1 + 11 + 21 + 31 + 41 = 105
    return sum;
}
