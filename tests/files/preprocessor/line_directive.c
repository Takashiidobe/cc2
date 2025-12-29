// Test #line directive

int a = __LINE__;  // Should be 3

#line 100
int b = __LINE__;  // Should be 100
int c = __LINE__;  // Should be 101

#line 200 "other.c"
int d = __LINE__;  // Should be 200

int main() {
    // Verify the line numbers are correct
    if (a != 3) return 1;
    if (b != 100) return 2;
    if (c != 101) return 3;
    if (d != 200) return 4;
    return 0;
}
