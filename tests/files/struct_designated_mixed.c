// Test: Mix positional and designated initializers

struct Data {
    int a;
    int b;
    int c;
};

int main() {
    // Positional for a, designated for c
    struct Data d = {5, .c = 15};
    return d.a + d.c;  // Should be 20 (5 + 15)
}
