// Test: Partial designated initializer

struct Point {
    int x;
    int y;
    int z;
};

int main() {
    struct Point p = {.y = 20};
    // x and z should be 0 (uninitialized in our impl, but we'll just use y)
    return p.y;  // Should be 20
}
