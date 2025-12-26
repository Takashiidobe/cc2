// Test: Designated initializers in any order

struct Point {
    int x;
    int y;
    int z;
};

int main() {
    // Initialize in different order than declaration
    struct Point p = {.z = 30, .x = 10, .y = 20};
    return p.x + p.y + p.z;  // Should be 60
}
