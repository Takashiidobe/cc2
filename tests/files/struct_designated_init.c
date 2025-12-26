// Test: Designated initializer for struct

struct Point {
    int x;
    int y;
};

int main() {
    struct Point p = {.x = 10, .y = 20};
    return p.x + p.y;  // Should be 30
}
