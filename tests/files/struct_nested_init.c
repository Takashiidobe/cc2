// Test: Nested struct initialization

struct Inner {
    int a;
    int b;
};

struct Outer {
    int x;
    struct Inner inner;
    int y;
};

int main() {
    struct Outer o = {.x = 5, .inner = {.a = 10, .b = 20}, .y = 15};
    struct Inner* ptr = &o.inner;
    return o.x + ptr->a + ptr->b + o.y;  // Should be 50 (5 + 10 + 20 + 15)
}
