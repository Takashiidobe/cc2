// Test offsetof with struct
// For gcc: define offsetof (our compiler has builtin)
#ifdef __GNUC__
#ifndef offsetof
#define offsetof(type, member) ((unsigned long)&(((type*)0)->member))
#endif
#endif

struct Point {
    int x;
    int y;
};

int main() {
    return offsetof(struct Point, y);
}
