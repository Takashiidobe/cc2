// Test offsetof for first member (should be 0)
// For gcc: define offsetof (our compiler has builtin)
#ifdef __GNUC__
#ifndef offsetof
#define offsetof(type, member) ((unsigned long)&(((type*)0)->member))
#endif
#endif

struct Data {
    int a;
    int b;
    int c;
};

int main() {
    return offsetof(struct Data, a);
}
