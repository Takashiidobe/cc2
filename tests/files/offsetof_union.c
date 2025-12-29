// Test offsetof with union (all members at offset 0)
// For gcc: define offsetof (our compiler has builtin)
#ifdef __GNUC__
#ifndef offsetof
#define offsetof(type, member) ((unsigned long)&(((type*)0)->member))
#endif
#endif

union Value {
    int i;
    long l;
};

int main() {
    return offsetof(union Value, i) + offsetof(union Value, l);
}
