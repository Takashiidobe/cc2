// Test #undef directive
#define VALUE 10

int main() {
    int x = VALUE;  // Should be 10

#undef VALUE
#define VALUE 20

    int y = VALUE;  // Should be 20

#undef VALUE

    // After #undef, VALUE is not defined
#ifdef VALUE
    int z = 100;  // Should not execute
#else
    int z = 30;   // Should execute
#endif

    return x + y + z;  // Should return 60
}
