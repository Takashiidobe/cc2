// Test function-like macros
#define ADD(a, b) a + b
#define MUL(x, y) x * y
#define SQUARE(n) n * n

int main() {
    int x = ADD(5, 3);
    int y = MUL(4, 2);
    int z = SQUARE(5);
    return x + y + z;
}
