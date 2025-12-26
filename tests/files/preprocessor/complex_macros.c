// Test complex macro scenarios
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define ABS(x) ((x) < 0 ? -(x) : (x))

int main() {
    int a = MAX(10, 20);
    int b = MIN(15, 5);
    int c = ABS(0 - 7);
    return a + b + c;
}
