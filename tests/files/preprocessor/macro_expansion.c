// Test macro expansion
#define A 5
#define B A + 3
#define C B * 2

int main() {
    return C;
}
