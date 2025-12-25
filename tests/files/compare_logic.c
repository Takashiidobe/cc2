int main() {
    int a = 5;
    int b = 7;
    int c = 0;
    if (a < b && b != 0) {
        c = 1;
    }
    if (a >= b || a == 5) {
        c = c + 2;
    }
    return c;
}
