int add(int a, int b);
int multiply(int a, int b);
int subtract(int a, int b);

int main() {
    int x = add(5, 3);
    int y = multiply(4, 2);
    int z = subtract(10, 3);
    return x + y + z;
}

int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

int subtract(int a, int b) {
    return a - b;
}
