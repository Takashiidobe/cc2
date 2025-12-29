// Test float parameters and calling convention
double add_doubles(double a, double b) {
    return a + b;
}

int mixed_params(int x, double y, int z, double w) {
    double sum = y + w;
    int result = x + z;
    return result;
}

int main() {
    double result1 = add_doubles(3.14, 2.71);
    int result2 = mixed_params(10, 1.5, 20, 2.5);
    return 0;
}
