// Test functions with more than 6 parameters
int add_many(int a, int b, int c, int d, int e, int f, int g, int h) {
    return a + b + c + d + e + f + g + h;
}

int add_10_params(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    return a + b + c + d + e + f + g + h + i + j;
}

int main() {
    int result1 = add_many(1, 2, 3, 4, 5, 6, 7, 8);
    int result2 = add_10_params(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    return 0;
}
