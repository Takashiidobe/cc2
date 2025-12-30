int add(int a, int b) {
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

int main() {
    int (*op)(int, int) = &add;

    if ((*op)(3, 4) != 7) {
        return 1;
    }

    op = &sub;
    if ((*op)(10, 4) != 6) {
        return 1;
    }

    return 0;
}
