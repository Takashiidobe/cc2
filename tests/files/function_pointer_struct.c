int add(int a, int b) {
    return a + b;
}

struct Ops {
    int (*op)(int, int);
};

int main() {
    struct Ops ops = { add };

    if (ops.op(2, 3) != 5) {
        return 1;
    }

    return 0;
}
