struct Pair {
    int a;
    int b;
};

int main() {
    struct Pair p = {3, 4};
    struct Pair* ptr = &p;
    return p.a * 2 + ptr->b;
}
