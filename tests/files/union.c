union Data {
    int i;
    char c;
    long l;
};

int main() {
    union Data d;
    d.i = 42;
    int x = d.i;
    d.c = 65;
    int y = d.c;
    d.l = 100;
    int z = d.l;
    return x + y + z;
}
