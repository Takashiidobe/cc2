int main() {
    char c = -5;
    unsigned char uc = 200;
    short s = -12;
    int x = 7;

    if (+c != -5) return 1;
    if (+uc != 200) return 2;
    if (+s != -12) return 3;
    if (+x != 7) return 4;

    if (sizeof(+c) != sizeof(int)) return 5;
    if (sizeof(+uc) != sizeof(int)) return 6;
    if (sizeof(+s) != sizeof(int)) return 7;

    return 0;
}
