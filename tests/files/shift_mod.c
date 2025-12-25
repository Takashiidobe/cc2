int main() {
    int a = 5;
    int b = 2;
    int c = a << b;
    int d = c >> 1;
    int sr = -8 >> 1;
    unsigned int ua = 240;
    unsigned int ub = ua >> 3;
    int e = 17 % 5;
    unsigned int ue = 17 % 5;

    return (c + d + (sr & 255) + ub + e + ue) & 255;
}
