int main() {
    int a = 90;
    int b = 15;
    unsigned int ua = 240;
    unsigned int ub = 15;

    int r1 = a & b;
    int r2 = a | b;
    int r3 = a ^ b;
    unsigned int r4 = ua & ub;
    unsigned int r5 = ua | ub;
    unsigned int r6 = ua ^ ub;

    return (r1 + r2 + r3 + r4 + r5 + r6) & 255;
}
