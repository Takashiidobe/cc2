int main() {
    int x = 10;
    unsigned int u = 100;

    x += 5;
    x -= 3;
    x *= 2;
    x /= 3;
    x %= 7;
    x <<= 2;
    x >>= 1;
    x &= 31;
    x |= 3;
    x ^= 5;

    u += 20;
    u -= 10;
    u *= 2;
    u /= 5;
    u %= 9;
    u <<= 1;
    u >>= 2;
    u &= 255;
    u |= 1;
    u ^= 4;

    return (x + u) & 255;
}
