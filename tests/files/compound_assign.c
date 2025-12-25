int main() {
    int x = 10;
    unsigned int u = 100;

    x += 5;
    x -= 3;
    x *= 2;
    x /= 3;

    u += 20;
    u -= 10;
    u *= 2;
    u /= 5;

    return (x + u) & 255;
}
