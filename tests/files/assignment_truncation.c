int main() {
    char c;
    unsigned char uc;
    short s;
    unsigned short us;

    return ((c = 255) < 0)
        + ((uc = 255) < 0)
        + ((s = 65535) < 0)
        + ((us = 65535) < 0);
}
