int main() {
    long big;
    int medium;
    short small;
    char tiny;

    big = 300;
    medium = (int)big;
    small = (short)medium;
    tiny = (char)small;

    // 300 & 0xFF = 44
    return (int)tiny;
}
