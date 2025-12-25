int main() {
    int x = 0;
    int y = 0;
    int i = 0;
    while (i < 5) {
        if (i < 2 || i == 4) {
            y = y + 1;
        } else {
            y = y + 2;
        }
        x = x + i;
        i = i + 1;
    }
    return x + y;
}
