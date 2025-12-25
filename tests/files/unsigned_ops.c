int main() {
    unsigned int a = 4000000000;
    unsigned int b = 2;
    unsigned int c = a / b;
    if (a > b && c >= b) {
        return 1;
    }
    return 0;
}
