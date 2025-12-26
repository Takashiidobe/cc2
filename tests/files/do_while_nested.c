int main() {
    int i = 0;
    int j = 0;
    int sum = 0;
    do {
        j = 0;
        do {
            sum = sum + 1;
            j = j + 1;
        } while (j < 2);
        i = i + 1;
    } while (i < 3);
    return sum;
}
