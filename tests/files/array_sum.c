int sum(int* arr, int len) {
    int total = 0;
    for (int i = 0; i < len; i = i + 1) {
        total = total + arr[i];
    }
    return total;
}

int main() {
    int vals[4] = {2, 4, 6, 8};
    return sum(vals, 4);
}
