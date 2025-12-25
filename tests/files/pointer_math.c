int main() {
    int arr[4] = {5, 6, 7, 8};
    int* p = arr;
    int* q = p + 2;
    return *q + *(p + 1);
}
