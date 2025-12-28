int main() {
    int x;
    void* ptr;
    int* int_ptr;

    x = 42;
    ptr = &x;
    int_ptr = ptr;

    return *int_ptr;
}
