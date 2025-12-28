int main() {
    int x;
    int* int_ptr;
    long* long_ptr;
    char* char_ptr;

    x = 123;
    int_ptr = &x;
    long_ptr = (long*)int_ptr;
    char_ptr = (char*)long_ptr;

    return *((int*)char_ptr);
}
