// Test struct bit-fields

struct Flags {
    unsigned int flag1 : 1;
    unsigned int flag2 : 1;
    unsigned int value : 4;
    unsigned int padding : 26;
};

int main() {
    struct Flags f;

    // Set bit-fields
    f.flag1 = 1;
    f.flag2 = 0;
    f.value = 5;

    // Read bit-fields
    int result = 0;
    if (f.flag1 == 1) {
        result = result + 1;
    }
    if (f.flag2 == 0) {
        result = result + 1;
    }
    if (f.value == 5) {
        result = result + 1;
    }

    // Expected: 3
    return result;
}
