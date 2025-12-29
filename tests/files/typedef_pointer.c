// Test typedef with pointer type
typedef int* intptr;

int main() {
    int value = 55;
    intptr p = &value;
    return *p;
}
