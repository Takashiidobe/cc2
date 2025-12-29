// Test switch with fall-through
int main() {
    int x = 1;
    int result = 0;
    switch (x) {
        case 1:
            result = result + 1;
        case 2:
            result = result + 10;
        case 3:
            result = result + 100;
            break;
    }
    return result;
}
