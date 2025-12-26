// Test: Modify global variable

int counter = 5;

int main() {
    counter = counter + 10;
    counter++;
    return counter;  // Should return 16
}
