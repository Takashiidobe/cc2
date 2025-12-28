int counter;

void increment_counter() {
    counter = counter + 1;
}

int main() {
    counter = 0;
    increment_counter();
    increment_counter();
    increment_counter();
    return counter;
}
