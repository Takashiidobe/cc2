int counter() {
    static int count = 0;
    count = count + 1;
    return count;
}

int main() {
    int result;
    counter();
    counter();
    result = counter();
    return result;
}
