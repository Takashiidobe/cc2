enum Color {
    RED,
    GREEN,
    BLUE
};

enum Status {
    OK = 0,
    ERROR = 1,
    PENDING = 5,
    DONE
};

int main() {
    enum Color c = RED;
    enum Status s = PENDING;
    int x = GREEN;
    int y = DONE;
    return c + s + x + y;
}
