struct Point {
    int x;
    int y;
};

int main() {
    struct Point p = {10, 20};
    struct Point* ptr = &p;
    return ptr->x + ptr->y;
}
