int sum(a, b)
int a;
char b;
{
    return a + b;
}

int mul(a, b)
int a;
int b;
{
    return a * b;
}

int main() {
    if (sum(10, 5) != 15) {
        return 1;
    }

    if (mul(3, 4) != 12) {
        return 2;
    }

    return 0;
}
