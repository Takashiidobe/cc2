typedef int wchar_t;

int main() {
    wchar_t wc = L'A';
    if (wc != 65) {
        return 1;
    }

    wchar_t ws[] = L"Hi";
    if (ws[0] != 'H') {
        return 2;
    }
    if (ws[1] != 'i') {
        return 3;
    }
    if (ws[2] != 0) {
        return 4;
    }

    wchar_t *p = L"OK";
    if (p[0] != 'O' || p[1] != 'K' || p[2] != 0) {
        return 5;
    }

    return 0;
}
