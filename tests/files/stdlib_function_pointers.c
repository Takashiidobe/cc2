#include <stdlib.h>

static int called = 0;

void mark(void) {
    called = 1;
}

int compare(const void *a, const void *b) {
    int av = *(int *)a;
    int bv = *(int *)b;

    if (av < bv) {
        return -1;
    }
    if (av > bv) {
        return 1;
    }
    return 0;
}

int main() {
    int arr[3] = {3, 1, 2};
    int key = 2;

    if (atexit(mark) != 0) {
        return 1;
    }

    qsort(arr, 3, 4, compare);
    if (arr[0] != 1 || arr[1] != 2 || arr[2] != 3) {
        return 2;
    }

    int *found = bsearch(&key, arr, 3, 4, compare);
    if (!found || *found != 2) {
        return 3;
    }

    if (called != 0) {
        return 4;
    }

    return 0;
}
