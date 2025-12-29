#include <errno.h>

int main() {
    int err = EDOM + ERANGE;
    if (err <= 0) {
        return 1;
    }
    return 0;
}
