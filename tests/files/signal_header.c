#include <signal.h>

int main() {
    sig_atomic_t flag = 0;
    int sig = SIGINT;
    int sum = SIGABRT + SIGFPE + SIGILL + SIGSEGV + SIGTERM;

    if (sum == 0) {
        return 1;
    }

    if (sig == 0) {
        return 1;
    }

    if (flag != 0) {
        return 1;
    }

    if (SIG_ERR == SIG_DFL) {
        return 1;
    }

    signal(SIGINT, SIG_DFL);
    signal(SIGTERM, SIG_IGN);

    return 0;
}
