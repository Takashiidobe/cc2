// Test #if with expressions
#define VERSION 2

int main() {
    int result = 0;

#if VERSION == 1
    result = 10;
#elif VERSION == 2
    result = 20;
#elif VERSION == 3
    result = 30;
#else
    result = 0;
#endif

    return result;
}
