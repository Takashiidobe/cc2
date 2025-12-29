// Test va_list type support
// va_list is a built-in keyword in cc2
#include <stdarg.h>

int main() {
    va_list args;
    // Just test that we can declare a va_list variable
    // Can't actually use it without va_start/va_arg/va_end
    return 0;
}
