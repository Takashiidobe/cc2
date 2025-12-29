// Test limits.h standard header

#include <limits.h>

int main() {
    // Test that the macros are defined and have reasonable values

    // CHAR_BIT should be 8
    if (CHAR_BIT != 8) return 1;

    // Test char limits
    if (CHAR_MIN != -128) return 2;
    if (CHAR_MAX != 127) return 3;
    if (UCHAR_MAX != 255) return 4;

    // Test short limits
    if (SHRT_MIN != -32768) return 5;
    if (SHRT_MAX != 32767) return 6;
    if (USHRT_MAX != 65535) return 7;

    // Test int limits
    if (INT_MIN != -2147483648) return 8;
    if (INT_MAX != 2147483647) return 9;

    // Test that we can use the macros in expressions
    int x = INT_MAX;
    if (x != 2147483647) return 10;

    unsigned int y = UINT_MAX;
    if (y < 4000000000) return 11;  // UINT_MAX should be 4294967295

    return 0;
}
