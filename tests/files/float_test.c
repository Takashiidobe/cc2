// Test float.h standard header

#include <float.h>

int main() {
    // Test that basic macros are defined

    // FLT_RADIX should be 2 for binary floating point
    if (FLT_RADIX != 2) return 1;

    // FLT_DIG should be 6 for single precision
    if (FLT_DIG != 6) return 2;

    // DBL_DIG should be 15 for double precision
    if (DBL_DIG != 15) return 3;

    // Test that we can use the epsilon values
    double epsilon = DBL_EPSILON;
    if (epsilon > 0.0001) return 4;  // Should be around 2.22e-16

    // Test max values are positive
    if (DBL_MAX < 1.0) return 5;
    if (FLT_MAX < 1.0) return 6;

    // Test min values are positive and small
    if (DBL_MIN > 1.0) return 7;
    if (DBL_MIN < 0.0) return 8;

    // Test FLT_ROUNDS is defined (typically 1 for round to nearest)
    if (FLT_ROUNDS < 0) return 9;

    return 0;
}
