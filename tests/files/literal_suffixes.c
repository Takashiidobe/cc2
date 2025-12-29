// Test integer and floating-point literal suffixes

int main() {
    // Integer suffixes
    int a = 42;                 // No suffix
    unsigned int b = 42U;       // Unsigned
    long c = 42L;               // Long
    unsigned long d = 42UL;     // Unsigned long
    unsigned long e = 42ul;     // unsigned long (lowercase)
    unsigned long f = 42LU;     // Unsigned long (reversed)

    // Floating-point suffixes
    float x = 3.14;   // No suffix (double)
    float y = 3.14F;  // Float
    float z = 3.14f;  // float (lowercase)

    // Scientific notation with suffixes
    float s1 = 1.5e10;
    float s2 = 1.5e10F;

    double ld = 3.14L;

    // Verify values are correct (suffixes shouldn't change value)
    if (a != 42) return 1;
    if (b != 42) return 2;
    if (c != 42) return 3;
    if (d != 42) return 4;

    if (sizeof(42U) != sizeof(unsigned int)) return 5;
    if (sizeof(42L) != sizeof(long)) return 6;
    if (sizeof(42UL) != sizeof(unsigned long)) return 7;
    if (sizeof(3.14F) != sizeof(float)) return 8;

    if ((0U - 1) <= 0) return 9;

    return 0;
}
