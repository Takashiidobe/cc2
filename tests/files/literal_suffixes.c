// Test integer and floating-point literal suffixes

int main() {
    // Integer suffixes
    int a = 42;       // No suffix
    int b = 42U;      // Unsigned
    int c = 42L;      // Long
    int d = 42UL;     // Unsigned long
    int e = 42ul;     // unsigned long (lowercase)
    int f = 42LU;     // Unsigned long (reversed)

    // Floating-point suffixes
    float x = 3.14;   // No suffix (double)
    float y = 3.14F;  // Float
    float z = 3.14f;  // float (lowercase)

    // Scientific notation with suffixes
    float s1 = 1.5e10;
    float s2 = 1.5e10F;

    // Verify values are correct (suffixes shouldn't change value)
    if (a != 42) return 1;
    if (b != 42) return 2;
    if (c != 42) return 3;
    if (d != 42) return 4;

    return 0;
}
