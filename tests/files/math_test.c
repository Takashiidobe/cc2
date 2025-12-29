// Test math.h mathematical functions
#include <math.h>

int main() {
    // Test sqrt - convert to int (should be 2)
    double s = sqrt(4.0);
    int sqrt_result = (int)s;

    // Test fabs - convert to int (should be 5)
    double a = fabs(-5.5);
    int fabs_result = (int)a;

    // Test pow - convert to int (should be 8)
    double p = pow(2.0, 3.0);
    int pow_result = (int)p;

    // Test floor - convert to int (should be 3)
    double f = floor(3.7);
    int floor_result = (int)f;

    // Test ceil - convert to int (should be 4)
    double c = ceil(3.2);
    int ceil_result = (int)c;

    // Return sum: 2 + 5 + 8 + 3 + 4 = 22
    return sqrt_result + fabs_result + pow_result + floor_result + ceil_result;
}
