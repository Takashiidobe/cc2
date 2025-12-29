// Test predefined macros (__FILE__, __LINE__, __STDC__, __DATE__, __TIME__)

int main() {
    // Test __LINE__ macro
    int line1 = __LINE__;  // Should be 5
    int line2 = __LINE__;  // Should be 6
    int line3 = __LINE__;  // Should be 7

    // Test __STDC__ (should be 1)
    int stdc = __STDC__;

    // Test __STDC_HOSTED__ (should be 1)
    int stdc_hosted = __STDC_HOSTED__;

    // Verify line numbers are correct
    if (line1 != 5 || line2 != 6 || line3 != 7) {
        return 1;  // Line numbers incorrect
    }

    // Verify STDC macros
    if (stdc != 1 || stdc_hosted != 1) {
        return 2;  // STDC macros incorrect
    }

    // __FILE__ and __DATE__/__TIME__ are harder to test automatically
    // but they should at least be defined and expand correctly

    return 0;
}
