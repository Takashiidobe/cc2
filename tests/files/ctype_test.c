// Test ctype.h character classification functions
#include <ctype.h>

int main() {
    int result = 0;

    // Test isdigit
    if (isdigit('5')) {
        result = result + 1;
    }
    if (!isdigit('a')) {
        result = result + 1;
    }

    // Test isalpha
    if (isalpha('x')) {
        result = result + 1;
    }
    if (!isalpha('9')) {
        result = result + 1;
    }

    // Test isupper
    if (isupper('A')) {
        result = result + 1;
    }
    if (!isupper('b')) {
        result = result + 1;
    }

    // Test islower
    if (islower('z')) {
        result = result + 1;
    }
    if (!islower('Z')) {
        result = result + 1;
    }

    // Test isspace
    if (isspace(' ')) {
        result = result + 1;
    }
    if (!isspace('x')) {
        result = result + 1;
    }

    // Test toupper
    int upper = toupper('a');
    if (upper == 'A') {
        result = result + 1;
    }

    // Test tolower
    int lower = tolower('B');
    if (lower == 'b') {
        result = result + 1;
    }

    // Should return 12 if all tests pass
    return result;
}
