// Test string.h string manipulation functions
#include <string.h>

int main() {
    int result = 0;

    // Test strlen
    char str1[] = "hello";
    if (strlen(str1) == 5) {
        result = result + 1;
    }

    // Test strcpy
    char dest[10];
    strcpy(dest, "abc");
    if (dest[0] == 'a' && dest[1] == 'b' && dest[2] == 'c' && dest[3] == '\0') {
        result = result + 1;
    }

    // Test strcat
    char buf[20] = "foo";
    strcat(buf, "bar");
    if (buf[0] == 'f' && buf[3] == 'b' && buf[5] == 'r' && buf[6] == '\0') {
        result = result + 1;
    }

    // Test strcmp
    if (strcmp("abc", "abc") == 0) {
        result = result + 1;
    }
    if (strcmp("abc", "abd") < 0) {
        result = result + 1;
    }

    // Test strchr
    char *p = strchr("hello", 'l');
    if (p != NULL && *p == 'l') {
        result = result + 1;
    }

    // Test memset
    char mem[5];
    memset(mem, 'x', 5);
    if (mem[0] == 'x' && mem[4] == 'x') {
        result = result + 1;
    }

    // Test memcpy
    char src[] = "test";
    char dst[5];
    memcpy(dst, src, 5);
    if (dst[0] == 't' && dst[3] == 't' && dst[4] == '\0') {
        result = result + 1;
    }

    // Test memcmp
    if (memcmp("abc", "abc", 3) == 0) {
        result = result + 1;
    }
    if (memcmp("abc", "abd", 3) < 0) {
        result = result + 1;
    }

    // Should return 10 if all tests pass
    return result;
}
