// Test memory functions from glibc
int printf(char *fmt, ...);
void *malloc(unsigned long size);
void free(void *ptr);
void *calloc(unsigned long nmemb, unsigned long size);
void *memcpy(void *dest, void *src, unsigned long n);
void *memset(void *s, int c, unsigned long n);

int main() {
    // Test malloc and free (using hardcoded size 4 for int on x86-64)
    int *ptr1 = malloc(4);
    *ptr1 = 42;
    printf("malloc test: %d\n", *ptr1);
    free(ptr1);

    // Test calloc (allocates and zeros memory)
    int *ptr2 = calloc(3, 4);
    printf("calloc test: %d %d %d\n", ptr2[0], ptr2[1], ptr2[2]);
    ptr2[0] = 10;
    ptr2[1] = 20;
    ptr2[2] = 30;
    printf("calloc after assignment: %d %d %d\n", ptr2[0], ptr2[1], ptr2[2]);
    free(ptr2);

    // Test memcpy (3 ints = 12 bytes on x86-64)
    int src[3];
    src[0] = 100;
    src[1] = 200;
    src[2] = 300;
    int dest[3];
    memcpy(dest, src, 12);
    printf("memcpy test: %d %d %d\n", dest[0], dest[1], dest[2]);

    // Test memset
    char buffer[10];
    memset(buffer, 65, 5); // 65 is 'A'
    buffer[5] = 0; // null terminate
    printf("memset test: %s\n", buffer);

    printf("All memory tests passed!\n");
    return 0;
}
