// Test string functions from glibc
int printf(char *fmt, ...);
int strlen(char *s);
char *strcpy(char *dest, char *src);
int strcmp(char *s1, char *s2);
char *strcat(char *dest, char *src);

int main() {
    // Test strlen
    char *str1 = "Hello";
    int len = strlen(str1);
    printf("strlen(\"%s\") = %d\n", str1, len);

    // Test strcpy
    char buffer[50];
    strcpy(buffer, "World");
    printf("strcpy result: %s\n", buffer);

    // Test strcmp
    char *str2 = "Hello";
    char *str3 = "World";
    int cmp1 = strcmp(str1, str2);
    int cmp2 = strcmp(str1, str3);
    printf("strcmp(\"%s\", \"%s\") = %d\n", str1, str2, cmp1);
    printf("strcmp(\"%s\", \"%s\") = %d\n", str1, str3, cmp2);

    // Test strcat
    char cat_buffer[100];
    strcpy(cat_buffer, "Hello");
    strcat(cat_buffer, " ");
    strcat(cat_buffer, "World");
    printf("strcat result: %s\n", cat_buffer);

    printf("All string tests passed!\n");
    return 0;
}
