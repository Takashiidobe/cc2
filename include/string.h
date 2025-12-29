#ifndef _STRING_H
#define _STRING_H

/* NULL pointer constant */
#ifndef NULL
#define NULL ((void*)0)
#endif

/* size_t type - unsigned integer type of sizeof operator */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif

/* Memory manipulation functions */
extern void *memcpy(void *dest, const void *src, size_t n);
extern void *memmove(void *dest, const void *src, size_t n);
extern int memcmp(const void *s1, const void *s2, size_t n);
extern void *memset(void *s, int c, size_t n);
extern void *memchr(const void *s, int c, size_t n);

/* String copying functions */
extern char *strcpy(char *dest, const char *src);
extern char *strncpy(char *dest, const char *src, size_t n);

/* String concatenation functions */
extern char *strcat(char *dest, const char *src);
extern char *strncat(char *dest, const char *src, size_t n);

/* String comparison functions */
extern int strcmp(const char *s1, const char *s2);
extern int strncmp(const char *s1, const char *s2, size_t n);
extern int strcoll(const char *s1, const char *s2);
extern size_t strxfrm(char *dest, const char *src, size_t n);

/* String search functions */
extern char *strchr(const char *s, int c);
extern char *strrchr(const char *s, int c);
extern size_t strspn(const char *s, const char *accept);
extern size_t strcspn(const char *s, const char *reject);
extern char *strpbrk(const char *s, const char *accept);
extern char *strstr(const char *haystack, const char *needle);
extern char *strtok(char *str, const char *delim);

/* String length and error functions */
extern size_t strlen(const char *s);
extern char *strerror(int errnum);

#endif /* _STRING_H */
