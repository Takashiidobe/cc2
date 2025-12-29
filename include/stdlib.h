#ifndef _STDLIB_H
#define _STDLIB_H

/* NULL pointer constant */
#ifndef NULL
#define NULL ((void*)0)
#endif

/* size_t type - unsigned integer type of sizeof operator */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif

/* wchar_t type - wide character type */
#ifndef _WCHAR_T
#define _WCHAR_T
typedef int wchar_t;
#endif

/* Process exit codes */
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

/* Maximum value returned by rand() */
#define RAND_MAX 2147483647

/* Maximum number of bytes in a multibyte character */
#define MB_CUR_MAX 1

/* Structure types for div and ldiv */
struct div_struct {
    int quot;  /* quotient */
    int rem;   /* remainder */
};
typedef struct div_struct div_t;

struct ldiv_struct {
    long quot; /* quotient */
    long rem;  /* remainder */
};
typedef struct ldiv_struct ldiv_t;

/* String conversion functions */
extern double atof(const char *nptr);
extern int atoi(const char *nptr);
extern long atol(const char *nptr);
extern double strtod(const char *nptr, char **endptr);
extern long strtol(const char *nptr, char **endptr, int base);
extern unsigned long strtoul(const char *nptr, char **endptr, int base);

/* Random number generation */
extern int rand(void);
extern void srand(unsigned int seed);

/* Memory allocation functions */
extern void *calloc(size_t nmemb, size_t size);
extern void *malloc(size_t size);
extern void *realloc(void *ptr, size_t size);
extern void free(void *ptr);

/* Process control functions */
extern void abort(void);
/* NOTE: atexit not yet supported - requires function pointer parameters */
/* extern int atexit(void (*func)(void)); */
extern void exit(int status);
extern char *getenv(const char *name);
extern int system(const char *string);

/* Searching and sorting */
/* NOTE: bsearch and qsort not yet supported - require function pointer parameters */
/* extern void *bsearch(const void *key, const void *base, size_t nmemb,
                     size_t size, int (*compar)(const void *, const void *));
extern void qsort(void *base, size_t nmemb, size_t size,
                  int (*compar)(const void *, const void *)); */

/* Integer arithmetic functions */
extern int abs(int j);
extern long labs(long j);
extern div_t div(int numer, int denom);
extern ldiv_t ldiv(long numer, long denom);

/* Multibyte character functions */
extern int mblen(const char *s, size_t n);
extern int mbtowc(wchar_t *pwc, const char *s, size_t n);
extern int wctomb(char *s, wchar_t wchar);
extern size_t mbstowcs(wchar_t *pwcs, const char *s, size_t n);
extern size_t wcstombs(char *s, const wchar_t *pwcs, size_t n);

#endif /* _STDLIB_H */
