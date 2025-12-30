#ifndef _STDDEF_H
#define _STDDEF_H

/* NULL pointer constant */
#ifndef NULL
#define NULL ((void*)0)
#endif

/* size_t type - unsigned integer type of sizeof operator */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif

/* ptrdiff_t type - signed integer type of pointer subtraction */
#ifndef _PTRDIFF_T
#define _PTRDIFF_T
typedef long ptrdiff_t;
#endif

/* wchar_t type - wide character type */
#ifndef _WCHAR_T
#define _WCHAR_T
typedef int wchar_t;
#endif

/* Offset of a member within a struct */
#define offsetof(type, member) ((size_t)&(((type *)0)->member))

#endif /* _STDDEF_H */
