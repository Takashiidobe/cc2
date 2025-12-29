/* limits.h - Implementation limits for integer types (C89) */

#ifndef _LIMITS_H
#define _LIMITS_H

/* Number of bits in a char */
#define CHAR_BIT 8

/* Minimum and maximum values for a signed char */
#define SCHAR_MIN (-128)
#define SCHAR_MAX 127

/* Maximum value for an unsigned char */
#define UCHAR_MAX 255

/* Minimum and maximum values for a char */
/* On x86-64, char is signed by default */
#define CHAR_MIN SCHAR_MIN
#define CHAR_MAX SCHAR_MAX

/* Minimum and maximum values for a short */
#define SHRT_MIN (-32768)
#define SHRT_MAX 32767

/* Maximum value for an unsigned short */
#define USHRT_MAX 65535

/* Minimum and maximum values for an int */
#define INT_MIN (-2147483648)
#define INT_MAX 2147483647

/* Maximum value for an unsigned int */
#define UINT_MAX 4294967295U

/* Minimum and maximum values for a long */
/* On x86-64 Linux, long is 64 bits */
#define LONG_MIN (-9223372036854775807L - 1L)
#define LONG_MAX 9223372036854775807L

/* Maximum value for an unsigned long */
#define ULONG_MAX 18446744073709551615UL

#endif /* _LIMITS_H */
