/* float.h - Floating point characteristics (C89) */

#ifndef _FLOAT_H
#define _FLOAT_H

/* Radix of exponent representation */
#define FLT_RADIX 2

/* Number of decimal digits of precision */
#define FLT_DIG 6
#define DBL_DIG 15
#define LDBL_DIG 18

/* Number of base-FLT_RADIX digits in mantissa */
#define FLT_MANT_DIG 24
#define DBL_MANT_DIG 53
#define LDBL_MANT_DIG 64

/* Minimum negative integer such that FLT_RADIX raised to that power minus 1 is normalized */
#define FLT_MIN_EXP (-125)
#define DBL_MIN_EXP (-1021)
#define LDBL_MIN_EXP (-16381)

/* Minimum negative integer such that 10 raised to that power is normalized */
#define FLT_MIN_10_EXP (-37)
#define DBL_MIN_10_EXP (-307)
#define LDBL_MIN_10_EXP (-4931)

/* Maximum integer such that FLT_RADIX raised to that power minus 1 is representable */
#define FLT_MAX_EXP 128
#define DBL_MAX_EXP 1024
#define LDBL_MAX_EXP 16384

/* Maximum integer such that 10 raised to that power is representable */
#define FLT_MAX_10_EXP 38
#define DBL_MAX_10_EXP 308
#define LDBL_MAX_10_EXP 4932

/* Maximum representable finite floating-point number */
#define FLT_MAX 3.402823466e+38F
#define DBL_MAX 1.7976931348623157e+308
#define LDBL_MAX 1.18973149535723176502e+4932L

/* Minimum normalized positive floating-point number */
#define FLT_MIN 1.175494351e-38F
#define DBL_MIN 2.2250738585072014e-308
#define LDBL_MIN 3.36210314311209350626e-4932L

/* Difference between 1.0 and the next representable value (machine epsilon) */
#define FLT_EPSILON 1.192092896e-07F
#define DBL_EPSILON 2.2204460492503131e-16
#define LDBL_EPSILON 1.08420217248550443401e-19L

/* Addition rounds to nearest */
#define FLT_ROUNDS 1

/* Floating-point evaluation method */
/* 0: evaluate all to type's range and precision */
/* 1: evaluate float/double to double, long double to long double */
/* 2: evaluate all to long double */
#define FLT_EVAL_METHOD 0

#endif /* _FLOAT_H */
