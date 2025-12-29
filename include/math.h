#ifndef _MATH_H
#define _MATH_H

/* HUGE_VAL - value used to indicate overflow in functions */
#define HUGE_VAL 1.7976931348623157e+308

/* Trigonometric functions */
extern double sin(double x);
extern double cos(double x);
extern double tan(double x);

/* Inverse trigonometric functions */
extern double asin(double x);
extern double acos(double x);
extern double atan(double x);
extern double atan2(double y, double x);

/* Hyperbolic functions */
extern double sinh(double x);
extern double cosh(double x);
extern double tanh(double x);

/* Exponential and logarithmic functions */
extern double exp(double x);        /* e^x */
extern double log(double x);        /* natural logarithm */
extern double log10(double x);      /* base-10 logarithm */

/* Power functions */
extern double pow(double x, double y);  /* x^y */
extern double sqrt(double x);           /* square root */

/* Rounding and absolute value */
extern double ceil(double x);       /* smallest integer >= x */
extern double floor(double x);      /* largest integer <= x */
extern double fabs(double x);       /* absolute value */

/* Floating-point manipulation */
extern double ldexp(double x, int exp);                /* x * 2^exp */
extern double frexp(double x, int *exp);               /* break into mantissa and exponent */
extern double modf(double x, double *iptr);            /* break into integer and fractional parts */
extern double fmod(double x, double y);                /* floating-point remainder of x/y */

#endif /* _MATH_H */
