#ifndef _CTYPE_H
#define _CTYPE_H

/* Character classification functions */
extern int isalnum(int c);  /* alphanumeric (letter or digit) */
extern int isalpha(int c);  /* alphabetic (letter) */
extern int iscntrl(int c);  /* control character */
extern int isdigit(int c);  /* decimal digit (0-9) */
extern int isgraph(int c);  /* printing character except space */
extern int islower(int c);  /* lowercase letter */
extern int isprint(int c);  /* printing character including space */
extern int ispunct(int c);  /* printing character except space, letter, or digit */
extern int isspace(int c);  /* whitespace character */
extern int isupper(int c);  /* uppercase letter */
extern int isxdigit(int c); /* hexadecimal digit (0-9, A-F, a-f) */

/* Character conversion functions */
extern int tolower(int c);  /* convert to lowercase */
extern int toupper(int c);  /* convert to uppercase */

#endif /* _CTYPE_H */
