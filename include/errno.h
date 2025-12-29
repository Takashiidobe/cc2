#ifndef _ERRNO_H
#define _ERRNO_H

/* errno - error reporting */
extern int *__errno_location(void);
#define errno (*__errno_location())

/* Error codes */
#define EDOM 33
#define ERANGE 34

#endif /* _ERRNO_H */
