/* assert.h - Diagnostics (C89) */

/*
 * Note: assert.h is special - it can be included multiple times
 * to redefine assert based on whether NDEBUG is defined.
 * Therefore, no include guard is used.
 */

#undef assert

#ifdef NDEBUG
    /* If NDEBUG is defined, assert does nothing */
    /* Note: Should be ((void)0) but compiler doesn't support (void) cast yet */
    #define assert(expression) (0)
#else
    /* If NDEBUG is not defined, assert checks the expression */

    /* External declarations for assert implementation */
    /* Note: Should be const char* but compiler doesn't support const in params yet */
    extern void __assert_fail(char *assertion, char *file, int line);

    #define assert(expression) \
        ((expression) ? 0 : \
         (__assert_fail(#expression, __FILE__, __LINE__), 0))
#endif
