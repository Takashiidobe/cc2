#ifndef _PTHREAD_H
#define _PTHREAD_H

#ifndef NULL
#define NULL ((void *)0)
#endif

typedef int pthread_t;
typedef int pthread_attr_t;

static int pthread_create(
    pthread_t *thread,
    const pthread_attr_t *attr,
    int (*start_routine)(void *),
    void *arg
) {
    (void)thread;
    (void)attr;
    start_routine(arg);
    return 0;
}

static int pthread_join(pthread_t thread, void **retval) {
    (void)thread;
    if (retval) {
        *retval = 0;
    }
    return 0;
}

#endif /* _PTHREAD_H */
