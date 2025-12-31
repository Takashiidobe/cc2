#ifndef _STDATOMIC_H
#define _STDATOMIC_H

#define _Atomic

typedef int atomic_int;

static int atomic_compare_exchange_weak(atomic_int *obj, int *expected, int desired) {
    int current = *obj;
    if (current == *expected) {
        *obj = desired;
        return 1;
    }
    *expected = current;
    return 0;
}

static int atomic_exchange(atomic_int *obj, int desired) {
    int old = *obj;
    *obj = desired;
    return old;
}

#endif /* _STDATOMIC_H */
