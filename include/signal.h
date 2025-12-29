#ifndef _SIGNAL_H
#define _SIGNAL_H

/* sig_atomic_t type */
typedef int sig_atomic_t;

/* Signal handler values */
#define SIG_DFL ((void *)0)
#define SIG_IGN ((void *)1)
#define SIG_ERR ((void *)-1)

/* Signals */
#define SIGABRT 6
#define SIGFPE 8
#define SIGILL 4
#define SIGINT 2
#define SIGSEGV 11
#define SIGTERM 15

/* Signal functions */
extern int raise(int sig);
/* NOTE: function pointer types are not fully supported yet; use void* for now. */
extern void *signal(int sig, void *handler);

#endif /* _SIGNAL_H */
