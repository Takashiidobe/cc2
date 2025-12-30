#ifndef _SIGNAL_H
#define _SIGNAL_H

/* sig_atomic_t type */
typedef int sig_atomic_t;

/* Signal handler type */
typedef void (*__sighandler_t)(int);

/* Signal handler values */
#define SIG_DFL ((__sighandler_t)0)
#define SIG_IGN ((__sighandler_t)1)
#define SIG_ERR ((__sighandler_t)-1)

/* Signals */
#define SIGABRT 6
#define SIGFPE 8
#define SIGILL 4
#define SIGINT 2
#define SIGSEGV 11
#define SIGTERM 15

/* Signal functions */
extern int raise(int sig);
extern __sighandler_t signal(int sig, __sighandler_t handler);

#endif /* _SIGNAL_H */
