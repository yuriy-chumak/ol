#ifndef BTPD_EVLOOP_H
#define BTPD_EVLOOP_H

#include <sys/time.h>
#include <stdint.h>

#include "timeheap.h"

#define EV_READ    1
#define EV_WRITE   2
#define EV_TIMEOUT 3

typedef void (*evloop_cb_t)(int fd, short type, void *arg);

#if defined(EVLOOP_EPOLL) || defined(EVLOOP_KQUEUE)

struct fdev {
    evloop_cb_t cb;
    void *arg;
    int fd;
    uint16_t flags;
#ifdef EVLOOP_EPOLL
    int16_t index;
#else
    int16_t rdidx;
    int16_t wridx;
#endif
};

#elif defined(EVLOOP_POLL)

struct fdev {
    int i;
};

#else
#error No evloop method defined.
#endif

struct timeout {
    evloop_cb_t cb;
    void *arg;
    struct th_handle th;
};

int evloop_init(void);
int evloop(void);

int fdev_new(struct fdev *ev, int fd, uint16_t flags, evloop_cb_t cb,
    void *arg);
int fdev_del(struct fdev *ev);
int fdev_enable(struct fdev *ev, uint16_t flags);
int fdev_disable(struct fdev *ev, uint16_t flags);

void evtimer_init(struct timeout *, evloop_cb_t, void *);
int evtimer_add(struct timeout *, struct timespec *);
void evtimer_del(struct timeout *);

void evtimers_run(void);
struct timespec evtimer_delay(void);
int evtimer_gettime(struct timespec *);

#endif
