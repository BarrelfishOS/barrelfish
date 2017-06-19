#ifndef _PTHREADS_PRIVATE_H
#define _PTHREADS_PRIVATE_H

#include <sys/types.h>
#include <sys/cpuset.h>

struct pthread_attr {
    int stacksize;
    bool affinity_set;
    cpuset_t affinity;
};

struct pthread_barrier {
	int count;
	int max_count;
	struct thread_sem mutex;
	struct thread_sem barrier;
	struct thread_sem reset;
};

#endif
