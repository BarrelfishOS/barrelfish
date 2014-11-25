/**
 * \file
 * \brief Spinning synchronizations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BOMP_SPIN_H
#define BOMP_SPIN_H

#include <barrelfish/barrelfish.h>
/* #include <string.h> */
#include <omp.h>

/** \brief spinlock */
typedef volatile unsigned int bomp_lock_t;

static inline void bomp_lock(bomp_lock_t *lock)
{
#ifdef __k1om__
    /* The Xeon Phi does not support pause instruction. we use delay instead
     * which does the same thing as pause but with a variable amount of delay.
     * 750 cycles for now.
     */
    uint32_t wait = 750;
    __asm__ __volatile__("0:\n\t"
                    "cmpq $0, %0\n\t"
                    "je 1f\n\t"
                    "delay %1\n\t"
                    "jmp 0b\n\t"
                    "1:\n\t"
                    "lock btsq $0, %0\n\t"
                    "jc 0b\n\t"
                    : "+m" (*lock), "=r"(wait) : : "memory", "cc");
#else
    __asm__ __volatile__("0:\n\t"
                    "cmpq $0, %0\n\t"
                    "je 1f\n\t"
                    "pause\n\t"
                    "jmp 0b\n\t"
                    "1:\n\t"
                    "lock btsq $0, %0\n\t"
                    "jc 0b\n\t"
                    : "+m" (*lock) : : "memory", "cc");
#endif
}

static inline void bomp_unlock(bomp_lock_t *lock)
{
    *lock = 0;
}

static inline void bomp_lock_init(bomp_lock_t *lock)
{
    /* nop */
}

struct bomp_barrier
{
    unsigned max;
    volatile unsigned cycle;
    volatile unsigned counter;
};

static inline void bomp_barrier_init(struct bomp_barrier *barrier,
                                     int count)
{
    barrier->max = count;
    barrier->cycle = 0;
    barrier->counter = 0;
}

static inline void bomp_clear_barrier(struct bomp_barrier *barrier)
{
    /* nop */
}

static inline void bomp_barrier_wait(struct bomp_barrier *barrier)
{
    int cycle = barrier->cycle;
    if (__sync_fetch_and_add(&barrier->counter, 1) == barrier->max - 1) {
        barrier->counter = 0;
        barrier->cycle = !barrier->cycle;
    } else {
        uint64_t waitcnt = 0;

        while (cycle == barrier->cycle) {
            if (waitcnt == 0x400) {
                waitcnt = 0;
                thread_yield();
            }
            waitcnt++;
        }
    }
}

#endif /* BOMP_SPIN_H */
