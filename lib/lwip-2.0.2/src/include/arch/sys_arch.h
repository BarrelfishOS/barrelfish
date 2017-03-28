/**
 * \file sys_arch.h
 * \brief 
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_LWIP_2_0_2_SRC_INCLUDE_ARCH_SYS_ARCH_H_
#define LIB_LWIP_2_0_2_SRC_INCLUDE_ARCH_SYS_ARCH_H_ 1

#include <stdbool.h>
#include <assert.h>
#include <barrelfish/thread_sync.h>

/// Protection level
typedef u8_t    sys_prot_t;

//typedef struct thread_wrapper *sys_thread_t;

typedef struct thread_sem *sys_sem_t;
typedef struct thread_mutex sys_mutex_t;
typedef struct tread *sys_thread_t;

typedef void * sys_mbox_t;

#if 0
struct bf_sys_mbox {
    void *msg;
    bool empty;
    struct thread_mutex mutex;
    struct thread_cond changed_cond;
};
typedef struct bf_sys_mbox * sys_mbox_t;


#define SYS_MBOX_NULL   0
#define SYS_SEM_NULL    0

#endif

#endif /* LIB_LWIP_2_0_2_SRC_INCLUDE_ARCH_SYS_ARCH_H_ */
