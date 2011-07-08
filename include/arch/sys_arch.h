/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SYS_ARCH_H
#define SYS_ARCH_H

/// Protection level
typedef u8_t    sys_prot_t;

// XXX: Still need to be implemented
typedef u8_t sys_sem_t;
typedef u8_t sys_mbox_t;
struct sys_timeo {u8_t dummy;};

// XXX: Still need to be implemented
#define sys_init()
#define sys_timeout(m,h,a)
#define sys_untimeout(m,a)
#define sys_sem_new(c) c
#define sys_sem_signal(s)
#define sys_sem_wait(s)
#define sys_sem_wait_timeout(s,t)
#define sys_arch_sem_wait(s,t)
#define sys_sem_free(s)
#define sys_mbox_new(s) 0
#define sys_mbox_fetch(m,d)
#define sys_mbox_tryfetch(m,d)
#define sys_mbox_post(m,d)
#define sys_mbox_trypost(m,d)
#define sys_mbox_free(m)
#define sys_thread_new(n,t,a,s,p)

#endif
