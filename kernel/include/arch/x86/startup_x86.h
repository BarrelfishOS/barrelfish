/**
 * \file
 * \brief Startup prototypes.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __STARTUP_X86_H
#define __STARTUP_X86_H

#include <startup.h>
#include <arch/x86/start_aps.h>

#define BOOTINFO_BASE           ((lvaddr_t)0x200000)
#define ARGS_BASE               (BOOTINFO_BASE + BOOTINFO_SIZE)
#define DISPATCHER_BASE         (ARGS_BASE + ARGS_SIZE)
#define MON_URPC_BASE           (DISPATCHER_BASE + DISPATCHER_SIZE)

#ifdef __scc__
extern int kernel_scckernel;
#endif

errval_t startup_map_init(lvaddr_t vbase, lpaddr_t base, size_t size,
                          uint32_t flags);
errval_t startup_alloc_init(void *state, genvaddr_t gvbase, size_t size,
                            uint32_t flags, void **ret);
void create_module_caps(struct spawn_state *st);
void cleanup_bios_regions(char *mmap_addr, char **new_mmap_addr,
                          uint32_t *new_mmap_length);

struct dcb *spawn_bsp_init(const char *name, alloc_phys_func alloc_phys);
struct dcb *spawn_app_init(struct x86_core_data *core_data,
                           const char *name, alloc_phys_func alloc_phys);

extern struct x86_core_data *glbl_core_data; // XXX: Arch specific

// global pointers used in init_ap.S
extern uint64_t x86_64_start_ap;
extern uint64_t x86_64_init_ap_wait;
extern uint64_t x86_32_start_ap;
extern uint64_t x86_32_init_ap_wait;

static inline void start_ap_signal(void)
{

    //pointer to a variable used as pseudo-lock to synchronize the BSP
    //and the AP which gets enabled
#if defined(__k1om__) || defined(__x86_64__)
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t)&x86_64_init_ap_wait - ((lpaddr_t)&x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);
#elif defined (__i386__)
#       if !defined(__scc__)
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t)&x86_32_init_ap_wait - ((lpaddr_t)&x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);
#       endif
#else
#error "Architecture not supported"
#endif

    *ap_wait = AP_STARTED;
}

void configure_page_attribute_table(void);

#endif // __STARTUP_X86_H
