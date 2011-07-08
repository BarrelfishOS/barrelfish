/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_SYSCALL_H
#define KERNEL_SYSCALL_H

#include <kernel.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <capabilities.h>

errval_t sys_print(const char *str, size_t length);
struct sysret sys_yield(caddr_t target);
struct sysret
sys_dispatcher_setup(struct capability *to, caddr_t cptr, int depth,
                     caddr_t vptr, caddr_t dptr, bool run, caddr_t odptr);
struct sysret
sys_dispatcher_properties(struct capability *to,
                          enum task_type type, unsigned long deadline,
                          unsigned long wcet, unsigned long period,
                          unsigned long release, unsigned short weight);
struct sysret
sys_retype(struct capability *root, caddr_t source_cptr, enum objtype type,
           uint8_t objbits, caddr_t dest_cnode_cptr, cslot_t dest_slot,
           uint8_t dest_vbits, bool from_monitor);
struct sysret
sys_copy_or_mint(struct capability *root, caddr_t destcn_cptr, cslot_t dest_slot,
                 caddr_t source_cptr, int destcn_vbits, int source_vbits,
                 uintptr_t param1, uintptr_t param2, bool mint);
struct sysret sys_delete(struct capability *root, caddr_t cptr, uint8_t bits,
                         bool from_monitor);
struct sysret sys_revoke(struct capability *root, caddr_t cptr, uint8_t bits,
                         bool from_monitor);
struct sysret sys_monitor_register(caddr_t ep_caddr);
struct sysret sys_monitor_identify_cap(struct capability *root,
                                       caddr_t cptr, uint8_t bits,
                                       struct capability *retbuf);
struct sysret sys_monitor_nullify_cap(caddr_t cptr, uint8_t bits);
struct sysret
sys_dispatcher_setup_guest (struct capability *to,
                            caddr_t epp, caddr_t vnodep,
                            caddr_t vmcbp, caddr_t ctrlp);
struct sysret sys_monitor_domain_id(caddr_t cptr, domainid_t domain_id);
struct sysret sys_trace_setup(struct capability *cap, caddr_t cptr);

#endif
