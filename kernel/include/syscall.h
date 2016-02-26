/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
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
#include <barrelfish_kpi/types.h>
#include <capabilities.h>

errval_t sys_print(const char *str, size_t length);
struct sysret sys_yield(capaddr_t target);
struct sysret sys_suspend(bool halt);
struct sysret
sys_dispatcher_setup(struct capability *to, capaddr_t cptr, int depth,
                     capaddr_t vptr, capaddr_t dptr, bool run, capaddr_t odptr);
struct sysret
sys_dispatcher_properties(struct capability *to,
                          enum task_type type, unsigned long deadline,
                          unsigned long wcet, unsigned long period,
                          unsigned long release, unsigned short weight);
struct sysret
sys_retype(struct capability *root, capaddr_t source_cptr, enum objtype type,
           uint8_t objbits, capaddr_t dest_cnode_cptr, cslot_t dest_slot,
           uint8_t dest_vbits, bool from_monitor);
struct sysret sys_create(struct capability *root, enum objtype type,
                         uint8_t objbits, capaddr_t dest_cnode_cptr,
                         cslot_t dest_slot, int dest_vbits);
struct sysret
sys_map(struct capability *ptable, cslot_t slot, capaddr_t source_cptr,
        int source_vbits, uintptr_t flags, uintptr_t offset,
        uintptr_t pte_count, capaddr_t mapping_cnptr, int mapping_cnvbits,
        cslot_t mapping_slot);
struct sysret
sys_copy_or_mint(struct capability *root, capaddr_t destcn_cptr, cslot_t dest_slot,
                 capaddr_t source_cptr, int destcn_vbits, int source_vbits,
                 uintptr_t param1, uintptr_t param2, bool mint);
struct sysret sys_delete(struct capability *root, capaddr_t cptr, uint8_t bits);
struct sysret sys_revoke(struct capability *root, capaddr_t cptr, uint8_t bits);
struct sysret sys_get_state(struct capability *root, capaddr_t cptr, uint8_t bits);
struct sysret
sys_dispatcher_setup_guest (struct capability *to,
                            capaddr_t epp, capaddr_t vnodep,
                            capaddr_t vmcbp, capaddr_t ctrlp);
struct sysret sys_trace_setup(struct capability *cap, capaddr_t cptr);
struct sysret sys_idcap_identify(struct capability *cap, idcap_id_t *id);
struct sysret sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry);

struct sysret sys_kernel_add_kcb(struct kcb* new_kcb);
struct sysret sys_kernel_remove_kcb(struct kcb* kcb_addr);
struct sysret sys_kernel_suspend_kcb_sched(bool toggle);
struct sysret sys_handle_kcb_identify(struct capability* to, struct frame_identity *fi);
struct sysret sys_get_absolute_time(void);

/*
 * Monitor syscalls
 */

struct sysret sys_monitor_register(capaddr_t ep_caddr);
struct sysret sys_monitor_domain_id(capaddr_t cptr, domainid_t domain_id);
struct sysret sys_monitor_remote_relations(capaddr_t root_addr, uint8_t root_bits,
                                           capaddr_t cptr, uint8_t bits,
                                           uint8_t relations, uint8_t mask);
struct sysret sys_monitor_identify_cap(struct capability *root,
                                       capaddr_t cptr, uint8_t bits,
                                       struct capability *retbuf);
struct sysret sys_monitor_nullify_cap(capaddr_t cptr, uint8_t bits);
struct sysret sys_get_cap_owner(capaddr_t root_addr, uint8_t root_bits,
                                capaddr_t cptr, uint8_t bits);
struct sysret sys_set_cap_owner(capaddr_t root_addr, uint8_t root_bits,
                                capaddr_t cptr, uint8_t bits, coreid_t owner);
struct sysret sys_cap_has_relations(capaddr_t caddr, uint8_t vbits, uint8_t mask);
struct sysret sys_lock_cap(capaddr_t root_addr, uint8_t root_bits,
                           capaddr_t cptr, uint8_t bits);
struct sysret sys_unlock_cap(capaddr_t root_addr, uint8_t root_bits,
                             capaddr_t cptr, uint8_t bits);
struct sysret sys_monitor_copy_existing(struct capability *src,
                                        capaddr_t cnode_cptr,
                                        uint8_t cnode_vbits,
                                        cslot_t slot);

/*
 * Monitor syscalls for delete & revoke
 */

struct sysret sys_monitor_delete_last(capaddr_t root_addr, uint8_t root_bits,
                                      capaddr_t target_addr, uint8_t target_bits,
                                      capaddr_t ret_cn_addr, uint8_t ret_cn_bits,
                                      cslot_t ret_slot);
struct sysret sys_monitor_delete_foreigns(capaddr_t cptr, uint8_t bits);
struct sysret sys_monitor_revoke_mark_tgt(capaddr_t root_addr,
                                          uint8_t root_bits,
                                          capaddr_t target_addr,
                                          uint8_t target_bits);
struct sysret sys_monitor_revoke_mark_rels(struct capability *base);
struct sysret sys_monitor_delete_step(capaddr_t ret_cn_addr,
                                      uint8_t ret_cn_bits,
                                      cslot_t ret_slot);
struct sysret sys_monitor_clear_step(capaddr_t ret_cn_addr,
                                     uint8_t ret_cn_bits,
                                     cslot_t ret_slot);

#endif
