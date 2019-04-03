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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
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
sys_dispatcher_setup(struct capability *to, capaddr_t cptr, uint8_t level,
                     capaddr_t vptr, capaddr_t dptr, bool run, capaddr_t odptr);
struct sysret
sys_dispatcher_properties(struct capability *to,
                          enum task_type type, unsigned long deadline,
                          unsigned long wcet, unsigned long period,
                          unsigned long release, unsigned short weight);
struct sysret
sys_retype(struct capability *root, capaddr_t source_croot, capaddr_t source_cptr,
           gensize_t offset, enum objtype type, gensize_t objsize, size_t count,
           capaddr_t dest_cspace_ptr, capaddr_t dest_cnode_cptr,
           uint8_t dest_level, cslot_t dest_slot,  bool from_monitor);
struct sysret sys_create(struct capability *root, enum objtype type,
                         size_t objsize, capaddr_t dest_cnode_cptr,
                         uint8_t dest_level, cslot_t dest_slot);
struct sysret
sys_map(struct capability *ptable, cslot_t slot, capaddr_t source_root_cptr,
        capaddr_t source_cptr, uint8_t source_level, uintptr_t flags,
        uintptr_t offset, uintptr_t pte_count, capaddr_t mapping_crootptr,
        capaddr_t mapping_cnptr, uint8_t mapping_cn_level, cslot_t mapping_slot);
struct sysret
sys_copy_remap(struct capability *ptable, cslot_t slot, capaddr_t source_cptr,
               int source_level, uintptr_t flags, uintptr_t offset,
               uintptr_t pte_count, capaddr_t mapping_cnptr,
               uint8_t mapping_cn_level, cslot_t mapping_slot);
struct sysret
sys_copy_or_mint(struct capability *root, capaddr_t dest_cspace_cptr,
                 capaddr_t destcn_cptr, cslot_t dest_slot, capaddr_t
                 source_croot_ptr, capaddr_t source_cptr,
                 uint8_t destcn_level, uint8_t source_level,
                 uintptr_t param1, uintptr_t param2, bool mint);
struct sysret sys_delete(struct capability *root, capaddr_t cptr, uint8_t level);
struct sysret sys_revoke(struct capability *root, capaddr_t cptr, uint8_t level);
struct sysret sys_get_state(struct capability *root, capaddr_t cptr, uint8_t level);
struct sysret sys_get_size_l1cnode(struct capability *root);
struct sysret sys_resize_l1cnode(struct capability *root, capaddr_t newroot_cptr,
                                 capaddr_t retcn_cptr, cslot_t retslot);
struct sysret sys_identify_cap(struct capability *root, capaddr_t cptr,
                               uint8_t level, struct capability *out);
struct sysret
sys_dispatcher_setup_guest (struct capability *to,
                            capaddr_t epp, capaddr_t vnodep,
                            capaddr_t vmcbp, capaddr_t ctrlp);
struct sysret sys_trace_setup(struct capability *cap, capaddr_t cptr);
struct sysret sys_idcap_identify(struct capability *cap, idcap_id_t *id);
struct sysret sys_monitor_spawn_core(hwid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry, genpaddr_t context_id);

struct sysret sys_kernel_add_kcb(struct kcb* new_kcb);
struct sysret sys_kernel_remove_kcb(struct kcb* kcb_addr);
struct sysret sys_kernel_suspend_kcb_sched(bool toggle);
struct sysret sys_handle_kcb_identify(struct capability* to, struct frame_identity *fi);

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
                                        capaddr_t croot_cptr,
                                        capaddr_t cnode_cptr,
                                        uint8_t cnode_level,
                                        cslot_t slot);

/*
 * Monitor syscall for retype
 */
struct sysret sys_monitor_is_retypeable(struct capability *source, gensize_t offset,
                                        gensize_t objsize, size_t count);

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
struct sysret sys_monitor_reclaim_ram(capaddr_t retcn_addr,
                                      uint8_t retcn_level,
                                      cslot_t ret_slot);
#endif
