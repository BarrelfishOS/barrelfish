/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef COREBOOT_H
#define COREBOOT_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/types.h>

#include <elf/elf.h>
#include <bench/bench.h>
#include <vfs/vfs.h>
#include <octopus/octopus.h>
#include <octopus/capability_storage.h>
#include <spawndomain/spawndomain.h>

#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include <if/intermon_defs.h>

#if defined(__x86__) && !defined(__k1om__)
#include <acpi_client/acpi_client.h>
#endif

#define DEBUG_CPUBOOT 0
#ifdef DEBUG_CPUBOOT
#define DEBUG(x...) if (debug_flag) debug_printf(x)
#else
#define DEBUG(x...) ((void)0)
#endif

typedef coreid_t archid_t;
typedef coreid_t kcbid_t;

// Globals
extern bool benchmark_flag;
extern bool debug_flag;
extern bool new_kcb_flag;

struct elf_allocate_state {
    void *vbase;
    genvaddr_t elfbase;
};

struct bench_data {
    uint64_t load;
    uint64_t alloc_cpu;
    uint64_t alloc_mon;
    uint64_t elf_load;
    uint64_t elf_reloc;
};
struct bench_data *bench_data;

// common.c
void boot_core_reply(struct monitor_binding *st, errval_t msgerr);
errval_t create_or_get_kcb_cap(coreid_t coreid, struct capref* kcb);
errval_t give_kcb_to_new_core(coreid_t destination_id, struct capref new_kcb);
errval_t frame_alloc_identify(struct capref *dest, size_t bytes,
                              size_t *retbytes, struct frame_identity *id);
errval_t lookup_module(const char *module_name, lvaddr_t *binary_virt,
                       genpaddr_t *binary_phys, size_t *binary_size);
errval_t cap_mark_remote(struct capref cap);
errval_t elfload_allocate(void *state, genvaddr_t base,
                          size_t size, uint32_t flags,
                          void **retbase);
char* get_binary_path(char* fmt, char* binary_name);


// platform specific code {x86boot.c, armboot.c}
errval_t get_architecture_config(enum cpu_type type,
                                 genpaddr_t *arch_page_size,
                                 const char **monitor_binary,
                                 const char **cpu_binary);
errval_t spawn_xcore_monitor(coreid_t coreid, int hwid,
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb);
errval_t get_core_info(coreid_t core_id, archid_t* apic_id, enum cpu_type* cpu_type);
errval_t invoke_monitor_cap_remote(capaddr_t cap, int bits, bool is_remote,
                                   bool *has_descendents);


#endif // COREBOOT_H
