/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_H
#define MONITOR_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <bench/bench_arch.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_defs.h>
#include <if/monitor_mem_defs.h>
#include <monitor_invocations_arch.h>
#include <queue.h>
#include <connection.h>
#include "monitor_debug.h"

STATIC_ASSERT(MON_URPC_SIZE == 2*BASE_PAGE_SIZE,
              "Change #URPC_SIZE if changing channel length");
#define MON_URPC_CHANNEL_LEN  (32 * UMP_MSG_BYTES)
#define MON_RAM_CHANNEL_LEN   (2  * UMP_MSG_BYTES)

// XXX: These should match the aliases in intermon.if
typedef uint64_t state_id_t;
typedef uint64_t mon_id_t;
typedef uint64_t con_id_t;
typedef uint32_t chanid_t;
typedef uint8_t  bool_t;

// XXX: from old routing library, to be removed
typedef uint32_t recordid_t;

//XXX used to wait until all monitors are up and connected. asq
extern int seen_connections;

struct intermon_state {
    struct msg_queue queue;             ///< Queue of outgoing messages
    struct intermon_binding *binding;   ///< Back-pointer to binding
    coreid_t core_id;                   ///< Core ID of monitor on other end
    rsrcid_t rsrcid;
    bool rsrcid_inflight;
    bool capops_ready;
    struct monitor_binding *originating_client;
};

struct monitor_state {
    struct msg_queue queue;
};

extern iref_t mem_serv_iref;
extern iref_t name_serv_iref;
extern iref_t ramfs_serv_iref;
extern iref_t monitor_rpc_iref;
extern iref_t monitor_mem_iref;
extern coreid_t my_core_id;
extern bool bsp_monitor;
extern struct capref trace_cap;
extern struct bootinfo *bi;
extern bool update_ram_alloc_binding;

union capability_caprep_u {
    intermon_caprep_t caprep;
    monitor_mem_caprep_t caprep2;
    monitor_blocking_caprep_t caprepb; // XXX: identical to intermon_caprep_t
    struct capability cap;
};
STATIC_ASSERT(sizeof(union capability_caprep_u) >= sizeof(struct capability), \
                  ASSERT_CONCAT("Size mismatch:", intermon_caprep_t));

STATIC_ASSERT(sizeof(struct capability) <= sizeof(intermon_caprep_t),
        ASSERT_CONCAT("Size mismatch:", intermon_caprep_t));

static inline void capability_to_caprep(struct capability *cap,
                                        intermon_caprep_t *caprep)
{
    memcpy(caprep, cap, sizeof(*cap));
}

static inline void caprep_to_capability(intermon_caprep_t *caprep,
                                        struct capability *cap)
{
    memcpy(cap, caprep, sizeof(*cap));
}

static inline void debug_print_caprep(intermon_caprep_t *caprep)
{
    struct capability cap;
    memcpy(&cap, caprep, sizeof(cap));
    char buf[256];
    debug_print_cap(buf, 256, &cap);
    buf[255] = 0;
    DEBUG_CAPOPS("\t%s\n", buf);
}

static inline void debug_print_caprep2(monitor_mem_caprep_t *caprep)
{
    struct capability cap;
    memcpy(&cap, caprep, sizeof(cap));
    char buf[256];
    debug_print_cap(buf, 256, &cap);
    buf[255] = 0;
    DEBUG_CAPOPS("\t%s\n", buf);
}

#include <ram_alloc.h>
#include <spawn.h>
#include <monitor_server.h>
#include <monitor_invocations.h>

/* boot.c */
void boot_core_request(struct monitor_binding *st, coreid_t id,
                       struct capref frame);
void boot_initialize_request(struct monitor_binding *st);

errval_t spawn_xcore_monitor(coreid_t id, int hwid, enum cpu_type cpu_type,
                             const char *cmdline,
                             struct intermon_binding **ret_binding);
errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding);

/* main.c */
errval_t request_trace_caps(struct intermon_binding *st);
errval_t request_mem_serv_iref(struct intermon_binding *st);
errval_t request_name_serv_iref(struct intermon_binding *st);
errval_t request_ramfs_serv_iref(struct intermon_binding *st);

/* inter.c */
errval_t intermon_init(struct intermon_binding *b, coreid_t coreid);
errval_t arch_intermon_init(struct intermon_binding *b);

/* ump_support.c */
errval_t ump_intermon_init(struct intermon_binding *ib);
errval_t ump_monitor_init(struct monitor_binding *mb);

/* multihop_support.c */
errval_t multihop_intermon_init(struct intermon_binding *ib);
errval_t multihop_monitor_init(struct monitor_binding *mb);
errval_t multihop_request_routing_table(struct intermon_binding *b);

/* trace_support.c */
errval_t trace_intermon_init(struct intermon_binding *ib);
errval_t trace_monitor_init(struct monitor_binding *mb);

/* bfscope_support.c */
errval_t bfscope_intermon_init(struct intermon_binding *ib);
errval_t bfscope_monitor_init(struct monitor_binding *mb);

/* rck_support.c */
errval_t rck_intermon_init(struct intermon_binding *ib);
errval_t rck_monitor_init(struct monitor_binding *mb);

// Resource control
errval_t rsrc_new(rsrcid_t *id);
errval_t rsrc_join_satellite(rsrcid_t id, coreid_t coreid);
errval_t rsrc_join(rsrcid_t id, struct capref dispcap,
                   struct monitor_blocking_binding *b);
errval_t rsrc_submit_manifest(rsrcid_t id, char *manifest);
errval_t rsrc_set_phase(rsrcid_t id, uintptr_t phase);
errval_t rsrc_set_phase_inter(rsrcid_t id, uintptr_t phase, uint64_t timestamp);
struct monitor_blocking_binding *rsrc_get_binding(rsrcid_t id);
errval_t rsrc_set_phase_data(rsrcid_t id, uintptr_t active, void *data,
                             size_t len);

// Time coordination
errval_t timing_sync_timer(void);
void timing_sync_timer_reply(errval_t err);
void timing_sync_bench(void);

/* domain.c */
void domain_mgmt_init(void);

/* intermon_bindings.c */
errval_t intermon_binding_set(struct intermon_state *st);
errval_t intermon_binding_get(coreid_t coreid, struct intermon_binding **ret);

/* iref.c */
errval_t iref_alloc(struct monitor_binding *binding, uintptr_t service_id,
                    iref_t *iref);
errval_t iref_get_core_id(iref_t iref, coreid_t *core_id);
errval_t iref_get_binding(iref_t iref, struct monitor_binding **binding);
errval_t iref_get_service_id(iref_t iref, uintptr_t *service_id);

#endif // MONITOR_H
