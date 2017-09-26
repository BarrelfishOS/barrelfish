/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef BARRELFISH_DOMAIN_H
#define BARRELFISH_DOMAIN_H

#include <sys/cdefs.h>
#include <barrelfish/event_queue.h>
#include <barrelfish/threads.h>

__BEGIN_DECLS

typedef void (*domain_spanned_callback_t)(void *arg, errval_t err);

///< Struct for spanning domains state machine
struct span_domain_state {
    struct thread *thread;              ///< Thread to run on remote core
    uint8_t core_id;                    ///< Id of the remote core
    errval_t err;                       ///< To propagate error value
    domain_spanned_callback_t callback; ///< Callback for when domain has spanned
    void *callback_arg;                 ///< Optional argument to pass with callback
    struct capref frame;                ///< Dispatcher frame
    struct capref vroot;                ///< VRoot cap
    struct event_queue_node event_qnode;       ///< Event queue node
    struct waitset_chanstate initev;    ///< Dispatcher initialized event
    bool initialized;                   ///< True if remote initialized
};

struct mem_binding;
struct octopus_binding;
struct monitor_binding;
struct monitor_blocking_binding;
struct waitset;
struct spawn_binding;
struct arrakis_binding;
struct proc_mgmt_binding;

struct waitset *get_default_waitset(void);
void disp_set_core_id(coreid_t core_id);
coreid_t disp_get_core_id(void);
coreid_t disp_get_current_core_id(void);
void disp_get_eh_frame(lvaddr_t *eh_frame, size_t *eh_frame_size);
void disp_get_eh_frame_hdr(lvaddr_t *eh_frame_hdr, size_t *eh_frame_hdr_size);
domainid_t disp_get_domain_id(void);
coreid_t disp_handle_get_core_id(dispatcher_handle_t handle);
void set_monitor_binding(struct monitor_binding *b);
struct monitor_binding *get_monitor_binding(void);
struct waitset_chanstate *get_monitor_binding_chanstate(void);
void set_monitor_blocking_binding(struct monitor_blocking_binding *st);
struct monitor_blocking_binding *get_monitor_blocking_binding(void);
void set_mem_client(struct mem_binding *st);
struct mem_binding *get_mem_client(void);
struct pinned_state *get_current_pinned_state(void);
struct vspace *get_current_vspace(void);
struct pmap *get_current_pmap(void);
struct morecore_state *get_morecore_state(void);
struct ram_alloc_state *get_ram_alloc_state(void);
void set_octopus_binding(struct octopus_binding *st);
struct octopus_binding *get_octopus_binding(void);
void set_spawn_binding(coreid_t core, struct spawn_binding *st);
void set_arrakis_binding(coreid_t core, struct arrakis_binding *st);
struct spawn_binding *get_spawn_binding(coreid_t core);
struct arrakis_binding *get_arrakis_binding(coreid_t core);
struct terminal_state *get_terminal_state(void);
void set_terminal_state(struct terminal_state *st);
struct domain_state *get_domain_state(void);
void set_domain_state(struct domain_state *st);
struct spawn_state *get_spawn_state(void);
void set_spawn_state(struct spawn_state *st);
struct slot_alloc_state *get_slot_alloc_state(void);
struct skb_state *get_skb_state(void);
struct proc_mgmt_binding *get_proc_mgmt_binding(void);
void set_proc_mgmt_binding(struct proc_mgmt_binding *st);

errval_t domain_init(void);
errval_t domain_new_dispatcher(coreid_t core_id,
                               domain_spanned_callback_t callback,
                               void *callback_arg);
errval_t domain_new_dispatcher_setup_only(coreid_t core_id,
                                          struct span_domain_state **ret_state);
errval_t domain_thread_create_on(coreid_t core_id, thread_func_t start_func,
                                 void *arg, struct thread **newthread);
errval_t domain_thread_create_on_varstack(coreid_t core_id,
                                          thread_func_t start_func,
                                          void *arg, size_t stacksize,
                                          struct thread **newthread);
errval_t domain_thread_join(struct thread *thread, int *retval);
errval_t domain_send_cap(coreid_t core_id, struct capref cap);
errval_t domain_wakeup_on(dispatcher_handle_t disp, struct thread *thread);
errval_t domain_wakeup_on_disabled(dispatcher_handle_t disp,
                                   struct thread *thread,
                                   dispatcher_handle_t mydisp);
errval_t domain_thread_move_to(struct thread *thread, coreid_t core_id);
errval_t domain_cap_hash(struct capref domain_cap, uint64_t *ret_hash);

__END_DECLS

#endif
