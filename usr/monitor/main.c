/**
 * \file
 * \brief Monitor process
 */

/*
 * Copyright (c) 2007 - 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <barrelfish_kpi/init.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/deferred.h>
#include <barrelfish/domain.h>
#include <barrelfish/spawn_client.h>
#include <trace/trace.h>

#ifdef __k1om__
extern char **environ;
#endif

/* irefs for mem server, name service, ramfs and spawnd*/
iref_t mem_serv_iref = 0;
iref_t ramfs_serv_iref = 0;
iref_t name_serv_iref = 0;
iref_t spawn_iref = 0;
iref_t monitor_rpc_iref = 0;

// Capref to trace cap
struct capref trace_cap;

/* Set to the core id so no need to load from dispatcher */
coreid_t my_core_id = -1;

/* Flag to indicate if monitor is running on bsp core */
bool bsp_monitor = false;

// Flag to indicate whether to update the ram_alloc binding
bool update_ram_alloc_binding = false;

struct bootinfo *bi;

/**
 * \brief Initialize monitor running on bsp core
 */
static errval_t boot_bsp_core(int argc, char *argv[])
{
    errval_t err;

    // First argument contains the bootinfo location
    bi = (struct bootinfo*)strtol(argv[1], NULL, 10);

    bsp_monitor = true;

    err = monitor_client_setup_mem_serv();
    assert(err_is_ok(err));

    /* Wait for mem_serv to advertise its iref to us */
    while (mem_serv_iref == 0) {
        messages_wait_and_handle_next();
    }
    update_ram_alloc_binding = false;

    /* Can now connect to and use mem_serv */
    err = ram_alloc_set(NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    // Export ram_alloc service
    err = mon_ram_alloc_serve();
    assert(err_is_ok(err));

    /* Set up monitor rpc channel */
    err = monitor_rpc_init();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor rpc init failed");
        return err;
    }

    /* SKB needs vfs for ECLiPSe so we need to start ramfsd first... */
    err = spawn_domain("ramfsd");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawning ramfsd");
        return err;
    }
    // XXX: Wait for ramfsd to initialize
    while (ramfs_serv_iref == 0) {
        messages_wait_and_handle_next();
    }

    /* Spawn skb (new nameserver) before other domains */
    err = spawn_domain("skb");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawning skb");
        return err;
    }
    // XXX: Wait for name_server to initialize
    while (name_serv_iref == 0) {
        messages_wait_and_handle_next();
    }
#ifdef __k1om__
    char args[40];
    snprintf(args, sizeof(args), "0x%016lx 0x%02x", bi->host_msg,
             bi->host_msg_bits);
    char *mgr_argv[MAX_CMDLINE_ARGS + 1];
    spawn_tokenize_cmdargs(args, mgr_argv, ARRAY_LENGTH(mgr_argv));
    err = spawn_domain_with_args("xeon_phi", mgr_argv,environ);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawning xeon_phi");
        return err;
    }
#endif

    // Connect to octopus
    err = octopus_client_bind();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "binding to octopus service");
        return err;
    }

    // Store BSP KCB cap in octopus
    debug_printf("Storing BSP KCB in octopus\n");
    err = octopus_set_bspkcb();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "storing BSP KCB in octopus");
        return err;
    }

    // Spawn process manager, to be used by further domains.
    set_proc_mgmt_binding(NULL);
    err = spawn_domain("proc_mgmt");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawning proc_mgmt");
        return err;
    }
    // XXX: Wait for proc_mgmt to initialize
    while (get_proc_mgmt_binding() == NULL) {
        messages_wait_and_handle_next();
    }

    /* Spawn boot domains in menu.lst */
    err = spawn_all_domains();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "spawn_all_domains failed");
        return err;
    }

    // XXX: Wait for spawnd to initialize
    while (spawn_iref == 0) {
        messages_wait_and_handle_next();
    }
    // Now tell process manager about our new spawnd.
    err = proc_mgmt_add_spawnd(spawn_iref, disp_get_core_id());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending spawnd iref to process manager");
    }

    return SYS_ERR_OK;
}

static errval_t boot_app_core(int argc, char *argv[])
{
    coreid_t parent_core_id;
    struct intermon_binding *intermon_binding;
    errval_t err;

    /* Create the self endpoint as the kernel doesn't do it */
    err = cap_retype(cap_selfep, cap_dispatcher, 0, ObjType_EndPoint, 0, 1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Retyping dispatcher to self ep failed");
        return err;
    }

    err = boot_arch_app_core(argc, argv, &parent_core_id, &intermon_binding);
    if(err_is_fail(err)) {
        return err;
    }

    // connect it to our request handlers
    intermon_init(intermon_binding, parent_core_id);

    /* Request memserv and nameserv iref */
    err = request_mem_serv_iref(intermon_binding);
    assert(err_is_ok(err));
    err = request_name_serv_iref(intermon_binding);
    assert(err_is_ok(err));

    err = request_ramfs_serv_iref(intermon_binding);
    assert(err_is_ok(err));


#ifdef BARRELFISH_MULTIHOP_CHAN_H
    // request my part of the routing table
    err = multihop_request_routing_table(intermon_binding);
    assert(err_is_ok(err));
#endif // BARRELFISH_MULTIHOP_CHAN_H

    /* initialize self ram alloc */
    err = mon_ram_alloc_init(parent_core_id, intermon_binding);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    /* with memory alloc running, take part in cap ops */
    DEBUG_CAPOPS("sending capops_ready to %"PRIuCOREID"\n", parent_core_id);
    err = intermon_binding->tx_vtbl.capops_ready(intermon_binding, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    ((struct intermon_state*)intermon_binding->st)->capops_ready = true;

    /* Set up monitor rpc channel */
    err = monitor_rpc_init();
    if(err_is_fail(err)) {
        return err;
    }

#ifdef TRACING_EXISTS
    // Request trace caps
    err = request_trace_caps(intermon_binding);
    assert(err_is_ok(err));
#endif

    // Connect to octopus
    err = octopus_client_bind();
    assert(err_is_ok(err));

    // Spawn local spawnd
    err = spawn_spawnd(intermon_binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error spawning spawnd");
    }
    // XXX: Wait for spawnd to initialize
    while (spawn_iref == 0) {
        messages_wait_and_handle_next();
    }
    // Use monitor.0 to tell the process manager about our new spawnd.
    struct intermon_binding *ib0;
    err = intermon_binding_get(0, &ib0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error retrieving intermon_binding for monitor 0");
    }
    err = ib0->tx_vtbl.add_spawnd(ib0, NOP_CONT, spawn_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending add_spawnd request to monitor 0");
    }

    /* Signal the monitor that booted us that we have initialized */
    err = intermon_binding->tx_vtbl.monitor_initialized(intermon_binding, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }

    return SYS_ERR_OK;
}

errval_t request_trace_caps(struct intermon_binding *st)
{
    errval_t err = st->tx_vtbl.trace_caps_request(st, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    while(capref_is_null(trace_cap)) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

errval_t request_mem_serv_iref(struct intermon_binding *st)
{
    errval_t err = st->tx_vtbl.mem_serv_iref_request(st, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    while(mem_serv_iref == 0) {
        messages_wait_and_handle_next();
    }
    return SYS_ERR_OK;
}

errval_t request_name_serv_iref(struct intermon_binding *st)
{
    errval_t err = st->tx_vtbl.name_serv_iref_request(st, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    while(name_serv_iref == 0) {
        messages_wait_and_handle_next();
    }
    return SYS_ERR_OK;
}

errval_t request_ramfs_serv_iref(struct intermon_binding *st)
{
    errval_t err = st->tx_vtbl.ramfs_serv_iref_request(st, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    while(ramfs_serv_iref == 0) {
        messages_wait_and_handle_next();
    }
    return SYS_ERR_OK;
}


void ipi_test(void);

#ifdef MONITOR_HEARTBEAT
static void mon_heartbeat(void *arg) {
    assert(arg != NULL);
    errval_t err;
    debug_printf("my_core_id = %d; curr_core_id = %d\n", my_core_id,
            disp_get_current_core_id());
    struct deferred_event *ev = arg;
    deferred_event_init(ev);
    // 1 s == 1000 ms == 1e6 us
    delayus_t one_sec = 15ULL*1000*1000;
    err = deferred_event_register(ev, get_default_waitset(), one_sec, MKCLOSURE(mon_heartbeat, arg));
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "heartbeat");
    }
    assert(err_is_ok(err));
}
#endif

/**
 * \brief Use cmdline args to figure out which core the monitor is running on
 * and which cores to boot.
 */
int main(int argc, char *argv[])
{
    printf("monitor: invoked as:");
    for (int i = 0; i < argc; i++) {
        printf(" %s", argv[i]);
    }
    printf("\n");

    errval_t err;

    /* Initialize the library */
    bench_init();

    /* Set core id */
    err = invoke_kernel_get_core_id(cap_kernel, &my_core_id);
    assert(err_is_ok(err));
    disp_set_core_id(my_core_id);

    // Setup all channels and channel support code
    err = monitor_client_setup_monitor();
    assert(err_is_ok(err));

    if (argc == 2) { /* Bsp monitor */
        err = boot_bsp_core(argc, argv);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to boot BSP core");
            return EXIT_FAILURE;
        }
    } else { /* Non bsp monitor */
        err = boot_app_core(argc, argv);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "starting app monitor");
            return EXIT_FAILURE;
        }
    }

#if defined(TRACING_EXISTS) && defined(CONFIG_TRACE)
    err = trace_my_setup();
    assert(err_is_ok(err));
    trace_reset_buffer();

    struct capref tracecap;
    err = trace_setup_on_core(&tracecap);
    if (err_is_fail(err)) {
        if(err_no(err) != TRACE_ERR_NO_BUFFER) {
            DEBUG_ERR(err, "trace_setup_on_core failed");
            printf("Warning: tracing not available on core %d\n", my_core_id);
        }
    } else {
        err = invoke_trace_setup(tracecap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "invoke_trace_setup failed");
            printf("Warning: tracing not available on core %d\n", my_core_id);
        }
    }
#endif // tracing

    domain_mgmt_init();

#ifdef MONITOR_HEARTBEAT
    struct deferred_event ev;
    mon_heartbeat(&ev);
#endif

    for(;;) {
        err = event_dispatch(get_default_waitset());
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "event_dispatch");
        }

        if(update_ram_alloc_binding) {
            update_ram_alloc_binding = false;

            err = ram_alloc_set(NULL);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "ram_alloc_set to local allocator failed. "
                          "Will stick with intermon memory allocation.");
            }
        }
    }
}
