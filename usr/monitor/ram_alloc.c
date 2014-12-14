/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include "capops.h"
#include <if/monitor_mem_defs.h>
#include <if/monitor_mem_rpcclient_defs.h>
#include <if/mem_rpcclient_defs.h>

static uint8_t mem_core_id;
static struct monitor_mem_rpc_client monitor_mem_client;
static bool mem_setup_complete = false;
iref_t monitor_mem_iref = 0;

static void mem_alloc_delete_result_handler(errval_t status, void *st)
{
    struct capref *cap = (struct capref*)st;
    errval_t err;

    if (err_is_fail(status)) {
        DEBUG_ERR(status, "capops_delete failed, cap will leak");
    }
    else {
        err = slot_free(*cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "slot_free failed, cap will leak");
        }
    }

    free(cap);
}

/**
 * \brief Request for some memory (over the memory allocation channel)
 */
static void mem_alloc_handler(struct monitor_mem_binding *b,
                              uint8_t size_bits, genpaddr_t minbase,
                              genpaddr_t maxlimit, coreid_t from)
{
    struct capref *cap = NULL;
    monitor_mem_caprep_t caprep = {0,0,0,0};
    errval_t err;
    errval_t reterr = SYS_ERR_OK;

    /* This should only run on the core with the mem_serv. Or else the system
       will deadlock. */
    assert(bsp_monitor);

    cap = malloc(sizeof(*cap));
    if (!cap) {
        reterr = LIB_ERR_MALLOC_FAIL;
        goto out;
    }

    err = ram_alloc(cap, size_bits);
    if (err_is_fail(err)) {
        reterr = err_push(err, LIB_ERR_RAM_ALLOC);
        goto out;
    }

    struct capability cap_raw;
    err = monitor_cap_identify(*cap, &cap_raw);
    if (err_is_fail(err)) {
        reterr = err_push(err, MON_ERR_CAP_IDENTIFY);
        goto out;
    }
    assert(cap_raw.type == ObjType_RAM);
    intermon_caprep_t caprep2;
    capability_to_caprep(&cap_raw, &caprep2);
    // XXX: work around stupid flounder behaviour: these types are identical!
    STATIC_ASSERT_SIZEOF(caprep, sizeof(caprep2));
    memcpy(&caprep, &caprep2, sizeof(caprep));

    err = monitor_set_cap_owner(cap_root, get_cap_addr(*cap), get_cap_valid_bits(*cap), from);
    if (err_is_fail(err)) {
        reterr = err;
        memset(&caprep, 0, sizeof(caprep));
    }

out:
    // RPC protocol, this can never fail with TX_BUSY
    err = b->tx_vtbl.alloc_response(b, NOP_CONT, reterr, caprep);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "reply failed");
    }

    if (cap) {
        capops_delete(get_cap_domref(*cap), mem_alloc_delete_result_handler, cap);
    }
}

static void mem_free_handler(struct monitor_mem_binding *b,
                             monitor_mem_caprep_t caprep,
                             genpaddr_t base, uint8_t bits)
{
    errval_t err, result;
    // this should only run on the bsp monitor
    assert(bsp_monitor);
    DEBUG_CAPOPS("%s\n", __FUNCTION__);

    // convert caprep into cap
    intermon_caprep_t caprep2;
    // XXX: work around stupid flounder behaviour: these types are identical!
    STATIC_ASSERT_SIZEOF(caprep, sizeof(caprep2));
    memcpy(&caprep2, &caprep, sizeof(caprep));
    struct capability cap_raw;
    caprep_to_capability(&caprep2, &cap_raw);

    DEBUG_CAPOPS("%s: freeing:\n", __FUNCTION__);
    debug_print_caprep(&caprep2);

    assert(ObjType_RAM == cap_raw.type);

    struct capref cap;
    err = slot_alloc(&cap);
    if (err_is_fail(err)) {
        result = err_push(err, LIB_ERR_SLOT_ALLOC);
        goto out;
    }

    err = monitor_cap_create(cap, &cap_raw, my_core_id);
    if (err_is_fail(err)) {
        result = err_push(err, MON_ERR_CAP_CREATE);
        goto out;
    }
    DEBUG_CAPOPS("%s: created local copy, sending to memserv\n", __FUNCTION__);

    struct mem_rpc_client *mb = get_mem_client();
    assert(mb);
    err = mb->vtbl.free_monitor(mb, cap, base, bits, &result);
    if (err_is_fail(err)) {
        result = err;
    }
out:
    DEBUG_CAPOPS("%s: sending reply: %s\n", __FUNCTION__, err_getstring(result));
    err = b->tx_vtbl.free_response(b, NOP_CONT, result);
    assert(err_is_ok(err));
}

static errval_t mon_ram_alloc(struct capref *ret, uint8_t size_bits,
                              uint64_t minbase, uint64_t maxlimit)
{
    errval_t err;
    intermon_caprep_t caprep;
    errval_t reterr;

    err = monitor_mem_client.vtbl.alloc(&monitor_mem_client, size_bits,
                                        minbase, maxlimit, my_core_id, &reterr,
                                        (monitor_mem_caprep_t *) &caprep);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_RAM_ALLOC_ERR);
    } else if (err_is_fail(reterr)) {
        return err_push(reterr, MON_ERR_RAM_ALLOC_RETERR);
    }

    struct capability cap_raw;
    caprep_to_capability(&caprep, &cap_raw);
    assert(ObjType_RAM == cap_raw.type);

    err = slot_alloc(ret);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = monitor_cap_create(*ret, &cap_raw, my_core_id);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_CAP_CREATE);
    }

    return reterr;
}

static struct monitor_mem_rx_vtbl the_monitor_mem_vtable = {
    .alloc_call = mem_alloc_handler,
    .free_call = mem_free_handler,
};

static errval_t monitor_mem_connected(void *st, struct monitor_mem_binding *b)
{
    b->rx_vtbl = the_monitor_mem_vtable;
    return SYS_ERR_OK;
}

static void monitor_mem_listening(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "err in monitor_mem_listening failed");
    }
    monitor_mem_iref = iref;
}

errval_t mon_ram_alloc_serve(void)
{
    errval_t err;

    err = monitor_mem_export(NULL, monitor_mem_listening, monitor_mem_connected,
                             get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

static void bind_cont(void *st, errval_t err, struct monitor_mem_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "err in bind_cont failed");
    }

    // setup RPC client above binding
    err = monitor_mem_rpc_client_init(&monitor_mem_client, b);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "in monitor_mem_rpc_client_init");
    } else {
        mem_setup_complete = true;
    }
}

errval_t mon_ram_alloc_init(coreid_t core_id, struct intermon_binding *b)
{
#ifdef __scc__
    assert(!"Should not be calling this on SCC platform");
#endif

    errval_t err;

    /* Set memcore_id */
    mem_core_id = core_id;

    // Get service IREF from core
    err = b->tx_vtbl.monitor_mem_iref_request(b, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    while(monitor_mem_iref == 0) {
        messages_wait_and_handle_next();
    }

    // Bind to service
    err = monitor_mem_bind(monitor_mem_iref, bind_cont, NULL,
                           get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_mem_bind failed");
    }
    while(!mem_setup_complete) {
        messages_wait_and_handle_next();
    }

    err = ram_alloc_set(mon_ram_alloc);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    return SYS_ERR_OK;
}

errval_t mon_ram_free(struct capability *cap_raw, genpaddr_t base, uint8_t bits)
{
    errval_t err, status;
    // get caprep
    intermon_caprep_t caprep;
    capability_to_caprep(cap_raw, &caprep);
    // XXX: work around stupid flounder behaviour: these types are identical!
    monitor_mem_caprep_t caprep2;
    STATIC_ASSERT_SIZEOF(caprep, sizeof(caprep2));
    memcpy(&caprep2, &caprep, sizeof(caprep));

    err = monitor_mem_client.vtbl.free(&monitor_mem_client, caprep2, base, bits, &status);
    if (err_is_fail(err)) {
        return err;
    }
    return status;
}

