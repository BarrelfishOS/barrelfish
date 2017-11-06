/** \file
 *  \brief Monitor's connection with the dispatchers on the same core for
 *  blocking rpc calls.
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include "monitor.h"
#include <barrelfish/monitor_client.h>
#include <barrelfish_kpi/platform.h>
#include "capops.h"

// workaround inlining bug with gcc 4.4.1 shipped with ubuntu 9.10 and 4.4.3 in Debian
#if defined(__i386__) && defined(__GNUC__) \
    && __GNUC__ == 4 && __GNUC_MINOR__ == 4 && __GNUC_PATCHLEVEL__ <= 3
#define SAFEINLINE __attribute__((noinline))
#else
#define SAFEINLINE
#endif

static void retype_reply_status(errval_t status, void *st)
{
    struct monitor_blocking_binding *b = (struct monitor_blocking_binding*)st;
    errval_t err = b->tx_vtbl.remote_cap_retype_response(b, NOP_CONT, status);
    assert(err_is_ok(err));
}

static void remote_cap_retype(struct monitor_blocking_binding *b,
                              struct capref src_root, struct capref dest_root,
                              capaddr_t src, uint64_t offset, uint64_t new_type,
                              uint64_t objsize, uint64_t count, capaddr_t to,
                              capaddr_t slot, int32_t to_level)
{
    if (capref_is_null(dest_root)) {
        dest_root = src_root;
    }
    capops_retype(new_type, objsize, count, dest_root, to, to_level,
                  slot, src_root, src, 2, offset, retype_reply_status, (void*)b);
}

static void delete_reply_status(errval_t status, void *st)
{
    DEBUG_CAPOPS("sending cap_delete reply msg: %s\n", err_getstring(status));
    struct monitor_blocking_binding *b = (struct monitor_blocking_binding*)st;
    errval_t err = b->tx_vtbl.remote_cap_delete_response(b, NOP_CONT, status);
    assert(err_is_ok(err));
}

static void remote_cap_delete(struct monitor_blocking_binding *b,
                              struct capref croot, capaddr_t src, uint8_t level)
{
    struct domcapref cap = { .croot = croot, .cptr = src, .level = level };
    capops_delete(cap, delete_reply_status, (void*)b);
}

static void revoke_reply_status(errval_t status, void *st)
{
    struct monitor_blocking_binding *b = (struct monitor_blocking_binding*)st;
    errval_t err = b->tx_vtbl.remote_cap_revoke_response(b, NOP_CONT, status);
    assert(err_is_ok(err));
}

static void remote_cap_revoke(struct monitor_blocking_binding *b,
                              struct capref croot, capaddr_t src, uint8_t level)
{
    struct domcapref cap = { .croot = croot, .cptr = src, .level = level };
    capops_revoke(cap, revoke_reply_status, (void*)b);
}

static void rsrc_manifest(struct monitor_blocking_binding *b,
                          struct capref dispcap, const char *str)
{
    errval_t err, err2;
    rsrcid_t id;

    err = rsrc_new(&id);
    if(err_is_fail(err)) {
        goto out;
    }
    err = rsrc_join(id, dispcap, b);
    if(err_is_fail(err)) {
        // TODO: Cleanup
        goto out;
    }
    err = rsrc_submit_manifest(id, (CONST_CAST)str);

 out:
    err2 = b->tx_vtbl.rsrc_manifest_response(b, NOP_CONT, id, err);
    assert(err_is_ok(err2));
}

static void rsrc_phase(struct monitor_blocking_binding *b,
                       rsrcid_t id, uint32_t phase)
{
    errval_t err;

    err = rsrc_set_phase(id, phase);
    assert(err_is_ok(err));

    err = b->tx_vtbl.rsrc_phase_response(b, NOP_CONT);
    assert(err_is_ok(err));
}

static void rpc_rsrc_join(struct monitor_blocking_binding *b,
                          rsrcid_t id, struct capref dispcap)
{
    errval_t err, err2;

    err = rsrc_join(id, dispcap, b);

    if(err_is_fail(err)) {
        err2 = b->tx_vtbl.rsrc_join_response(b, NOP_CONT, err);
        assert(err_is_ok(err2));
    }
}

static void alloc_monitor_ep(struct monitor_blocking_binding *b)
{
    struct capref retcap = NULL_CAP;
    errval_t err, reterr = SYS_ERR_OK;

    struct monitor_lmp_binding *lmpb =
        malloc(sizeof(struct monitor_lmp_binding));
    assert(lmpb != NULL);

    // setup our end of the binding
    err = monitor_client_lmp_accept(lmpb, get_default_waitset(),
                                    DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        free(lmpb);
        reterr = err_push(err, LIB_ERR_MONITOR_CLIENT_ACCEPT);
        goto out;
    }

    retcap = lmpb->chan.local_cap;
    monitor_server_init(&lmpb->b);

out:
    err = b->tx_vtbl.alloc_monitor_ep_response(b, NOP_CONT, reterr, retcap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send alloc_monitor_ep_reply");
    }
}

struct cap_identify_del_st {
    struct monitor_blocking_binding *b;
    struct capref cap;
    union capability_caprep_u u;
    errval_t reterr;
};

static void cap_identify_delete_result_handler(errval_t status, void *st)
{
    errval_t err;
    char *msg = NULL;
    if (err_is_fail(status) && err_no(status) != SYS_ERR_CAP_NOT_FOUND) {
        msg = "caps_delete failed";
        err = status;
        goto cleanup;
    }

    struct cap_identify_del_st *idst = st;

    // free slot
    err = slot_free(idst->cap);
    if (err_is_fail(err)) {
        msg = "slot_free failed";
        goto cleanup;
    }

    // send cap_identify reply
    err = idst->b->tx_vtbl.cap_identify_response(idst->b, NOP_CONT, idst->reterr,
            idst->u.caprepb);
    msg = "reply failed";

cleanup:
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, msg ? msg : "unknown reason?!");
    }
    free(st);
}

static void cap_identify(struct monitor_blocking_binding *b,
                         struct capref cap)
{
    // allocate delete state
    struct cap_identify_del_st *st = malloc(sizeof(*st));
    if (!st) {
        USER_PANIC("malloc in cap_identify");
    }
    st->b = b;
    st->cap = cap;
    st->reterr = monitor_cap_identify(cap, &st->u.cap);

    /* We always need to do the delete properly here as the cap might be
     * locked or in a delete already, furthermore if the function is called
     * from the monitor through it's self-client binding we still create a
     * copy of the capability, and need to cleanup our copy */
    struct domcapref dcap = get_cap_domref(cap);

    capops_delete(dcap, cap_identify_delete_result_handler, st);
}

#define ARM_IRQ_MAX 256

static void arm_irq_handle_call(struct monitor_blocking_binding *b,
        struct capref ep, uint32_t irq)
{

    errval_t err = 1;

    if (irq <= ARM_IRQ_MAX) {
        err = invoke_irqtable_set(cap_irq, irq, ep);
    }

    errval_t err2 = b->tx_vtbl.arm_irq_handle_response(b, NOP_CONT, err);
    assert(err_is_ok(err2));
}

static void irq_handle_call(struct monitor_blocking_binding *b, struct capref ep)
{
    /* allocate a new slot in the IRQ table */
    int vec;
    errval_t err, err2;
    err = invoke_irqtable_alloc_vector(cap_irq, &vec);
    if (err_is_fail(err)) {
        err = err_push(err, MON_ERR_INVOKE_IRQ_ALLOCATE);
        err2 = b->tx_vtbl.irq_handle_response(b, NOP_CONT, err, 0);
    }
    // we got a vector

    /* set it and reply */
    err = invoke_irqtable_set(cap_irq, vec, ep);
    if (err_is_fail(err)) {
        err = err_push(err, MON_ERR_INVOKE_IRQ_SET);
    }
    err2 = b->tx_vtbl.irq_handle_response(b, NOP_CONT, err, vec);
    assert(err_is_ok(err2));
}

static void get_arch_core_id(struct monitor_blocking_binding *b)
{
    static uintptr_t arch_id = -1;
    errval_t err;
//    printf("%s:%s:%d: \n", __FILE__, __FUNCTION__, __LINE__);

    if (arch_id == -1) {
        err = invoke_monitor_get_arch_id(&arch_id);
        assert(err_is_ok(err));
        assert(arch_id != -1);
    }

    err = b->tx_vtbl.get_arch_core_id_response(b, NOP_CONT, arch_id);
    assert(err_is_ok(err));
}

struct pending_reply {
    struct monitor_blocking_binding *b;
    errval_t err;
};

static void retry_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct monitor_blocking_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.cap_set_remote_response(b, NOP_CONT, r->err);
    if (err_is_ok(err)) {
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply, r));
        assert(err_is_ok(err));
    } else {
        DEBUG_ERR(err, "failed to reply to memory request");
    }
}

static void cap_set_remote(struct monitor_blocking_binding *b,
                           struct capref cap, bool remote)
{
    errval_t err, reterr;

    reterr = monitor_remote_relations(cap, RRELS_COPY_BIT, RRELS_COPY_BIT, NULL);

    err = b->tx_vtbl.cap_set_remote_response(b, NOP_CONT, reterr);
    if(err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = reterr;
            err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply, r));
            assert(err_is_ok(err));
        } else {
            USER_PANIC_ERR(err, "cap_set_remote_response");
        }
    }
}

/* ----------------------- BOOTINFO REQUEST CODE START ---------------------- */

static void get_phyaddr_cap(struct monitor_blocking_binding *b)
{
    // XXX: We should not just hand out this cap to everyone
    // who requests it. There is currently no way to determine
    // if the client is a valid recipient
    errval_t err;

    struct capref src = {
        .cnode = cnode_root,
        .slot  = ROOTCN_SLOT_PACN
    };

    err = b->tx_vtbl.get_phyaddr_cap_response(b, NOP_CONT, src,
            SYS_ERR_OK);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_phyaddr_cap, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_phyaddr_cap_response failed");
    }
}

static void get_io_cap(struct monitor_blocking_binding *b)
{
    // XXX: We should not just hand out this cap to everyone
    // who requests it. There is currently no way to determine
    // if the client is a valid recipient
    errval_t err;
    struct capref src = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };

    err = b->tx_vtbl.get_io_cap_response(b, NOP_CONT, src,
            SYS_ERR_OK);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_io_cap, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_io_cap_response failed");
    }
}

static void get_irq_dest_cap(struct monitor_blocking_binding *b)
{
    errval_t err;
    //TODO get real cap

    struct capref dest_cap;
    slot_alloc(&dest_cap);
    err = invoke_irqtable_alloc_dest_cap(cap_irq, dest_cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err,"x");
        USER_PANIC_ERR(err, "could not allocate dest cap!");
    }


    err = b->tx_vtbl.get_irq_dest_cap_response(b, NOP_CONT, dest_cap,
            SYS_ERR_OK);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_io_cap, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_io_cap_response failed");
    }
}


static void get_bootinfo(struct monitor_blocking_binding *b)
{
    errval_t err;

    struct capref frame = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_BOOTINFO
    };

    struct frame_identity id = { .base = 0, .bytes = 0 };
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    err = b->tx_vtbl.get_bootinfo_response(b, NOP_CONT, SYS_ERR_OK, frame,
                                           id.bytes);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_bootinfo, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_bootinfo_response failed");
    }
}

/* ----------------------- BOOTINFO REQUEST CODE END ----------------------- */

static void get_ipi_cap(struct monitor_blocking_binding *b)
{
    errval_t err;

    // XXX: We should not just hand out this cap to everyone
    // who requests it. There is currently no way to determine
    // if the client is a valid recipient

    err = b->tx_vtbl.get_ipi_cap_response(b, NOP_CONT, cap_ipi);
    assert(err_is_ok(err));
}

// XXX: these look suspicious in combination with distops!
static void forward_kcb_request(struct monitor_blocking_binding *b,
                                coreid_t destination, struct capref kcb)
{
    printf("%s:%s:%d: forward_kcb_request in monitor\n",
           __FILE__, __FUNCTION__, __LINE__);

    errval_t err = SYS_ERR_OK;

    struct capability kcb_cap;
    err = monitor_cap_identify(kcb, &kcb_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_identify failed");
        err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
        return;
    }

    if (destination == my_core_id) {
        uintptr_t kcb_base = (uintptr_t)kcb_cap.u.kernelcontrolblock.kcb;
        printf("%s:%s:%d: Invoke syscall directly, destination==my_core_id; kcb_base = 0x%"PRIxPTR"\n",
               __FILE__, __FUNCTION__, __LINE__, kcb_base);
        err = invoke_monitor_add_kcb(kcb_base);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "invoke_montitor_add_kcb failed.");
        }

        err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
        return;
    }

    struct intermon_binding *ib;
    err = intermon_binding_get(destination, &ib);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "intermon_binding_get failed");
        err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
        return;
    }

    intermon_caprep_t kcb_rep;
    capability_to_caprep(&kcb_cap, &kcb_rep);

    ib->st = b;
    err = ib->tx_vtbl.give_kcb_request(ib, NOP_CONT, kcb_rep);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "give_kcb send failed");
        err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
        return;
    }
}

static void forward_kcb_rm_request(struct monitor_blocking_binding *b,
                                   coreid_t destination, struct capref kcb)
{
    errval_t err = SYS_ERR_OK;

    // can't move ourselves
    assert(destination != my_core_id);

    struct capability kcb_cap;
    err = monitor_cap_identify(kcb, &kcb_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_identify failed");
        err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
        return;
    }

    struct intermon_binding *ib;
    err = intermon_binding_get(destination, &ib);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "intermon_binding_get failed");
        err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
        return;
    }
    uintptr_t kcb_base = (uintptr_t )kcb_cap.u.kernelcontrolblock.kcb;

    // send request to other monitor
    // remember monitor binding to send answer
    struct intermon_state *ist = (struct intermon_state*)ib->st;
    ist->originating_client = (struct monitor_binding*)b; //XXX: HACK
    err = ib->tx_vtbl.forward_kcb_rm_request(ib, NOP_CONT, kcb_base);
    assert(err_is_ok(err));
}

static void get_global_paddr(struct monitor_blocking_binding *b)
{
    genpaddr_t global = 0;
    errval_t err;
    err = invoke_get_global_paddr(cap_kernel, &global);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "get_global_paddr invocation");
    }

    err = b->tx_vtbl.get_global_paddr_response(b, NOP_CONT, global);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending global paddr failed.");
    }
}

static void get_platform(struct monitor_blocking_binding *b)
{
    struct platform_info pi;
    errval_t err;
    err = invoke_get_platform_info((uintptr_t)&pi);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "get_platform_info invocation");
    }

    err = b->tx_vtbl.get_platform_response(b, NOP_CONT, pi.arch, pi.platform);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending platform info failed.");
    }
}

static void get_platform_arch(struct monitor_blocking_binding *b)
{
    errval_t err;
    size_t struct_size;

    struct platform_info *pi= malloc(sizeof(struct platform_info));
    if(!pi) USER_PANIC("Failed to allocate platform info struct.\n");

    err = invoke_get_platform_info((uintptr_t)pi);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "get_platform_info invocation");
    }

    switch(pi->arch) {
        case PI_ARCH_ARMV7A:
            struct_size= sizeof(struct arch_info_armv7);
            break;
        default:
            struct_size= 0;
    }
    assert(struct_size < PI_ARCH_INFO_SIZE);

    err = b->tx_vtbl.get_platform_arch_response(b, MKCONT(free,pi),
            (uint8_t *)&pi->arch_info, struct_size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending platform info failed.");
    }
}

/*------------------------- Initialization functions -------------------------*/

static struct monitor_blocking_rx_vtbl rx_vtbl = {
    .get_bootinfo_call = get_bootinfo,
    .get_phyaddr_cap_call = get_phyaddr_cap,
    .get_io_cap_call = get_io_cap,
    .get_irq_dest_cap_call = get_irq_dest_cap,

    .remote_cap_retype_call  = remote_cap_retype,
    .remote_cap_delete_call  = remote_cap_delete,
    .remote_cap_revoke_call  = remote_cap_revoke,

    .rsrc_manifest_call      = rsrc_manifest,
    .rsrc_join_call          = rpc_rsrc_join,
    .rsrc_phase_call         = rsrc_phase,

    .alloc_monitor_ep_call   = alloc_monitor_ep,
    .cap_identify_call       = cap_identify,
    .irq_handle_call         = irq_handle_call,
    .arm_irq_handle_call     = arm_irq_handle_call,
    .get_arch_core_id_call   = get_arch_core_id,

    .cap_set_remote_call     = cap_set_remote,
    .get_ipi_cap_call = get_ipi_cap,

    .forward_kcb_request_call = forward_kcb_request,

    .forward_kcb_rm_request_call = forward_kcb_rm_request,

    .get_global_paddr_call = get_global_paddr,

    .get_platform_call = get_platform,
    .get_platform_arch_call = get_platform_arch,
};

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    set_monitor_rpc_iref(iref);
}

static errval_t connect_callback(void *st, struct monitor_blocking_binding *b)
{
    b->rx_vtbl = rx_vtbl;

    // TODO: set error handler
    return SYS_ERR_OK;
}

errval_t monitor_rpc_init(void)
{
    static struct monitor_blocking_export e = {
        .connect_cb = connect_callback,
        .common = {
            .export_callback = export_callback,
            .flags = IDC_EXPORT_FLAGS_DEFAULT,
            .connect_cb_st = &e,
            .lmp_connect_callback = monitor_blocking_lmp_connect_handler,
        }
    };

    e.waitset = get_default_waitset();

    return idc_export_service(&e.common);
}
