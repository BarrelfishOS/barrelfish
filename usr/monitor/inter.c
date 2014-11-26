/**
 * \file
 * \brief Inter-monitor communication
 *
 * Welcome to stack-rip hell.
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <monitor.h>
#include <barrelfish/dispatch.h>
#include <trace/trace.h>
#include "send_cap.h"
#include "capops.h"
#include <trace_definitions/trace_defs.h>

#define MIN(x,y) ((x<y) ? (x) : (y))
#define MAX(x,y) ((x>y) ? (x) : (y))
static bool* notification_sent = NULL;
static bool* monitor_ready = NULL;
static errval_t new_monitor_notify(coreid_t id)
{
    if (notification_sent == NULL) {
        notification_sent = calloc(MAX_COREID*MAX_COREID, sizeof(bool));
    }

    struct intermon_binding *b;
    errval_t err;
    // XXX: this is stupid...
    // XXX: I changed this a bit to keep track of what cores are ready
    // and who has gotten a notification, this allows to boot cores
    // in parallel thus speeding up the boot process
    for (int i = 0; i < MAX_COREID; i++) {
        if (i != my_core_id && i != id) {

            coreid_t min = MIN(id, i);
            coreid_t max = MAX(id, i);

            err = intermon_binding_get(i, &b);
            if (err_is_ok(err) && !notification_sent[min*MAX_COREID+max] && monitor_ready[i]) {
                while (1) {
                    err = b->tx_vtbl.new_monitor_notify(b, NOP_CONT, id);
                    if (err_is_ok(err)) {
                        notification_sent[min*MAX_COREID+max] = true;
                        break;
                    }
                    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                        messages_wait_and_handle_next();
                    } else {
                        return err;
                    }
                }
            }
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief A newly booted monitor indicates that can participate in capability
 * operations
 */
static void capops_ready(struct intermon_binding *b)
{
    struct intermon_state *st = b->st;
    st->capops_ready = true;
}

/**
 * \brief A newly booted monitor indicates that it has initialized
 */

static void boot_core_reply_handler(struct monitor_binding *b,
                                    struct monitor_msg_queue_elem *e);

struct boot_core_reply_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_boot_core_reply__args args;
};

static void
boot_core_reply_enqueue(struct monitor_binding *domain_binding,
                            errval_t error_code)
{
    errval_t err;

    struct boot_core_reply_state *me =
        malloc(sizeof(struct boot_core_reply_state));
    assert(me != NULL);
    me->args.err = error_code;
    me->elem.cont = boot_core_reply_handler;

    struct monitor_state *st = domain_binding->st;
    err = monitor_enqueue_send(domain_binding, &st->queue,
                               get_default_waitset(), &me->elem.queue);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_enqueue_send failed");
    }
}

static void
boot_core_reply_cont(struct monitor_binding *domain_binding,
                     errval_t error_code)
{
    assert(domain_binding != NULL);
    errval_t err;
    DEBUG_CAPOPS("boot_core_reply_cont: %s (%"PRIuERRV")\n",
            err_getstring(error_code), error_code);
    err = domain_binding->tx_vtbl.
            boot_core_reply(domain_binding, NOP_CONT, error_code);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            boot_core_reply_enqueue(domain_binding, error_code);
        } else {
            USER_PANIC_ERR(err, "error delivering boot_core_reply");
        }
    }
}

static void boot_core_reply_handler(struct monitor_binding *b,
                                    struct monitor_msg_queue_elem *e)
{
    struct boot_core_reply_state *st =
        (struct boot_core_reply_state *)e;
    boot_core_reply_cont(b, st->args.err);
    free(e);
}

static void monitor_initialized(struct intermon_binding *b)
{
    if (monitor_ready == NULL) {
        monitor_ready = calloc(MAX_COREID, sizeof(bool));
    }

    struct intermon_state *st = b->st;
    errval_t err = SYS_ERR_OK;
    assert(st->capops_ready);

    // Inform other monitors of this new monitor
    monitor_ready[st->core_id] = true;
    err = new_monitor_notify(st->core_id);
    if (err_is_fail(err)) {
        err = err_push(err, MON_ERR_INTERN_NEW_MONITOR);
    }

    // New plan, do timing sync for every time a monitor has come up...
    /*if(num_monitors > 1) {
        printf("monitor: synchronizing clocks\n");
        err = timing_sync_timer();
        assert(err_is_ok(err) || err_no(err) == SYS_ERR_SYNC_MISS);
        if(err_no(err) == SYS_ERR_SYNC_MISS) {
            printf("monitor: failed to sync clocks. Bad reference clock?\n");
        }
    }*/

    // Tell the client that asked us to boot this core what happened
    struct monitor_binding *client = st->originating_client;
    boot_core_reply_cont(client, err);
}

static void cap_receive_request_handler(struct monitor_binding *b,
                                        struct monitor_msg_queue_elem *e);

struct cap_receive_request_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_cap_receive_request__args args;
    uintptr_t your_mon_id;
    struct intermon_binding *b;
};

static void
cap_receive_request_enqueue(struct monitor_binding *domain_binding,
                            uintptr_t domain_id, errval_t msgerr,
                            struct capref cap, uint32_t capid,
                            uintptr_t your_mon_id,
                            struct intermon_binding *b)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;

    struct cap_receive_request_state *me =
        malloc(sizeof(struct cap_receive_request_state));
    assert(me != NULL);
    me->args.conn_id = domain_id;
    me->args.err = msgerr;
    me->args.cap = cap;
    me->args.capid = capid;
    me->your_mon_id = your_mon_id;
    me->b = b;
    me->elem.cont = cap_receive_request_handler;

    struct monitor_state *st = domain_binding->st;
    err = monitor_enqueue_send(domain_binding, &st->queue,
                               get_default_waitset(), &me->elem.queue);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_enqueue_send failed");
    }
}

static void
cap_receive_request_cont(struct monitor_binding *domain_binding,
                         uintptr_t domain_id, errval_t msgerr,
                         struct capref cap,
                         uint32_t capid, uintptr_t your_mon_id,
                         struct intermon_binding *b)
{
    DEBUG_CAPOPS("%s ->%"PRIuPTR", %s\n", __FUNCTION__, domain_id, err_getstring(msgerr));
    errval_t err, err2;
    struct capref *capp = caprefdup(cap);

    err = domain_binding->tx_vtbl.
        cap_receive_request(domain_binding, MKCONT(free, capp), domain_id, msgerr, cap, capid);

    if (err_is_fail(err)) {
        DEBUG_CAPOPS("%s: send failed: %s\n", __FUNCTION__, err_getstring(err));
        free(capp);
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            DEBUG_CAPOPS("%s: enqueueing message b/c flounder busy\n", __FUNCTION__);
            cap_receive_request_enqueue(domain_binding, domain_id, msgerr, cap,
                                        capid, your_mon_id, b);
        } else {
            if (!capref_is_null(cap)) {
                err2 = cap_destroy(cap);
                if (err_is_fail(err2)) {
                    USER_PANIC_ERR(err, "cap_destroy failed");
                }
            }
            // TODO: handle sanely: kill dispatcher/teardown binding/etc.
            USER_PANIC_ERR(err, "error delivering cap to local dispatcher");
        }
    }
}

static void cap_receive_request_handler(struct monitor_binding *b,
                                        struct monitor_msg_queue_elem *e)
{
    struct cap_receive_request_state *st =
        (struct cap_receive_request_state *)e;
    cap_receive_request_cont(b, st->args.conn_id, st->args.err, st->args.cap,
                             st->args.capid, st->your_mon_id, st->b);
    free(e);
}

struct cap_send_request_st {
    struct captx_recv_state captx_st;
    struct intermon_binding *b;
    uint32_t capid;
    uintptr_t my_mon_id;
};

static void
cap_send_request_caprecv_cont(errval_t err, struct captx_recv_state *captx_st,
                              struct capref cap, void *st_)
{
#if defined (DEBUG_MONITOR_CAPOPS)
    char buf[256];
    debug_print_capref(buf,256,cap);
    DEBUG_CAPOPS("%s: %s (cap: %s)\n", __FUNCTION__, err_getstring(err), buf);
#endif
    struct cap_send_request_st *st = (struct cap_send_request_st*)st_;

    uintptr_t my_mon_id = st->my_mon_id;
    struct remote_conn_state *conn = remote_conn_lookup(my_mon_id);
    assert(conn != NULL);
    uintptr_t your_mon_id = conn->mon_id;

    // Get the user domain's connection and connection id
    struct monitor_binding *domain_binding = conn->domain_binding;
    uintptr_t domain_id = conn->domain_id;

    // Try to send cap to the user domain, but only if the queue is empty
    struct monitor_state *mst = domain_binding->st;
    if (msg_queue_is_empty(&mst->queue)) {
        DEBUG_CAPOPS("deliver cap to user domain 0x%"PRIxPTR"\n", domain_id);
        cap_receive_request_cont(domain_binding, domain_id, err, cap, st->capid,
                                 your_mon_id, st->b);
    } else {
        DEBUG_CAPOPS("enqueue cap for delivery to user domain\n");
        // don't allow sends to bypass the queue
        cap_receive_request_enqueue(domain_binding, domain_id, err, cap, st->capid,
                                    your_mon_id, st->b);
    }
}

static void
cap_send_request(struct intermon_binding *b, mon_id_t my_mon_id,
                 uint32_t capid, intermon_captx_t captx)
{
    DEBUG_CAPOPS("intermon: %s\n", __FUNCTION__);
    errval_t err;

    struct cap_send_request_st *st;
    st = malloc(sizeof(*st));
    if (!st) {
        err = LIB_ERR_MALLOC_FAIL;
        goto send_err;
    }

    st->capid = capid;
    st->my_mon_id = my_mon_id;
    st->b = b;

    captx_handle_recv(&captx, &st->captx_st,
                      cap_send_request_caprecv_cont, st);

    return;

send_err:
    // XXX... should send error here
    DEBUG_ERR(err, "error while handling intermon send_request");
}

static void span_domain_request(struct intermon_binding *b,
                                state_id_t state_id, genpaddr_t vnodebase,
                                genpaddr_t framebase, uint8_t framebits)
{
    errval_t err, err2;

    /* Sender's core_id */
    struct intermon_state *st = b->st;
    coreid_t core_id = st->core_id;

    trace_event(TRACE_SUBSYS_MONITOR, TRACE_EVENT_MONITOR_SPAN, disp_get_core_id());

    /* Contruct vroot */
    struct capability vnode_cap = {
        .type = ObjType_VNode_x86_64_pml4,
        .rights = CAPRIGHTS_READ_WRITE, // XXX
        .u.vnode_x86_64_pml4 = {
            .base = vnodebase,
        }
    };
    struct capref vroot;
    err = slot_alloc(&vroot);
    if (err_is_fail(err)) {
        err_push(err, LIB_ERR_SLOT_ALLOC);
        goto reply;
    }
    err = monitor_cap_create(vroot, &vnode_cap, core_id);
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_CAP_CREATE);
        goto reply;
    }

    /* Construct disp frame */
    struct capability dispframe_cap = {
        .type = ObjType_Frame,
        .rights = CAPRIGHTS_READ_WRITE, // XXX
        .u.frame = {
            .base = framebase,
            .bits = framebits
        }
    };
    struct capref disp;
    err = slot_alloc(&disp);
    if (err_is_fail(err)) {
        err_push(err, LIB_ERR_SLOT_ALLOC);
        goto reply;
    }
    err = monitor_cap_create(disp, &dispframe_cap, core_id);
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_CAP_CREATE);
        goto reply;
    }

    err = monitor_remote_relations(disp, RRELS_COPY_BIT, RRELS_COPY_BIT, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_remote_relations failed");
        return;
    }
    err = monitor_remote_relations(vroot, RRELS_COPY_BIT, RRELS_COPY_BIT, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_remote_relations failed");
        return;
    }

    err = span_domain(vroot, disp);
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_SPAN_DOMAIN);
    }

 reply:
    err2 = b->tx_vtbl.span_domain_reply(b, NOP_CONT, state_id, err);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "Failed to reply to the monitor");
    }
    err2 = cap_destroy(vroot);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "Failed to destroy span_vroot cap");
    }
    err2 = cap_destroy(disp);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "Failed to destroy disp cap");
    }
}

static void span_domain_reply(struct intermon_binding *b,
                              uint64_t state_id, errval_t msgerr)
{
    errval_t err;
    struct span_state *state = span_state_lookup(state_id);
    err = state->mb->tx_vtbl.span_domain_reply(state->mb, NOP_CONT, msgerr,
                                               state->domain_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Replying to the domain failed");
    }

    err = span_state_free(state_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Freeing span state failed");
    }
}

static void trace_caps_request(struct intermon_binding *b)
{
    errval_t err;

    /* Identify the frame cap */
    struct capref tracecap = {
        .cnode  = cnode_task,
        .slot   = TASKCN_SLOT_TRACEBUF
    };
    struct capability tracecapa;
    err = monitor_cap_identify(tracecap, &tracecapa);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_identify failed");
        return;
    }

    intermon_caprep_t caprep;
    capability_to_caprep(&tracecapa, &caprep);

    err = b->tx_vtbl.trace_caps_reply(b, NOP_CONT, caprep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending trace_caps_reply failed");
    }
}

static void trace_caps_reply(struct intermon_binding *b,
                             intermon_caprep_t caprep)
{
    struct capability capability;
    caprep_to_capability(&caprep, &capability);
    assert(capability.type != ObjType_Null);

    trace_cap.cnode = cnode_task;
    trace_cap.slot = TASKCN_SLOT_TRACEBUF;

    errval_t err = monitor_cap_create(trace_cap, &capability, my_core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_create failed");
    }
}

static void mem_serv_iref_request(struct intermon_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.mem_serv_iref_reply(b, NOP_CONT, mem_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending mem_serv_iref_reply failed");
    }
}

static void mem_serv_iref_reply(struct intermon_binding *b, iref_t iref)
{
    assert(mem_serv_iref == 0);
    mem_serv_iref = iref;
}

static void name_serv_iref_request(struct intermon_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.name_serv_iref_reply(b, NOP_CONT, name_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending mem_serv_iref_reply failed");
    }
}

static void name_serv_iref_reply(struct intermon_binding *b, iref_t iref)
{
    assert(name_serv_iref == 0);
    name_serv_iref = iref;
}

static void monitor_mem_iref_request(struct intermon_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.monitor_mem_iref_reply(b, NOP_CONT, monitor_mem_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending mem_serv_iref_reply failed");
    }
}

static void monitor_mem_iref_reply(struct intermon_binding *b, iref_t iref)
{
    assert(monitor_mem_iref == 0);
    monitor_mem_iref = iref;
}

static void ramfs_serv_iref_request(struct intermon_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.ramfs_serv_iref_reply(b, NOP_CONT, ramfs_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending ramfs_serv_iref_request failed");
    }
}

static void ramfs_serv_iref_reply(struct intermon_binding *b, iref_t iref)
{
    assert(ramfs_serv_iref == 0);
    ramfs_serv_iref = iref;
}

static void inter_rsrc_join(struct intermon_binding *b,
                            rsrcid_t id, uint8_t coreid)
{
    errval_t err = rsrc_join_satellite(id, coreid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "rsrc_join_satellite failed");
    }
}

struct rsrc_timer_sync__args {
    errval_t error;
};

struct rsrc_timer_sync_state {
    struct intermon_msg_queue_elem elem;
    struct rsrc_timer_sync__args args;
};

static void inter_rsrc_timer_sync_retry(struct intermon_binding *b,
                                        struct intermon_msg_queue_elem *e);

static void inter_rsrc_timer_sync_cont(struct intermon_binding *b,
                                       errval_t msgerr)
{
    errval_t err;
    err = b->tx_vtbl.rsrc_timer_sync_reply(b, NOP_CONT, msgerr);
    if(err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct rsrc_timer_sync_state *me =
                malloc(sizeof(struct rsrc_timer_sync_state));
            assert(me);
            struct intermon_state *st = b->st;
            me->elem.cont = inter_rsrc_timer_sync_retry;
            me->args.error = msgerr;
            err = intermon_enqueue_send(b, &st->queue, get_default_waitset(),
                                        &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "intermon_enqueue_send failed");
            }
        } else {
            USER_PANIC_ERR(err, "sending rsrc_timer_sync_reply failed");
        }
    }
}

static void inter_rsrc_timer_sync_retry(struct intermon_binding *b,
                                        struct intermon_msg_queue_elem *e)
{
    struct rsrc_timer_sync_state *st = (struct rsrc_timer_sync_state*)e;
    inter_rsrc_timer_sync_cont(b, st->args.error);
}

static void inter_rsrc_timer_sync(struct intermon_binding *b,
                                  uint64_t timestamp)
{
    errval_t err = invoke_monitor_sync_timer(timestamp);
    inter_rsrc_timer_sync_cont(b, err);
}

static void inter_rsrc_timer_sync_reply(struct intermon_binding *b,
                                        errval_t err)
{
    // Relay to timing code
    timing_sync_timer_reply(err);
}

static void inter_rsrc_phase(struct intermon_binding *b, rsrcid_t id,
                             uint32_t phase, uint64_t timestamp)
{
    errval_t err = rsrc_set_phase_inter(id, phase, timestamp);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "rsrc_set_phase_inter failed");
    }
}

static void inter_rsrc_phase_data(struct intermon_binding *b, rsrcid_t id,
                                  uint32_t phase, uint8_t *data, size_t len)
{
    errval_t err = rsrc_set_phase_data(id, phase, data, len);
    assert(err_is_ok(err));
}

static void inter_rsrc_join_complete(struct intermon_binding *b, rsrcid_t id)
{
    struct monitor_blocking_binding *mb = rsrc_get_binding(id);

    assert(mb != NULL);
    errval_t err = mb->tx_vtbl.rsrc_join_response(mb, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void spawnd_image_request(struct intermon_binding *b)
{
    assert(bsp_monitor);
    errval_t err;

    struct mem_region *mod = multiboot_find_module(bi, "/spawnd");
    if (mod == NULL) {
        USER_PANIC("didn't find spawnd module in multiboot image");
    }

    assert(mod->mr_type == RegionType_Module);

    err = b->tx_vtbl.spawnd_image_reply(b, NOP_CONT, mod->mr_base, mod->mrmod_size);
    assert(err_is_ok(err));
}

static void give_kcb_request(struct intermon_binding *b, intermon_caprep_t kcb_rep)
{
    errval_t err;
    struct capability kcb_cap;

    caprep_to_capability(&kcb_rep, &kcb_cap);
    assert(kcb_cap.type != ObjType_Null);

    struct capref kcb_capref;
    err = slot_alloc(&kcb_capref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can't allocate slot for kcb_capref.");
    }

    err = monitor_cap_create(kcb_capref, &kcb_cap, my_core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_create failed");
    }

    printf("%s:%s:%d: Remote monitor: give kcb to kernel\n",
                __FILE__, __FUNCTION__, __LINE__);
    uintptr_t kcb_base = (uintptr_t)kcb_cap.u.kernelcontrolblock.kcb;
    err = invoke_monitor_add_kcb(kcb_base);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "invoke_monitor_add_kcb failed.");
    }

    err = b->tx_vtbl.give_kcb_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void give_kcb_response(struct intermon_binding *ib, errval_t error)
{
    printf("%s:%s:%d: Local monitor received answer\n",
                __FILE__, __FUNCTION__, __LINE__);
    if (err_is_fail(error)) {
        USER_PANIC_ERR(error, "give kcb did not work.");
    }

    struct monitor_blocking_binding * b = (struct monitor_blocking_binding *) ib->st;
    if (b != NULL) {
        errval_t err = b->tx_vtbl.forward_kcb_request_response(b, NOP_CONT, error);
        assert(err_is_ok(err));
    }
}

static void forward_kcb_rm_request(struct intermon_binding *b, uint64_t kcb_base)
{
    errval_t err;
    // don't switch kcbs on the current core
    err = invoke_monitor_suspend_kcb_scheduler(true);
    assert(err_is_ok(err));
    // remove kcb from ring
    err = invoke_monitor_remove_kcb((uintptr_t) kcb_base);
    assert(err_is_ok(err));
    // send reply
    err = b->tx_vtbl.forward_kcb_rm_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
    // disp_save_rm_kcb -> next kcb -> enable kcb switching again
    disp_save_rm_kcb();
    // send monitor initialized when we're back up
    //err = b->tx_vtbl.monitor_initialized(b, NOP_CONT);
    //assert(err_is_ok(err));
}

static void forward_kcb_rm_response(struct intermon_binding *b, errval_t error)
{
    //XXX: HACK
    struct monitor_blocking_binding *mb =
        (struct monitor_blocking_binding*)
        ((struct intermon_state*)b->st)->originating_client;

    debug_printf("received kcb_rm response on %d, forwarding to %p\n", my_core_id, mb);

    mb->tx_vtbl.forward_kcb_rm_request_response(mb, NOP_CONT, error);
}

static struct intermon_rx_vtbl the_intermon_vtable = {
    .trace_caps_request = trace_caps_request,
    .trace_caps_reply = trace_caps_reply,
    .mem_serv_iref_request = mem_serv_iref_request,
    .mem_serv_iref_reply = mem_serv_iref_reply,
    .name_serv_iref_request = name_serv_iref_request,
    .name_serv_iref_reply = name_serv_iref_reply,
    .ramfs_serv_iref_request = ramfs_serv_iref_request,
    .ramfs_serv_iref_reply = ramfs_serv_iref_reply,
    .monitor_mem_iref_request = monitor_mem_iref_request,
    .monitor_mem_iref_reply = monitor_mem_iref_reply,

    .capops_ready              = capops_ready,
    .monitor_initialized       = monitor_initialized,

    .spawnd_image_request      = spawnd_image_request,

    .cap_send_request          = cap_send_request,

    .span_domain_request       = span_domain_request,
    .span_domain_reply         = span_domain_reply,

    .rsrc_join                 = inter_rsrc_join,
    .rsrc_join_complete        = inter_rsrc_join_complete,
    .rsrc_timer_sync           = inter_rsrc_timer_sync,
    .rsrc_timer_sync_reply     = inter_rsrc_timer_sync_reply,
    .rsrc_phase                = inter_rsrc_phase,
    .rsrc_phase_data           = inter_rsrc_phase_data,

    .give_kcb_request = give_kcb_request,
    .give_kcb_response = give_kcb_response,

    .forward_kcb_rm_request = forward_kcb_rm_request,
    .forward_kcb_rm_response = forward_kcb_rm_response,
};

errval_t intermon_init(struct intermon_binding *b, coreid_t coreid)
{
    errval_t err;

    struct intermon_state *st = malloc(sizeof(struct intermon_state));
    assert(st != NULL);

    st->core_id = coreid;
    st->binding = b;
    st->queue.head = st->queue.tail = NULL;
    st->rsrcid_inflight = false;
    st->capops_ready = true;
    st->originating_client = NULL;
    b->st = st;
    b->rx_vtbl = the_intermon_vtable;

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    err = ump_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ump_intermon_init failed");
        return err;
    }
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
    err = multihop_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "multihop_intermon_init failed");
        return err;
    }
#endif

#if CONFIG_TRACE
    err = trace_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_intermon_init failed");
        return err;
    }

    err = bfscope_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bfscope_intermon_init failed");
        return err;
    }
#endif

    err = arch_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "arch_intermon_init failed");
        return err;
    }

    err = capops_init(b->waitset, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "capops_intermon_init failed");
        return err;
    }

    err = intermon_binding_set(st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "intermon_binding_set failed");
        return err;
    }

    return SYS_ERR_OK;
}
