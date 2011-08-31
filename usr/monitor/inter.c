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
#include "monitor.h"
#include <trace/trace.h>

/**
 * \brief A newly booted monitor indicates that it has initialized
 */
static void monitor_initialized(struct intermon_binding *b)
{
    struct intermon_state *st = b->st;

    // Set the flag
    errval_t err = intern_set_initialize(st->core_id, true);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to set initialized");
    }
}

static void cap_receive_request_handler(struct monitor_binding *b,
                                        struct monitor_msg_queue_elem *e);
static void cap_send_reply_handler(struct intermon_binding *b,
                                   struct intermon_msg_queue_elem *e);

struct cap_receive_request_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_cap_receive_request__args args;
    uintptr_t your_mon_id;
    struct intermon_binding *b;
};

struct cap_send_reply_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_cap_send_reply__args args;
};

static void cap_send_reply_cont(struct intermon_binding *b,
                                uintptr_t your_mon_id, uint32_t capid,
                                errval_t err)
{
    errval_t err2;

    /* Send reply back to the requesting monitor */
    err2 = b->tx_vtbl.cap_send_reply(b, NOP_CONT, your_mon_id, capid, err);
    if (err_is_fail(err2)) {
        if(err_no(err2) == FLOUNDER_ERR_TX_BUSY) {
            struct cap_send_reply_state *me =
                malloc(sizeof(struct cap_send_reply_state));
            struct intermon_state *st = b->st;
            me->args.con_id = your_mon_id;
            me->args.capid = capid;
            me->args.err = err;
            me->elem.cont = cap_send_reply_handler;

            err = intermon_enqueue_send(b, &st->queue, get_default_waitset(),
                                        &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "intermon_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err, "cap send reply to monitor failed");
    }
}

static void cap_send_reply_handler(struct intermon_binding *b,
                                   struct intermon_msg_queue_elem *e)
{
    struct cap_send_reply_state *st = (struct cap_send_reply_state *)e;
    cap_send_reply_cont(b, st->args.con_id, st->args.capid, st->args.err);
    free(e);
}

static void
cap_receive_request_enqueue(struct monitor_binding *domain_closure,
                         uintptr_t domain_id, struct capref cap,
                         uint32_t capid, uintptr_t your_mon_id,
                         struct intermon_binding *b)
{
    errval_t err;

    struct cap_receive_request_state *me =
        malloc(sizeof(struct cap_receive_request_state));
    assert(me != NULL);
    me->args.conn_id = domain_id;
    me->args.cap = cap;
    me->args.capid = capid;
    me->your_mon_id = your_mon_id;
    me->b = b;
    me->elem.cont = cap_receive_request_handler;

    struct monitor_state *st = domain_closure->st;
    err = monitor_enqueue_send(domain_closure, &st->queue,
                               get_default_waitset(), &me->elem.queue);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_enqueue_send failed");
    }
}

static void
cap_receive_request_cont(struct monitor_binding *domain_closure,
                         uintptr_t domain_id, struct capref cap,
                         uint32_t capid, uintptr_t your_mon_id,
                         struct intermon_binding *b)
{
    errval_t err, err2;
    struct capref *capp = caprefdup(cap);

    struct event_closure cont;
    if (capref_is_null(cap)) {
        cont = NOP_CONT;
    } else {
        cont = MKCONT(destroy_outgoing_cap, capp);
    }
    err = domain_closure->tx_vtbl.
        cap_receive_request(domain_closure, cont, domain_id, cap, capid);

    if (err_is_fail(err)) {
        free(capp);
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            cap_receive_request_enqueue(domain_closure, domain_id, cap, capid,
                                        your_mon_id, b);
            return;
        }

        if (!capref_is_null(cap)) {
            err2 = cap_destroy(cap);
            if (err_is_fail(err2)) {
                USER_PANIC_ERR(err, "cap_destroy failed");
            }
            err = err_push(err, MON_ERR_CAP_SEND);
        } 
        goto reply;
    }

reply:
    if(err_is_fail(err)) {
        cap_send_reply_cont(b, your_mon_id, capid, err);
    }
}

static void cap_receive_request_handler(struct monitor_binding *b,
                                        struct monitor_msg_queue_elem *e)
{
    struct cap_receive_request_state *st =
        (struct cap_receive_request_state *)e;
    cap_receive_request_cont(b, st->args.conn_id, st->args.cap, st->args.capid,
                             st->your_mon_id, st->b);
    free(e);
}

static void cap_send_request(struct intermon_binding *b,
                             mon_id_t my_mon_id, uint32_t capid,
                             intermon_caprep_t caprep, 
                             bool give_away, bool remote_has_desc,
                             coremask_t on_cores, 
                             bool null_cap) 
{
    errval_t err, err2;

    /* Get client's core_id from the closure */
    coreid_t core_id = 0;
    err = intern_get_core_id(b, &core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "intern_get_core_id failed");
    }

    struct remote_conn_state *conn = remote_conn_lookup(my_mon_id);
    assert(conn != NULL);
    uintptr_t your_mon_id = conn->mon_id;

    /* Construct the capability */
    struct capability *capability = (struct capability *)&caprep;
    struct capref cap;

    if (null_cap) {
        cap = NULL_CAP;
    } else {
        err = slot_alloc(&cap);
        if (err_is_fail(err)) {
            err = err_push(err, LIB_ERR_SLOT_ALLOC);
            goto reply;
        }

        err = monitor_cap_create(cap, capability, core_id);
        if (err_is_fail(err)) {
            // cleanup
            err = err_push(err, MON_ERR_CAP_CREATE);
            goto cleanup1;
        }

        if (!give_away) {
            if (!rcap_db_exists(capability)) {
                bool kern_has_desc;
                err = monitor_cap_remote(cap, true, &kern_has_desc);
                if (err_is_fail(err)) {
                    // cleanup
                    err = err_push(err, MON_ERR_CAP_REMOTE);
                    goto cleanup2;
                }
                // if this assert fires, then something wierd has happened 
                // where our kernel knows this cap has descendents, but the 
                // sending core didn't
                assert(!kern_has_desc || remote_has_desc);
            
                rcap_db_update_on_recv (capability, remote_has_desc, on_cores, 
                                        core_id);
                if (err_is_fail(err)) {
                    // cleanup
                    err = err_push(err, MON_ERR_CAP_REMOTE);
                    goto cleanup2;
                }
            } else {
                // have already been sent this capability
                assert (on_cores & get_coremask(my_core_id));
            }
        } else {
            bool kern_has_desc;
            err = monitor_cap_remote(cap, true, &kern_has_desc);
            if (err_is_fail(err)) {
                // cleanup
                err = err_push(err, MON_ERR_CAP_REMOTE);
                goto cleanup2;
            }

            // TODO, do something if this cap already has descendents to ensure 
            // that it cannot be retyped on this core
        }

    }

    /* Get the user domain's connection and connection id */
    struct monitor_binding *domain_closure = conn->domain_closure;
    uintptr_t domain_id = conn->domain_id;

    /* Try to send cap to the user domain, but only if the queue is empty */
    struct monitor_state *mst = domain_closure->st;
    if (msg_queue_is_empty(&mst->queue)) {
        cap_receive_request_cont(domain_closure, domain_id, cap, capid,
                                 your_mon_id, b);
    } else {
        // don't allow sends to bypass the queue
        cap_receive_request_enqueue(domain_closure, domain_id, cap, capid,
                                    your_mon_id, b);
    }
    return;

cleanup2:
    err2 = cap_destroy(cap);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "cap_destroy failed");
    }
cleanup1:
    err2 = slot_free(cap);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "slot_free failed");
    }
reply:
    cap_send_reply_cont(b, your_mon_id, capid, err);
}

struct monitor_cap_send_reply_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_cap_send_reply__args args;
};

static void monitor_cap_send_reply_handler(struct monitor_binding *b,
                                           struct monitor_msg_queue_elem *e);

static void monitor_cap_send_reply_cont(struct monitor_binding *domain_closure,
                                        uintptr_t domain_id, uint32_t capid,
                                        errval_t msgerr)
{
    errval_t err;

    err = domain_closure->tx_vtbl.cap_send_reply(domain_closure, NOP_CONT,
                                                 domain_id, capid, msgerr);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_cap_send_reply_state *me =
                malloc(sizeof(struct monitor_cap_send_reply_state));
            struct monitor_state *st = domain_closure->st;
            me->args.conn_id = domain_id;
            me->args.capid = capid;
            me->args.err = msgerr;
            me->elem.cont = monitor_cap_send_reply_handler;

            err = monitor_enqueue_send(domain_closure, &st->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err, "cap send reply to user failed");
    }
}

static void monitor_cap_send_reply_handler(struct monitor_binding *b,
                                           struct monitor_msg_queue_elem *e)
{
    struct monitor_cap_send_reply_state *st =
        (struct monitor_cap_send_reply_state *)e;
    monitor_cap_send_reply_cont(b, st->args.conn_id,
                                st->args.capid, st->args.err);
    free(e);
}

static void cap_send_reply(struct intermon_binding *closure,
                           con_id_t con_id, uint32_t capid,
                           errval_t msgerr)
{
    struct remote_conn_state *conn = remote_conn_lookup(con_id);
    assert(conn != NULL);

    /* Forward reply */
    uintptr_t domain_id = conn->domain_id;
    struct monitor_binding *domain_closure = conn->domain_closure;
    monitor_cap_send_reply_cont(domain_closure, domain_id, capid, msgerr);
}

static void span_domain_request(struct intermon_binding *st,
                                state_id_t state_id, genpaddr_t vnodebase,
                                genpaddr_t framebase, uint8_t framebits)
{
    errval_t err, err2;

    /* Sender's core_id */
    coreid_t core_id = 0;
    err = intern_get_core_id(st, &core_id);
    assert(err_is_ok(err));

    trace_event(TRACE_SUBSYS_MONITOR, TRACE_EVENT_SPAN, disp_get_core_id());

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

    bool has_descendants;
    err = monitor_cap_remote(disp, true, &has_descendants);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_remote failed");
        return;
    }
    err = monitor_cap_remote(vroot, true, &has_descendants);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_remote failed");
        return;
    }

    err = span_domain(vroot, disp);
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_SPAN_DOMAIN);
    }

 reply:
    err2 = st->tx_vtbl.span_domain_reply(st, NOP_CONT, state_id, err);
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

static void span_domain_reply(struct intermon_binding *closure,
                              uint64_t state_id, errval_t msgerr)
{
    errval_t err;
    struct span_state *state = span_state_lookup(state_id);
    err = state->st->tx_vtbl.span_domain_reply(state->st, NOP_CONT, msgerr,
                                               state->domain_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Replying to the domain failed");
    }

    err = span_state_free(state_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Freeing span state failed");
    }
}

#define MAX_ITERATIONS          1000

static void pingpong(struct intermon_binding *closure,
                     uint64_t data)
{
    static uint64_t lastdata = 0;
    static uint64_t timestamps[MAX_ITERATIONS] = { 0 };
    static int cnt = 0;

    /* printf("pingpong(%u)\n", data); */

    if(lastdata != 0) {
        assert(lastdata == data - 2);
    }

    lastdata = data;

    if(disp_get_core_id() == 0) {
        timestamps[cnt] = bench_tsc();
        uint64_t sum = 0;

        if(cnt == MAX_ITERATIONS - 1) {
            for(int i = 1; i < MAX_ITERATIONS; i++) {
                sum += timestamps[i] - timestamps[i - 1];
                if(i < 50) {
                    printf("%d: %" PRIu64 "\n", i,
                           timestamps[i] - timestamps[i - 1]);
                }
            }

            sum /= MAX_ITERATIONS - 1;
            printf("average pingpong time: %" PRIu64 "\n", sum);
            cnt = 0;
            abort();
        } else {
            cnt++;
        }
    }

    // This is a special case and shouldn't fail
    errval_t err = closure->tx_vtbl.pingpong(closure, NOP_CONT, data + 1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Replying to the domain failed");
    }
}

static inline void testdata(int lastdata, uint64_t data, int iter, int word)
{
    if(data != lastdata) {
        printf("mismatch at iter %d, word %d: lastdata = %u, "
               "word = %" PRIu64 "\n", iter, word, lastdata, data + 2 + word);
        abort();
    }
}

static void test_pingpong(struct intermon_binding *closure,
                          uint64_t w0, uint64_t w1, uint64_t w2,
                          uint64_t w3, uint64_t w4, uint64_t w5,
                          uint64_t w6, uint64_t w7)
{
    static int lastdata = 0;
    static int iter = 0;

    if(lastdata != 0) {
        testdata(lastdata, w0 - 2, iter, 0);
        testdata(lastdata, w1 - 3, iter, 1);
        testdata(lastdata, w2 - 4, iter, 2);
        testdata(lastdata, w3 - 5, iter, 3);
        testdata(lastdata, w4 - 6, iter, 4);
        testdata(lastdata, w5 - 7, iter, 5);
        testdata(lastdata, w6 - 8, iter, 6);
        testdata(lastdata, w7 - 9, iter, 7);
    }

    lastdata = w0;
    iter++;

    // This is a special case and shouldn't fail
    errval_t err = closure->tx_vtbl.test_pingpong(closure, NOP_CONT, w0 + 1, w0 + 2, w0 + 3, w0 + 4, w0 + 5, w0 + 6, w0 + 7, w0 + 8);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Replying to the domain failed");
    }
}

static void trace_caps_request(struct intermon_binding *st)
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

    err = st->tx_vtbl.trace_caps_reply(st, NOP_CONT, caprep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending trace_caps_reply failed");
    }
}

static void trace_caps_reply(struct intermon_binding *st,
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

static void mem_serv_iref_request(struct intermon_binding *st)
{
    errval_t err;
    err = st->tx_vtbl.mem_serv_iref_reply(st, NOP_CONT, mem_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending mem_serv_iref_reply failed");
    }
}

static void mem_serv_iref_reply(struct intermon_binding *st, iref_t iref)
{
    assert(mem_serv_iref == 0);
    mem_serv_iref = iref;
}

static void name_serv_iref_request(struct intermon_binding *st)
{
    errval_t err;
    err = st->tx_vtbl.name_serv_iref_reply(st, NOP_CONT, name_serv_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending mem_serv_iref_reply failed");
    }
}

static void name_serv_iref_reply(struct intermon_binding *st, iref_t iref)
{
    assert(name_serv_iref == 0);
    name_serv_iref = iref;
}

static void monitor_mem_iref_request(struct intermon_binding *st)
{
    errval_t err;
    err = st->tx_vtbl.monitor_mem_iref_reply(st, NOP_CONT, monitor_mem_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending mem_serv_iref_reply failed");
    }
}

static void monitor_mem_iref_reply(struct intermon_binding *st, iref_t iref)
{
    assert(monitor_mem_iref == 0);
    monitor_mem_iref = iref;
}

static void join_route(struct intermon_binding *st, intermon_ROUTE_TYPE_t rt, 
                       routeid_t id)
{
    errval_t err = route_join_app_core(st, rt, id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "route_join_app_core failed");
    }
}

static void connect_neighbors(struct intermon_binding *st, intermon_ROUTE_TYPE_t rt, 
                              routeid_t id, uint8_t * irefs_list, size_t size)
{
    errval_t err = route_connect_app_core(rt, id, (iref_t *)irefs_list, 
                                          size / BYTES_IN_IREF);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "route_join_app_core failed");
    }
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

static struct intermon_rx_vtbl the_intermon_vtable = {
    .trace_caps_request = trace_caps_request,
    .trace_caps_reply = trace_caps_reply,
    .mem_serv_iref_request = mem_serv_iref_request,
    .mem_serv_iref_reply = mem_serv_iref_reply,
    .name_serv_iref_request = name_serv_iref_request,
    .name_serv_iref_reply = name_serv_iref_reply,
    .monitor_mem_iref_request = monitor_mem_iref_request,
    .monitor_mem_iref_reply = monitor_mem_iref_reply,

    .monitor_initialized       = monitor_initialized,

    .spawnd_image_request      = spawnd_image_request,

    .cap_send_request          = cap_send_request,
    .cap_send_reply            = cap_send_reply,

    .span_domain_request       = span_domain_request,
    .span_domain_reply         = span_domain_reply,

    .pingpong                  = pingpong,
    .test_pingpong             = test_pingpong,

    .join_route                = join_route,
    .connect_neighbors         = connect_neighbors,
    .route_done_join           = route_done_join,
    .route_done_connect        = route_done_connect,

    .rsrc_join                 = inter_rsrc_join,
    .rsrc_join_complete        = inter_rsrc_join_complete,
    .rsrc_timer_sync           = inter_rsrc_timer_sync,
    .rsrc_timer_sync_reply     = inter_rsrc_timer_sync_reply,
    .rsrc_phase                = inter_rsrc_phase,
    .rsrc_phase_data           = inter_rsrc_phase_data,
};

errval_t intermon_init(struct intermon_binding *b, coreid_t coreid)
{
    struct intermon_state *st = malloc(sizeof(struct intermon_state));
    assert(st != NULL);
    st->core_id = coreid;
    st->queue.head = st->queue.tail = NULL;
    st->rsrcid_inflight = false;
    b->st = st;
    b->rx_vtbl = the_intermon_vtable;

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    errval_t err;
    err = ump_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ump_intermon_init failed");
    }
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_BMP
    errval_t err;
    err = bmp_intermon_init(b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bmp_intermon_init failed");
    }
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
    errval_t err2;
    err2 = multihop_intermon_init(b);
    if (err_is_fail(err2)) {
      USER_PANIC_ERR(err2, "multihop_intermon_init failed");
    }
    return arch_intermon_init(b);
#endif

    return arch_intermon_init(b);
}
