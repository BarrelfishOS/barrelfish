/**
 * \file
 * \brief Arch-specific inter-monitor communication
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include "monitor.h"
#include <trace/trace.h>

/* FIXME: needs a header file! */
extern errval_t beehive_create_cap(coreid_t coreid, int chanid,
                                   struct capref *retcap);

extern errval_t beehive_chan_allocate(struct capref ep, int *chanid);

extern void intermon_bmp_setup(struct intermon_bmp_binding *b, struct capref bmp_cap,
                        struct lmp_endpoint *inep, struct capref inepcap);


/******* stack-ripped bind_monitor_request_bmp *******/

static void bind_monitor_reply_bmp_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e);

struct bind_monitor_reply_bmp_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_monitor_reply_bmp__args args;
};

static void bind_monitor_reply_bmp_cont(struct intermon_binding *st,
                                        errval_t err, chanid_t chanid)
{
    errval_t err2;

    err2 = st->tx_vtbl.bind_monitor_reply_bmp(st, NOP_CONT, err,
                                              chanid, my_core_id);
    if (err_is_fail(err2)) {
        if(err_no(err2) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_monitor_reply_bmp_state *me =
                malloc(sizeof(struct bind_monitor_reply_bmp_state));
            assert(me != NULL);
            struct intermon_state *ist = st->st;
            assert(ist != NULL);
            me->args.err = err;
            me->args.chan_id = chanid;
            me->elem.cont = bind_monitor_reply_bmp_handler;

            err = intermon_enqueue_send(st, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err2, "reply failed");
    }
}

static void bind_monitor_reply_bmp_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e)
{
    struct bind_monitor_reply_bmp_state *st = (struct bind_monitor_reply_bmp_state *)e;
    bind_monitor_reply_bmp_cont(b, st->args.err, st->args.chan_id);
    free(e);
}

/**
 * \brief A monitor receives request to setup a connection
 * with another newly booted monitor from a third monitor
 */
static void bind_monitor_request_bmp(struct intermon_binding *st,
                                     coreid_t core_id, 
                                     chanid_t chan_id, 
                                     coreid_t from_core_id)
{
    errval_t err;

    // Create our own beehive cap to the remote core
    struct capref beehive_cap;
    err = beehive_create_cap(core_id, chan_id, &beehive_cap);
    assert(err == SYS_ERR_OK);

    struct capref ep;
    struct lmp_endpoint *iep;
    err = endpoint_create(DEFAULT_BMP_BUF_WORDS, &ep, &iep);
    assert(err_is_ok(err));

    int mychanid;
    err = beehive_chan_allocate(ep, &mychanid);
    assert(err_is_ok(err));

    // setup our side of the binding
    struct intermon_bmp_binding *ibb;
    ibb = malloc(sizeof(struct intermon_bmp_binding));
    assert(ibb != NULL);

    intermon_bmp_setup(ibb, beehive_cap, iep, ep);

    // connect it to our request handlers
    intermon_init(&ibb->b, core_id);

    /* Send reply */
    assert(ibb != NULL);
    bind_monitor_reply_bmp_cont(&ibb->b, err, mychanid);
    return;
}

/**
 * \brief The monitor that proxied the request for one monitor to
 * setup a connection with another monitor gets the reply
 */
static void bind_monitor_reply_bmp(struct intermon_binding *closure,
                                   errval_t err, chanid_t chan_id,
                                   coreid_t core_id)
{
    struct intermon_bmp_binding *b = (struct intermon_bmp_binding *)closure;

    // Create a way to send beehive messages to new core
    err = beehive_create_cap(core_id, chan_id, &b->bmp_state.chan.outepcap);
    assert(err == SYS_ERR_OK);
}

/******* stack-ripped bind_monitor_proxy_bmp *******/

static void bind_monitor_request_bmp_handler(struct intermon_binding *b,
                                           struct intermon_msg_queue_elem *e);

struct bind_monitor_request_bmp_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_monitor_request_bmp__args args;
};

static void bind_monitor_request_bmp_cont(struct intermon_binding *dst_closure,
                                          coreid_t src_core_id,
                                          chanid_t chan_id,
                                          coreid_t core_id)
{
    errval_t err;

    err = dst_closure->tx_vtbl.
        bind_monitor_request_bmp(dst_closure, NOP_CONT, src_core_id, 
                                 chan_id, core_id);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_monitor_request_bmp_state *me =
                malloc(sizeof(struct bind_monitor_request_bmp_state));
            assert(me != NULL);
            struct intermon_state *ist = dst_closure->st;
            assert(ist != NULL);
            me->args.core_id = src_core_id;
            me->args.chan_id = chan_id;
            me->args.from_core_id = core_id;
            me->elem.cont = bind_monitor_request_bmp_handler;

            err = intermon_enqueue_send(dst_closure, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err, "forwarding bind request failed");
    }
}

static void bind_monitor_request_bmp_handler(struct intermon_binding *b,
                                             struct intermon_msg_queue_elem *e)
{
    struct bind_monitor_request_bmp_state *st = (struct bind_monitor_request_bmp_state *)e;
    bind_monitor_request_bmp_cont(b, st->args.core_id,
                                  st->args.chan_id, st->args.from_core_id);
    free(e);
}

/**
 * \brief A monitor asks this monitor to proxy
 * its request to bind to another monitor
 */
static void bind_monitor_proxy_bmp(struct intermon_binding *st,
                                   coreid_t dst_core_id,
                                   chanid_t chan_id,
                                   coreid_t core_id)
{
    seen_connections++;
    errval_t err;

    /* Get source monitor's core id */
    coreid_t src_core_id = 0;
    err = intern_get_core_id(st, &src_core_id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "intern_get_core_id failed");
    }

    /* Get destination monitor */
    struct intermon_binding *dst_closure = NULL;
    err = intern_get_closure(dst_core_id, &dst_closure);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "intern_get_closure failed");
    }

    // Proxy the request
    bind_monitor_request_bmp_cont(dst_closure, src_core_id, 
                                  chan_id, core_id);
}

/**
 * \brief Notification of a newly booted monitor.
 *  Setup our connection and request the sender to proxy
 *  the bind request to the monitor
 */
static void new_monitor_notify(struct intermon_binding *st,
                               coreid_t core_id)
{
    errval_t err;

    /*
     * Create the binding between this monitor and the monitor on the
     * newly spawned core. The other half of this connection setup is 
     * performed in bind_monitor_request_bmp() above.
     */

    struct intermon_bmp_binding *bmp_binding = 
        malloc(sizeof(struct intermon_bmp_binding));
    assert(bmp_binding != NULL);

    // Create a normal IDC endpoint cap for incoming messages
    struct capref ep;
    struct lmp_endpoint *iep;
    err = endpoint_create(DEFAULT_BMP_BUF_WORDS, &ep, &iep);
    assert(err_is_ok(err));

    // XXX Need to allocate a kernel 'association' for incoming messages
    // And then set it up to deliver to the above endpoint
    int chanid;
    err = beehive_chan_allocate(ep, &chanid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "beehive_chan_allocate failed");
        abort();
    }
    assert(err_is_ok(err));

    // init our end of the binding and channel
    // we won't know the bmp cap until we get the bind reply
    intermon_bmp_setup(bmp_binding, NULL_CAP, iep, ep);

    err = intermon_init(&bmp_binding->b, core_id);
    assert(err_is_ok(err));

    /* reply to the sending monitor to proxy request */
    err = st->tx_vtbl.bind_monitor_proxy_bmp(st, NOP_CONT, core_id,
                                             chanid, my_core_id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind proxy request failed");
    }
}

static void spawnd_image_request(struct intermon_binding *b)
{
    assert(bsp_monitor);
    errval_t err;

    char namebuf[32];
    size_t namelen;

    struct intermon_state *st = b->st;

    namelen = snprintf(namebuf, sizeof(namebuf),
                        "beehive/sbin/spawnd|%u", st->core_id);
    assert(namelen < sizeof(namebuf));

    struct mem_region *mod = multiboot_find_module(bi, namebuf);
    if (mod == NULL) {
        USER_PANIC("didn't find %s in multiboot image", namebuf);
    }

    assert(mod->mr_type == RegionType_Module);

    err = b->tx_vtbl.spawnd_image_reply(b, NOP_CONT, mod->mr_base, mod->mrmod_size);
    assert(err_is_ok(err));
}

errval_t arch_intermon_init(struct intermon_binding *b)
{
    b->rx_vtbl.bind_monitor_request_bmp = bind_monitor_request_bmp;
    b->rx_vtbl.bind_monitor_reply_bmp = bind_monitor_reply_bmp;
    b->rx_vtbl.bind_monitor_proxy_bmp = bind_monitor_proxy_bmp;
    b->rx_vtbl.new_monitor_notify = new_monitor_notify;
    b->rx_vtbl.spawnd_image_request = spawnd_image_request;

    return SYS_ERR_OK;
}
