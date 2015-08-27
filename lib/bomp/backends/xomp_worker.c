/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <flounder/flounder_txqueue.h>
#include <spawndomain/spawndomain.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>

#include <bomp_internal.h>
#include <xomp/xomp.h>
#include <xomp_debug.h>
#include <xomp_gateway.h>
#include <xomp_gateway_client.h>

#if XOMP_BENCH_WORKER_EN
#include <bench/bench.h>
#endif

#include <if/xomp_defs.h>

/// XOMP control channel to the master
static struct xomp_binding *xbinding;

/// flag indicating if the client is bound to the master
static volatile uint8_t is_bound = 0x0;

/// messaging frame capability
static struct capref msgframe;

/// where the messaging frame is mapped
static void *msgbuf;

/// pointer to the thread local storage
static void *tls;

/// service iref of the master (if local worker)
static iref_t svc_iref;

/// our own worker id
static xomp_wid_t worker_id;

/// Flounder TX messaging queue
static struct tx_queue txq;

#if XOMP_WORKER_ENABLE_DMA

#include <dma/dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#ifdef __k1om__
/// dma device type
static dma_dev_type_t dma_device_type = DMA_DEV_TYPE_XEON_PHI;
#else
/// dma device type
//static dma_dev_type_t dma_device_type = DMA_DEV_TYPE_IOAT;
#endif
/// dma client device
static struct dma_client_device *dma_dev;
#endif

/**
 * \brief Flounder TX queue message state representation
 */
struct xomp_msg_st {
    struct txq_msg_st common;

    /* union of arguments */
    union {
        struct {
            uint64_t arg;
            uint64_t id;
        } done_notify;
    } args;
};

#if XOMP_WORKER_ENABLE_DMA

static volatile uint8_t dma_replication_done = 0;

static void dma_replication_cb(errval_t err,
                               dma_req_id_t id,
                               void *arg)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "dma transfer failed\n");
    }
    dma_replication_done = 1;
}

#endif


static errval_t replicate_frame(lvaddr_t addr, struct capref *frame)
{
    errval_t err;

#if XOMP_BENCH_WORKER_EN
    cycles_t repl_timer = bench_tsc();
#endif

    struct frame_identity id;
    err = invoke_frame_identify(*frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    XWR_DEBUG("Replicating frame: [%016lx]\n", id.base);

    struct capref replicate;
    err = frame_alloc(&replicate, (1UL << id.bits), NULL);
    if (err_is_fail(err)) {
        return err;
    }

    XWR_DEBUG("registering memory with DMA service\n");

#if XOMP_BENCH_WORKER_EN
    cycles_t register_timer = bench_tsc();
#endif
    err = dma_register_memory((struct dma_device *) dma_dev, *frame);
    if (err_is_fail(err)) {
        return err;
    }

    err = dma_register_memory((struct dma_device *) dma_dev, replicate);
    if (err_is_fail(err)) {
        return err;
    }

#if XOMP_BENCH_WORKER_EN
    cycles_t register_timer_end = bench_tsc();
#endif

    struct dma_req_setup setup = {
        .done_cb = dma_replication_cb,
        .cb_arg = NULL,
        .args = {
            .memcpy = {
                .src = id.base,
                .bytes = (1UL << id.bits)
            }
        }
    };

    err = invoke_frame_identify(replicate, &id);
    if (err_is_fail(err)) {
        return err;
    }
    setup.args.memcpy.dst = id.base;

    dma_replication_done = 0x0;

    XWR_DEBUG("DMA request for replication\n");

    err = dma_request_memcpy((struct dma_device *)dma_dev, &setup, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    while (!dma_replication_done) {
        messages_wait_and_handle_next();
    }

    XWR_DEBUG("Replication done.\n");

    *frame = replicate;

#if XOMP_BENCH_WORKER_EN
    cycles_t timer_end = bench_tsc();
    debug_printf("%lx replication took %lu cycles, %lu ms\n", worker_id,
                 timer_end - repl_timer, bench_tsc_to_ms(timer_end - repl_timer));
    debug_printf("%lx register mem took %lu cycles, %lu ms\n", worker_id,
                 register_timer_end - register_timer, bench_tsc_to_ms(register_timer_end - register_timer));
#endif

    return SYS_ERR_OK;
}

#ifdef __k1om__

/*
 * ----------------------------------------------------------------------------
 * Xeon Phi Channel callbacks
 * ----------------------------------------------------------------------------
 */
static errval_t msg_open_cb(xphi_dom_id_t domain,
                            uint64_t usrdata,
                            struct capref frame,
                            uint8_t type)
{
    errval_t err;

    uint32_t map_flags = 0x0;
    lvaddr_t addr = 0x0;

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    XWI_DEBUG("msg_open_cb: from domid:%lx, usrdata:%lx, frame:%lx\n", domain,
              usrdata, id.base);

    switch ((xomp_frame_type_t) type) {
        case XOMP_FRAME_TYPE_MSG:
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RW:
            addr = (lvaddr_t) usrdata;
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RO:
            map_flags = VREGION_FLAGS_READ;
            break;
        case XOMP_FRAME_TYPE_REPL_RW:
            map_flags = VREGION_FLAGS_READ_WRITE;
#if XOMP_WORKER_ENABLE_DMA
            addr = (lvaddr_t) usrdata;
            err = replicate_frame(addr, &frame);
            if (err_is_fail(err)) {
                return err;
            }
            err = invoke_frame_identify(frame, &id);
#else
            struct capref replicate;
            err = frame_alloc(&replicate, (1UL << id.bits), NULL);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to allocate replicate frame\n");
                return err;
            }
            err = vspace_map_one_frame_fixed_attr((lvaddr_t) usrdata, (1UL << id.bits),
                                                  replicate, map_flags, NULL, NULL);
            if (err_is_fail(err)) {
                return err;
            }
            err = invoke_frame_identify(replicate, &id);
#endif
            if (err_is_fail(err)) {
                return err;
            }
            break;
        default:
            USER_PANIC("unknown type: %u", type)
            break;
    }
    if (addr) {
        if (worker_id & XOMP_WID_GATEWAY_FLAG) {
            XWR_DEBUG("registering memory with gateway: [%016lx]\n", addr);
            err = xomp_gateway_mem_insert(frame, addr);
            if (err_is_fail(err)) {
                /* todo: cleanup */
                return err;
            }
        }
        err = vspace_map_one_frame_fixed_attr(addr, (1UL << id.bits), frame,
                                              map_flags, NULL, NULL);
    } else {
        err = vspace_map_one_frame_attr((void **) &addr, (1UL << id.bits), frame,
                                        map_flags, NULL, NULL);
    }
    if (err_is_fail(err)) {
        return err;
    }

#if !XOMP_WORKER_ENABLE_DMA
    if ((xomp_frame_type_t) type == XOMP_FRAME_TYPE_REPL_RW) {
        memcpy((void *)usrdata, (void *)addr, (1UL << id.bits));
    }
#endif

    XWI_DEBUG("msg_open_cb: frame [%016lx] mapped @ [%016lx, %016lx]\n", id.base,
              addr, addr + (1UL << id.bits));

    if ((xomp_frame_type_t) type == XOMP_FRAME_TYPE_MSG) {
        USER_PANIC("NYI: initializing messaging");
    }

    return SYS_ERR_OK;
}


static struct xeon_phi_callbacks callbacks = {
    .open = msg_open_cb
};
#endif

/*
 * ----------------------------------------------------------------------------
 * XOMP channel send handlers
 * ----------------------------------------------------------------------------
 */



static errval_t gw_req_memory_response_tx(struct txq_msg_st *msg_st)
{
    return xomp_gw_req_memory_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                           msg_st->err);
}

static errval_t add_memory_response_tx(struct txq_msg_st *msg_st)
{
    return xomp_add_memory_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                        msg_st->err);
}

static errval_t done_notify_tx(struct txq_msg_st *msg_st)
{
    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;

    return xomp_done_notify__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                st->args.done_notify.id, msg_st->err);
}

static errval_t done_with_arg_tx(struct txq_msg_st *msg_st)
{
    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;

    return xomp_done_with_arg__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                  st->args.done_notify.id,
                                  st->args.done_notify.arg, msg_st->err);
}


/*
 * ----------------------------------------------------------------------------
 * XOMP channel receive handlers
 * ----------------------------------------------------------------------------
 */


static void gw_req_memory_call_rx(struct xomp_binding *b,
                                  uint64_t addr,
                                  uint8_t type)
{
    XWI_DEBUG("gw_req_memory_call_rx: addr:%lx, tyep: %u\n", addr, type);

#if XOMP_BENCH_WORKER_EN
    cycles_t mem_timer = bench_tsc();
#endif

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&txq);
    assert(msg_st != NULL);

    struct capref frame;
    if (type == XOMP_FRAME_TYPE_REPL_RW) {
        type = XOMP_FRAME_TYPE_SHARED_RW;
    }
    assert(!(worker_id & XOMP_WID_GATEWAY_FLAG));

    msg_st->send = gw_req_memory_response_tx;
    msg_st->cleanup = NULL;

    XWR_DEBUG("Requesting frame from gateway: [%016lx]\n", usrdata);

    msg_st->err = xomp_gateway_get_memory(addr, &frame);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }

    vregion_flags_t map_flags;

    switch ((xomp_frame_type_t) type) {
        case XOMP_FRAME_TYPE_MSG:
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RW:
        case XOMP_FRAME_TYPE_REPL_RW:
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RO:
            map_flags = VREGION_FLAGS_READ;
            break;
        default:
            USER_PANIC("unknown type: %u", type)
            break;
    }

    struct frame_identity id;
    msg_st->err = invoke_frame_identify(frame, &id);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }

    if (addr) {
        msg_st->err = vspace_map_one_frame_fixed_attr(addr, (1UL << id.bits),
                                                      frame, map_flags, NULL, NULL);
    } else {
        void *map_addr;
        msg_st->err = vspace_map_one_frame_attr(&map_addr, (1UL << id.bits),
                                                frame, map_flags, NULL, NULL);
    }

#if XOMP_BENCH_WORKER_EN
    mem_timer = bench_tsc() - mem_timer;
    debug_printf("%lx mem request %016lx took  %lu cycles, %lu ms\n", worker_id,
                 addr, mem_timer, bench_tsc_to_ms(mem_timer));
#endif

    txq_send(msg_st);
}

static void add_memory_call_rx(struct xomp_binding *b,
                               struct capref frame,
                               uint64_t addr,
                               uint8_t type)
{
    XWI_DEBUG("add_memory_call_rx: addr:%lx, tyep: %u\n", addr, type);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&txq);
    assert(msg_st != NULL);

    msg_st->send = add_memory_response_tx;
    msg_st->cleanup = NULL;

    uint32_t map_flags = 0x0;

    switch ((xomp_frame_type_t) type) {
        case XOMP_FRAME_TYPE_MSG:
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RW:
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RO:
            map_flags = VREGION_FLAGS_READ;
            break;
        default:
            USER_PANIC("unknown type: %u", type)
            break;
    }
    struct frame_identity id;
    msg_st->err = invoke_frame_identify(frame, &id);
    if(err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }

#if XOMP_WORKER_ENABLE_DMA
    if (0) {
        // todo: replicate frame on the same node if needed..
        replicate_frame(addr, &frame);
    }
#endif

#if XOMP_BENCH_WORKER_EN
    cycles_t map_start = bench_tsc();
#endif
    if (addr) {
        msg_st->err = vspace_map_one_frame_fixed_attr(addr, (1UL << id.bits),
                                                      frame, map_flags, NULL, NULL);
    } else {
        void *map_addr;
        msg_st->err = vspace_map_one_frame_attr(&map_addr, (1UL << id.bits),
                                                frame, map_flags, NULL, NULL);
    }
#if XOMP_BENCH_WORKER_EN
    cycles_t timer_end = bench_tsc();
    debug_printf("%lx mem map %016lx took  %lu cycles, %lu ms\n", worker_id, addr,
                     timer_end - map_start, bench_tsc_to_ms(timer_end - map_start));
#endif

    txq_send(msg_st);
}

static void do_work_rx(struct xomp_binding *b,
                       uint64_t fn,
                       uint64_t arg,
                       uint64_t id,
                       uint64_t flags)
{
    errval_t err;

    XWP_DEBUG("do_work_rx: fn:%lx, id:%lx\n", fn, id);

#if XOMP_BENCH_WORKER_EN
    cycles_t work_timer = bench_tsc();
#endif

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&txq);
    assert(msg_st != NULL);

    msg_st->err = SYS_ERR_OK;

    struct bomp_work *work = tls;

    XWP_DEBUG("do_work_rx: threadid = %u, nthreads = %u\n", work->thread_id,
              work->num_threads);

    g_bomp_state->num_threads = work->num_threads;

    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;
    st->args.done_notify.id = id;

    if (arg) {
        msg_st->send = done_with_arg_tx;
        st->args.done_notify.arg = arg;
    } else {
        msg_st->send = done_notify_tx;
    }

    if (fn & XOMP_FN_INDEX_FLAG) {
        uint32_t idx = fn & ~XOMP_FN_INDEX_FLAG;
        char *fn_name;
        err = spawn_symval_lookup_idx(idx, &fn_name, &fn);
        if (err_is_fail(err)) {
            msg_st->err = err;
            txq_send(msg_st);
            return;
        }
        XWP_DEBUG("do_work_rx: function index %u -> %s\n", idx, fn_name);
    }

    xomp_worker_fn_t fnct = (xomp_worker_fn_t) fn;
    XWP_DEBUG("do_work_rx: calling fnct %p with argument %p\n", fnct, work->data);

    for (uint32_t i = 0; i < work->num_vtreads; ++i) {
        fnct(work->data);
        work->thread_id++;
    }



#if XOMP_BENCH_WORKER_EN
    work_timer = bench_tsc() - work_timer;
    debug_printf("%lx work took %lu cycles, %lu ms\n", worker_id, work_timer,
                 bench_tsc_to_ms(work_timer));
#endif

    txq_send(msg_st);
}

static struct xomp_rx_vtbl rx_vtbl = {
    .gw_req_memory_call = gw_req_memory_call_rx,
    .add_memory_call = add_memory_call_rx,
    .do_work = do_work_rx
};

/*
 * ----------------------------------------------------------------------------
 * XOMP channel connect handler
 * ----------------------------------------------------------------------------
 */

/**
 * \brief XOMP channel connect callback called by the Flounder backend
 *
 * \param st    Supplied worker state
 * \param err   outcome of the connect attempt
 * \param xb    XOMP Flounder binding
 */
static void master_bind_cb(void *st,
                           errval_t err,
                           struct xomp_binding *xb)
{
    XWI_DEBUG("bound to master: %s\n", err_getstring(err));

    txq_init(&txq, xb, xb->waitset, (txq_register_fn_t) xb->register_send,
             sizeof(struct xomp_msg_st));

    xb->rx_vtbl = rx_vtbl;
    xbinding = xb;

    is_bound = 0x1;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief parses the command line arguments to extract
 *
 * \param argc  argument count
 * \param argv  argument values
 * \param wid   returns the XOMP worker ID
 *
 * \returns SYS_ERR_OK iff the command line arguments were parsed succecssfully
 *          XOMP_ERR_INVALID_WORKER_ARGS if there were no XOMP worker argumetnts
 *          errval on error
 *
 */
errval_t xomp_worker_parse_cmdline(uint8_t argc,
                                   char *argv[],
                                   xomp_wid_t *wid)
{
    XWI_DEBUG("xomp_worker_parse_cmdline\n");

    xomp_wid_t retwid = 0;
    uint8_t parsed = 0;
    uint8_t is_worker = 0x0;
    iref_t iref = 0x0;
    for (uint32_t i = 1; argv[i] != NULL; ++i) {
        if (strcmp(XOMP_WORKER_ARG, argv[i]) == 0) {
            parsed++;
            is_worker = 0x1;
        } else if (strncmp("--wid=", argv[i], 6) == 0) {
            retwid = strtoul(argv[i] + 6, NULL, 16);
            parsed++;
        } else if (strncmp("--iref=", argv[i], 7) == 0) {
            iref = strtoul(argv[i] + 7, NULL, 16);
            parsed++;
        }
    }

    if (!is_worker) {
        return XOMP_ERR_BAD_INVOCATION;
    }

    if (parsed < 2) {
        return XOMP_ERR_INVALID_WORKER_ARGS;
    }

    if (iref) {
        if (parsed != 3) {
            return XOMP_ERR_INVALID_WORKER_ARGS;
        }
        svc_iref = iref;
    }

    if (wid) {
        *wid = retwid;
    }

    return SYS_ERR_OK;
}

/**
 * \brief initializes the XOMP worker library
 *
 * \param wid   Xomp worker id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_worker_init(xomp_wid_t wid)
{
    errval_t err;

    worker_id = wid;

    XWI_DEBUG("initializing worker {%016lx} iref:%u\n", worker_id, svc_iref);

#if XOMP_BENCH_WORKER_EN
    bench_init();
#endif

    struct capref frame = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err_push(err, XOMP_ERR_INVALID_MSG_FRAME);
    }

    size_t frame_size = 0;

    if (svc_iref) {
        frame_size = XOMP_TLS_SIZE;
    } else {
        frame_size = XOMP_FRAME_SIZE;
        err = spawn_symval_cache_init(0);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if ((1UL << id.bits) < XOMP_TLS_SIZE) {
        return XOMP_ERR_INVALID_MSG_FRAME;
    }

    msgframe = frame;

    err = vspace_map_one_frame(&msgbuf, frame_size, frame, NULL, NULL);
    if (err_is_fail(err)) {
        err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
    }
    if (svc_iref) {
        tls = msgbuf;
    } else {
        tls = ((uint8_t *) msgbuf) + XOMP_MSG_FRAME_SIZE;
    }

    XWI_DEBUG("messaging frame mapped: [%016lx] @ [%016lx]\n", id.base,
              (lvaddr_t )msgbuf);

    struct bomp_thread_local_data *tlsinfo = malloc(sizeof(*tlsinfo));
    tlsinfo->thr = thread_self();
    tlsinfo->work = (struct bomp_work *) tls;
    tlsinfo->work->data = tlsinfo->work + 1;
    g_bomp_state->backend.set_tls(tlsinfo);

#ifdef __k1om__
    if (worker_id & XOMP_WID_GATEWAY_FLAG) {
        err = xomp_gateway_init();
    } else {
        if (!svc_iref) {
            err = xomp_gateway_bind_svc();
        } else {
            err = SYS_ERR_OK;
        }
    }
    if (err_is_fail(err)) {
        return err;
    }
#endif

#ifdef __k1om__
    if (!svc_iref) {
        err = xeon_phi_client_init(disp_xeon_phi_id());
        if (err_is_fail(err)) {
            err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
        }

        xeon_phi_client_set_callbacks(&callbacks);
    }
#endif

    struct waitset *ws = get_default_waitset();

// XXX: disabling DMA on the host as there is no replication used at this moment
#if XOMP_WORKER_ENABLE_DMA && defined(__k1om__)
    /* XXX: use lib numa */

#ifndef __k1om__
    uint8_t numanode = 0;
    if (disp_get_core_id() > 20) {
        numanode = 1;
    }

    err = dma_manager_wait_for_driver(dma_device_type, numanode);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not wait for the DMA driver");
    }
#endif
    char svc_name[30];
#ifdef __k1om__
    snprintf(svc_name, 30, "%s", XEON_PHI_DMA_SERVICE_NAME);
#else
    snprintf(svc_name, 30, "%s.%u", IOAT_DMA_SERVICE_NAME, numanode);
#endif

    struct dma_client_info dma_info = {
        .type = DMA_CLIENT_INFO_TYPE_NAME,
        .device_type = dma_device_type,
        .args.name = svc_name
    };
    err = dma_client_device_init(&dma_info, &dma_dev);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "DMA device initialization");
    }
#endif

    if (svc_iref) {
        err = xomp_bind(svc_iref, master_bind_cb, NULL, ws,
                        IDC_EXPORT_FLAGS_DEFAULT);
    } else {
        struct xomp_frameinfo fi = {
            .sendbase = id.base,
            .inbuf = ((uint8_t *) msgbuf) + XOMP_MSG_CHAN_SIZE,
            .inbufsize = XOMP_MSG_CHAN_SIZE,
            .outbuf = ((uint8_t *) msgbuf),
            .outbufsize = XOMP_MSG_CHAN_SIZE
        };
        err = xomp_connect(&fi, master_bind_cb, NULL, ws,
        IDC_EXPORT_FLAGS_DEFAULT);
    }

    if (err_is_fail(err)) {
        /* TODO: Clean up */
        return err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
    }

    XWI_DEBUG("Waiting until bound to master...\n");

    while (!is_bound) {
        messages_wait_and_handle_next();
    }

    if (xbinding == NULL) {
        return XOMP_ERR_WORKER_INIT_FAILED;
    }

    return SYS_ERR_OK;
}
