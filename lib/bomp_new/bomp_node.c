/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/**
 * \brief this file contains the controll channel implementation between the
 *        node masters and the bomp worker threads
 */
#include <barrelfish/barrelfish.h>
#include <bomp_internal.h>
#include <bitmap.h>

/* forward declaration */
static int bomp_node_msg_handler(void *arg);

///< stores the state of a message in transit
struct bomp_msg_st
{
    struct txq_msg_st common;       ///< common msg state

    /* union of arguments */
    union {
        struct {
            uint64_t fn;            ///< the function
            uint64_t arg;           ///< the argument to the function
            uint32_t tid;           ///< thread ID
            uint64_t icv;           ///< thread's control variables
        } exec;                     ///< execution
    } args;
};

/*
 * ==============================================================================
 * Control Channel: Program Master Side
 * ==============================================================================
 */


static errval_t bomp_node_init_threads(nodeid_t nodeid,
                                       nodeid_t numanode,
                                       coreid_t nthreads,
                                       size_t stack_size,
                                       struct bomp_node *node)
{
    errval_t err;

    BOMP_DEBUG_NODE("Initialize worker threads for node %" PRIuNODEID " with %"
                    PRIuCOREID " threads\n", nodeid, nthreads);

    node->threads = calloc(nthreads, sizeof(struct bomp_thread));
    if (node->threads == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

#if 0
    struct bitmap *bm = numa_allocate_cpumask();
    err = numa_node_to_cpus(numanode, bm);
    assert(err_is_ok(err));
#else
    struct bitmap *bm = numa_all_cpus_ptr;
#endif


    coreid_t core = (coreid_t)bitmap_get_first(bm);

    for (coreid_t i = 0; i < nthreads; ++i) {
        BOMP_DEBUG_NODE("spanning to core %u\n", core);
        node->threads_max++;
        node->threads[i].node = node;
        if (core == disp_get_core_id()) {
            /* master thread */
            core = (coreid_t)bitmap_get_next(bm, core);
            continue;
        }

        err = bomp_thread_init(core, stack_size, &node->threads[i]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "creating thread\n");
            return err;
        }

        core = (coreid_t)bitmap_get_next(bm, core);
    }
    node->threads_active = 0;
    node->tls = thread_get_tls();

    return SYS_ERR_OK;
}

/**
 * \brief callback for creating the dispatcher on the remote core
 *
 * \param arg   argument for the callback
 * \param err   outcome of the spanning request
 */
static void bomp_node_init_done(void *arg, errval_t err)
{
    assert(err_is_ok(err));

    uint32_t *done = arg;
    *done = 1;
}


/**
 * \brief callback when the BOMP thread connects to the node
 *
 * \param st    state pointer
 * \param err   status of the connect
 * \param _b    created BOMP binding
 */
static void bomp_node_accept_cb(void *st,
                                errval_t err,
                                struct bomp_binding *_b)
{
    struct bomp_node *n = st;

    BOMP_DEBUG_NODE("connection accepted. tid=%" PRIuCOREID "\n", n->id);

    n->node_err = err;

    txq_init(&n->txq, _b, _b->waitset, (txq_register_fn_t) _b->register_send,
                 sizeof(struct bomp_msg_st));

    _b->st = st;
    n->ctrl = _b;

 //   _b->rx_vtbl.done = done__rx;
}

/* a node that is on our local address space */
static errval_t bomp_node_init_local(nodeid_t nodeid,
                                     nodeid_t numanode,
                                     coreid_t nthreads,
                                     size_t stack_size,
                                     struct bomp_node *node)
{
    BOMP_DEBUG_NODE("Initialize local node node %" PRIuNODEID " with %"
                     PRIuCOREID " threads\n", nodeid, nthreads);

    errval_t err;

    uint32_t done;

    node->id = nodeid;
    node->numa_node = numanode;
    node->threads_max = nthreads;
    node->stack_size = stack_size;
    node->threads_max = nthreads;

    struct bitmap *bm = numa_allocate_cpumask();
    err = numa_node_to_cpus(numanode, bm);
    assert(err_is_ok(err));

    coreid_t core = (coreid_t)bitmap_get_first(bm);

    err = domain_new_dispatcher(core, bomp_node_init_done, &done);
    if (err_is_fail(err)) {
        BOMP_ERROR("creating new dispatcher on core %" PRIuCOREID "failed\n",
                   core);
        return err;
    }

    while(!done) {
        thread_yield();
    }

    BOMP_DEBUG_NODE("dispatcher ready. allocating memory for msg channel\n");

    size_t msg_frame_size;
    err = frame_alloc(&node->msgframe, 2 * BOMP_CHANNEL_SIZE, &msg_frame_size);
    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_map_one_frame(&node->msgbuf, msg_frame_size, node->msgframe,
                               NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    struct bomp_frameinfo fi = {
        .sendbase = (lpaddr_t)node->msgbuf + BOMP_CHANNEL_SIZE,
        .inbuf = node->msgbuf,
        .inbufsize = BOMP_CHANNEL_SIZE,
        .outbuf = ((uint8_t *) node->msgbuf) + BOMP_CHANNEL_SIZE,
        .outbufsize = BOMP_CHANNEL_SIZE
    };

    BOMP_DEBUG_NODE("creating channel on %p\n", node->msgbuf);

    err = bomp_accept(&fi, node, bomp_node_accept_cb,
                      get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        // XXX> error handling
        return err;
    }

    BOMP_DEBUG_NODE("creating thread on core %" PRIuCOREID "\n", core);
    err = domain_thread_create_on(core, bomp_node_msg_handler, node, NULL);
    if (err_is_fail(err)) {
        // XXX> error handling
        return err;
    }

    while (node->ctrl == NULL) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "event dispatch\n");
        }
    }

    BOMP_DEBUG_NODE("node master on node %" PRIuNODEID " connected \n", nodeid);

    return node->node_err;
}

/* remote node: a node that is in a foreign address space */
static errval_t bomp_node_init_remote(nodeid_t nodeid,
                                      coreid_t nthreads,
                                      size_t stack_size,
                                      struct bomp_node *node)
{
    BOMP_DEBUG_NODE("Initialize remote node node %" PRIuNODEID " with %"
                    PRIuCOREID " threads\n", nodeid, nthreads);

    assert(!"NYI");
    return SYS_ERR_OK;
}


/**
 * \brief
 */
errval_t bomp_node_init(bomp_node_type_t type,
                        nodeid_t numanode,
                        nodeid_t nodeid,
                        coreid_t nthreads,
                        size_t stack_size,
                        struct bomp_node *node)
{
    node->type = type;

    switch(type) {
        case BOMP_NODE_MASTER :
            return bomp_node_init_threads(nodeid, numanode, nthreads, stack_size, node);
            break;
        case BOMP_NODE_LOCAL:
            return bomp_node_init_local(nodeid, numanode, nthreads, stack_size, node);
            break;
        case BOMP_NODE_REMOTE :
            return bomp_node_init_remote(nodeid, nthreads, stack_size, node);
            break;
        default:
            return -1;
            break;
    }
}



coreid_t bomp_node_exec(struct bomp_node *node, void *fn, void *arg, coreid_t tid_start, coreid_t nthreads)
{
    debug_printf("Executing on node %u\n", node->id);
    assert(!"NYI");
    return node->threads_max;

    return 0;
}

#if 0


/*
 * ==============================================================================
 * Control Channel: Node Master Side
 * ==============================================================================
 */

/**
 * \brief initializes the shared memory channel Node Master - Worker Threads
 *        (Worker Side)
 *
 * \param channel    address of the message buffers to use
 */
errval_t bomp_noded_channel_bind(void *channel)
{
    assert(!"NYI");

    return SYS_ERR_OK;
}

#endif

/**
 * \brief XOMP channel connect callback called by the Flounder backend
 *
 * \param st    Supplied worker state
 * \param err   outcome of the connect attempt
 * \param xb    XOMP Flounder binding
 */
static void bomp_node_connect_cb(void *st,
                                   errval_t err,
                                   struct bomp_binding *b)
{
    struct bomp_thread *t = st;

    BOMP_DEBUG_THREAD("connected to node master.\n");

    t->ctrl = b;

    txq_init(&t->txq, b, b->waitset, (txq_register_fn_t) b->register_send,
             sizeof(struct bomp_msg_st));

    //b->rx_vtbl.execute = execute__rx;
}

/**
 * \brief
 *
 * \param arg
 *
 * \return
 */
static int bomp_node_msg_handler(void *arg)
{
    BOMP_DEBUG_NODE("node master message handler started\n");

    errval_t err;

    struct bomp_tls *tls = calloc(1, sizeof(struct bomp_tls));
    if (tls == NULL) {
        BOMP_ERROR("Could not allocate memory for TLS. %p\n", arg);
        return -1;
    }

    struct bomp_node *node = arg;

    assert(numa_current_node() == node->numa_node);

    tls->role = BOMP_THREAD_ROLE_NODE;
    tls->self = thread_self();
    tls->r.node.id = node->id;
    tls->r.node.msgbuf = node->msgbuf;
    tls->r.node.tls = tls;
    tls->r.node.stack_size = node->stack_size;

    struct bomp_frameinfo fi = {
        .sendbase = (lpaddr_t)arg,
        .inbuf = ((uint8_t *) arg) + BOMP_CHANNEL_SIZE,
        .inbufsize = BOMP_CHANNEL_SIZE,
        .outbuf = ((uint8_t *) arg),
        .outbufsize = BOMP_CHANNEL_SIZE
    };

    struct waitset *ws = get_default_waitset();

    BOMP_DEBUG_NODE("initializing local worker threads\n");
    err = bomp_node_init_threads(node->id, node->numa_node, node->threads_max,
                                 node->stack_size, &tls->r.node);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init threads\n");
    }

    assert(node->threads_max == tls->r.node.threads_max);

    BOMP_DEBUG_NODE("connecting to program master\n");

    err = bomp_connect(&fi, bomp_node_connect_cb, &tls->r.thread, ws,
                       IDC_EXPORT_FLAGS_DEFAULT);


    if (err_is_fail(err)) {
        /* TODO: Clean up */
        return err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
    }

    thread_set_tls(tls);

    while(1) {
        err = event_dispatch_non_block(ws);
        switch(err_no(err)) {
            case LIB_ERR_NO_EVENT :
                thread_yield();
                continue;
                break;
            case SYS_ERR_OK:
                continue;
            break;
            default:
                USER_PANIC_ERR(err, "event dispatch");
                break;
        }
    }

    BOMP_NOTICE("node master %" PRIuNODEID " terminated", tls->r.node.id);


    return 0;
}
