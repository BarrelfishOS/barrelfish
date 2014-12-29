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

#include <bomp_internal.h>

#include <if/bomp_defs.h>


/* forward declaration */
static int bomp_thread_msg_handler(void *arg);

///< stores the state of a message in transit
struct bomp_msg_st
{
    struct txq_msg_st common;       ///< common msg state

    uint32_t *message_sent;         ///<

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
 * Control Channel: Node Master Side
 * ==============================================================================
 */

/*
 * -----------------------------------------------------------------------------
 * RX Handlers
 * -----------------------------------------------------------------------------
 */

static void done__rx(struct bomp_binding *_binding, bomp_errval_t status)
{
    BOMP_DEBUG_THREAD("done__rx from thread\n");

    struct bomp_thread *t = _binding->st;

    struct bomp_node *node = t->node;


    node->threads_active--;

    BOMP_DEBUG_THREAD("threads active %u\n", node->threads_active);

}

/*
 * -----------------------------------------------------------------------------
 * TX Handlers
 * -----------------------------------------------------------------------------
 */

static void txq_msg_sent_cb(void *st)
{
    struct bomp_msg_st *msg_st = (struct bomp_msg_st *)st;
    *(msg_st->message_sent) = 1;
}

static errval_t execute__tx(struct txq_msg_st *msg_st)
{
    struct bomp_msg_st *st = (struct bomp_msg_st *)msg_st;

    return bomp_execute__tx(msg_st->queue->binding, TXQCONT(msg_st),
                            st->args.exec.fn, st->args.exec.arg, st->args.exec.tid,
                            st->args.exec.icv);
}



/**
 * \brief callback when the BOMP thread connects to the node
 *
 * \param st    state pointer
 * \param err   status of the connect
 * \param _b    created BOMP binding
 */
static void bomp_thread_accept_cb(void *st,
                                  errval_t err,
                                  struct bomp_binding *_b)
{
    struct bomp_thread *t = st;

    BOMP_DEBUG_THREAD("connection accepted. tid=%" PRIuCOREID "\n", t->coreid);

    t->thread_err = err;

    txq_init(&t->txq, _b, _b->waitset, (txq_register_fn_t) _b->register_send,
                 sizeof(struct bomp_msg_st));

    _b->st = st;
    t->ctrl = _b;

    _b->rx_vtbl.done = done__rx;
}

/**
 * \brief callback for creating the dispatcher on the remote core
 *
 * \param arg   argument for the callback
 * \param err   outcome of the spanning request
 */
static void bomp_thread_init_done(void *arg, errval_t err)
{
    assert(err_is_ok(err));

    uint32_t *done = arg;
    *done = 1;
}

/**
 * \brief initializes a thread on the given core
 *
 * \@param core         ID of the core on which to create the tread on
 * \param stack_size    size of the stack of the tread to be created
 * \param thread        pointer to the thread struct to create
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t bomp_thread_init(coreid_t core,
                          size_t stack_size,
                          struct bomp_thread *thread)
{
    errval_t err;

    BOMP_DEBUG_THREAD("Creating thread on core %"PRIuCOREID " \n", core);

    uint32_t done;

    err = domain_new_dispatcher(core, bomp_thread_init_done, &done);
    if (err_is_fail(err)) {
        BOMP_ERROR("creating new dispatcher on core %" PRIuCOREID "failed\n",
                   core);
        return err;
    }

    while(!done) {
        thread_yield();
    }

    BOMP_DEBUG_THREAD("dispatcher ready. allocating memory for msg channel\n");

    size_t msg_frame_size;
    err = frame_alloc(&thread->msgframe, 2 * BOMP_CHANNEL_SIZE, &msg_frame_size);
    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_map_one_frame(&thread->msgbuf, msg_frame_size, thread->msgframe,
                               NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    struct bomp_frameinfo fi = {
        .sendbase = (lpaddr_t)thread->msgbuf + BOMP_CHANNEL_SIZE,
        .inbuf = thread->msgbuf,
        .inbufsize = BOMP_CHANNEL_SIZE,
        .outbuf = ((uint8_t *) thread->msgbuf) + BOMP_CHANNEL_SIZE,
        .outbufsize = BOMP_CHANNEL_SIZE
    };

    BOMP_DEBUG_THREAD("creating channel on %p\n", thread->msgbuf);

    err = bomp_accept(&fi, thread, bomp_thread_accept_cb,
                      get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        // XXX> error handling
        return err;
    }

    BOMP_DEBUG_THREAD("creating thread on core %" PRIuCOREID "\n", core);
    err = domain_thread_create_on(core, bomp_thread_msg_handler, thread->msgbuf);
    if (err_is_fail(err)) {
        // XXX> error handling
        return err;
    }

    while (thread->ctrl == NULL) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "event dispatch\n");
        }
    }

    BOMP_DEBUG_THREAD("thread on core %" PRIuCOREID " connected \n", core);

    return thread->thread_err;
}

errval_t bomp_thread_exec(struct bomp_thread *thread,
                          bomp_thread_fn_t fn, void *arg, uint32_t tid)
{
    debug_printf("bomp_thread_exec(%p, %p, %p, %u) %p\n", thread, fn, arg, tid, thread->icvt);
    struct txq_msg_st *msg_st = txq_msg_st_alloc(&thread->txq);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    uint32_t msg_sent = 0;

    msg_st->send = execute__tx;
    msg_st->cleanup = (txq_cleanup_fn_t)txq_msg_sent_cb;

    struct bomp_msg_st *bomp_msg_st = (struct bomp_msg_st *)msg_st;

    bomp_msg_st->args.exec.arg = (uint64_t)arg;
    bomp_msg_st->args.exec.fn = (uint64_t)fn;
    bomp_msg_st->args.exec.tid = tid;
    bomp_msg_st->args.exec.icv = (uint64_t)thread->icvt;
    bomp_msg_st->message_sent = &msg_sent;

    txq_send(msg_st);

    while(msg_sent == 0) {
        event_dispatch(get_default_waitset());
    }

    //return event_dispatch_non_block(get_default_waitset());
    return SYS_ERR_OK;
}

/*
 * ==============================================================================
 * Control Channel: Worker Thread Side
 * ==============================================================================
 */

/*
 * -----------------------------------------------------------------------------
 * TX Handlers
 * -----------------------------------------------------------------------------
 */

static errval_t done__tx(struct txq_msg_st *msg_st)
{
    BOMP_DEBUG_THREAD("done__tx\n");

    return bomp_done__tx(msg_st->queue->binding, TXQCONT(msg_st),msg_st->err);
}

/*
 * -----------------------------------------------------------------------------
 * RX Handlers
 * -----------------------------------------------------------------------------
 */

static void execute__rx(struct bomp_binding *_binding,
                        uint64_t fn, uint64_t arg, uint32_t tid, uint64_t icv_task)
{


    struct bomp_thread *t = _binding->st;
    struct bomp_tls *tls = thread_get_tls();

    BOMP_DEBUG_THREAD("execute__rx: %p %p, %lx\n", t, tls, icv_task);

    assert(t == &tls->r.thread);

    struct omp_icv_task icvt;
    memcpy(&icvt, (void *)icv_task, sizeof(struct omp_icv_task));

    bomp_icv_set_task(&icvt);

    tls->thread_id = tid;

    bomp_thread_fn_t func= (bomp_thread_fn_t)fn;

    // calling the function
    func((void *)arg);

    bomp_icv_set_task(NULL);
    tls->thread_id = -1;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&t->txq);
    if (msg_st == NULL) {
        BOMP_ERROR("allocation of message state failed: %" PRIu32 "\n", tid);
        return;
    }

    msg_st->send = done__tx;
    msg_st->err = SYS_ERR_OK;

    txq_send(msg_st);
}

/**
 * \brief XOMP channel connect callback called by the Flounder backend
 *
 * \param st    Supplied worker state
 * \param err   outcome of the connect attempt
 * \param xb    XOMP Flounder binding
 */
static void bomp_thread_connect_cb(void *st,
                                   errval_t err,
                                   struct bomp_binding *b)
{
    struct bomp_thread *t = st;

    BOMP_DEBUG_THREAD("connected to node master.\n");

    t->ctrl = b;

    txq_init(&t->txq, b, b->waitset, (txq_register_fn_t) b->register_send,
             sizeof(struct bomp_msg_st));

    b->rx_vtbl.execute = execute__rx;
}


/**
 * \brief
 *
 * \param arg
 *
 * \return
 */
static int bomp_thread_msg_handler(void *arg)
{


    errval_t err;

    struct bomp_tls *tls = calloc(1, sizeof(struct bomp_tls));
    if (tls == NULL) {
        BOMP_ERROR("Could not allocate memory for TLS. %p\n", arg);
        return -1;
    }

    BOMP_DEBUG_THREAD("thread message handler started %p\n", tls);

    tls->role = BOMP_THREAD_ROLE_WORKER;
    tls->self = thread_self();
    tls->r.thread.coreid = disp_get_core_id();
    tls->r.thread.msgbuf = arg;
    tls->r.thread.tls = tls;

    struct waitset local_waitset;
    //struct waitset *ws = get_default_waitset();
    struct waitset *ws = &local_waitset;

    waitset_init(ws);


    struct bomp_frameinfo fi = {
        .sendbase = (lpaddr_t)arg,
        .inbuf = ((uint8_t *) arg) + BOMP_CHANNEL_SIZE,
        .inbufsize = BOMP_CHANNEL_SIZE,
        .outbuf = ((uint8_t *) arg),
        .outbufsize = BOMP_CHANNEL_SIZE
    };



    err = bomp_connect(&fi, bomp_thread_connect_cb, &tls->r.thread, ws,
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

    BOMP_NOTICE("thread %" PRIuCOREID " terminated", disp_get_core_id());


    return 0;
}
