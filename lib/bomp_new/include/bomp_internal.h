/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 64, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __LIBBOMP_INTERNAL_H
#define __LIBBOMP_INTERNAL_H

#include <barrelfish/barrelfish.h>
#include <flounder/flounder_txqueue.h>

#include <numa.h>
#include <omp.h>

#include <omp_abi.h>
#include <omp_icv.h>
#include <omp_environment.h>

#include <bomp_debug.h>


#include <if/bomp_defs.h>


///< size of the control channel in bytes
#define BOMP_CHANNEL_SIZE 2048

///< bomp thread id
typedef uint32_t bomp_tid_t;

/// the maximum size of a execution node
#define BOMP_NODE_SIZE_MAX -1


///< BOMP node types
typedef enum bomp_node_type {
    BOMP_NODE_INVALID,  ///< the node has an invalid type
    BOMP_NODE_MASTER,   ///< this is the program master node
    BOMP_NODE_LOCAL,    ///< the node is a address space local node
    BOMP_NODE_REMOTE    ///< the node is on a different address space
} bomp_node_type_t;

typedef enum bomp_thread_role {
    BOMP_THREAD_ROLE_INVALID, ///< Invalid thread type
    BOMP_THREAD_ROLE_WORKER,  ///< Normal worker thread
    BOMP_THREAD_ROLE_NODE,    ///< Node coordinator thread
    BOMP_THREAD_ROLE_MASTER   ///< Program master thread (initial thread)
} bomp_thread_role_t;

///< type of the execute function of BOMP
typedef void (*bomp_thread_fn_t)(void *);

/**
 * state of a normal BOMP thread
 */
struct bomp_thread {
    coreid_t id;                 ///< id of the thread
    coreid_t coreid;             ///< id of the core the thread runs on
    struct omp_icv_task *icvt;   ///<
    struct bomp_tls *tls;
    size_t stack_size;           ///< size of the stack
    void *msgbuf;                ///< message buffer for this frame
    struct capref msgframe;      ///< backing frame for the message buffer
    struct bomp_node *node;      ///< the node this threads belongs to
    errval_t thread_err;         ///< stores the error in case of failure
    struct bomp_binding *ctrl;   ///< control channel
    struct tx_queue txq;         ///< Flounder TX queue
};

/**
 * state of a BOMP node coordinator
 */
struct bomp_node {
    nodeid_t id;                  ///< the id of the execution node
    nodeid_t numa_node;           ///< numa node id
    bomp_node_type_t type;        ///< type of this node
    struct bomp_tls *tls;         ///< pointer to the thread local storage
    coreid_t threads_max;         ///< the number of threads of this node
    coreid_t threads_active;      ///< the number of active threads on this node
    size_t stack_size;           ///< size of the stack
    struct bomp_thread *threads;  ///< pointer to the local threads array
    struct bomp_binding *ctrl;    ///< control channel
    void *msgbuf;                 ///< message buffer for this frame
    struct capref msgframe;       ///< backing frame for the message buffer
    errval_t node_err;            ///< error of the code
    struct tx_queue txq;          ///< Flounder TX queue
};


/**
 * \brief stores the state of the BOMP master thread
 *
 * The BOMP master thread is also a node coordinator of its own node.
 */
struct bomp_master {
    nodeid_t num_nodes;          ///< the number of nodes in the system
    struct bomp_node *nodes;     ///< array of nodes to other
    coreid_t threads_max;        ///< the maximum number of threads in the system
    coreid_t nodes_active;       ///< the number of active threads in the system
    struct bomp_node local;      ///< the local node
};

struct bomp_binding *ctrl;   ///< control channel
    struct tx_queue txq;         ///< Flounder TX queue


struct bomp_work {
    coreid_t thread_id;
};

struct bomp_tls {
    struct thread *self;        ///< pointer ot the struct thread
    struct omp_icv icv;             ///< pointer holding the environment variables
    coreid_t thread_id;
    bomp_thread_role_t role;     ///< identifies the role of the thread
    union {
        struct bomp_master master;
        struct bomp_node   node;
        struct bomp_thread thread;
    } r;

};


errval_t bomp_node_init(bomp_node_type_t type, nodeid_t numanode, nodeid_t nodeid, coreid_t nthreads,
                        size_t stack_size, struct bomp_node *node);
coreid_t bomp_node_exec(struct bomp_node *node, void *fn, void *arg, coreid_t tid_start, coreid_t nthreads);

errval_t bomp_thread_init(coreid_t core, size_t stack_size, struct bomp_thread *thread);

errval_t bomp_thread_exec(struct bomp_thread *thread,
                          bomp_thread_fn_t fn, void *arg, uint32_t tid);


void bomp_start_processing(void (*fn)(void *),
                           void *data,
                           coreid_t tid_start,
                           coreid_t nthreads);
void bomp_end_processing(void);

/**
 * \brief obtaining a pointer to the control variables
 *
 * \return pointe to the ICV struct
 */
static inline struct omp_icv *bomp_icv_get(void)
{
    struct bomp_tls *tls = thread_get_tls();
    return &tls->icv;
}

static inline void bomp_icv_set_task(struct omp_icv_task *task)
{
    struct bomp_tls *tls = thread_get_tls();
    tls->icv.task = task;
}

static inline struct omp_icv_task *bomp_icv_get_task(void)
{
    struct bomp_tls *tls = thread_get_tls();
    return tls->icv.task;
}



#if 0
/**
 * \brief this struct stores thread local data such as the team / task
 *        of this thread
 */
struct bomp_thread
{
    bomp_thread_fn_t fn;
    void *arg;

    struct bomp_task *task;

};


struct bomp_work {
    void (*fn)(void *);
    void *data;
    unsigned thread_id;
    unsigned num_threads;
    unsigned num_vtreads;
    struct bomp_barrier *barrier;
};


struct bomp_thread_local_data {
    void *thr; // thread reference
    struct bomp_work *work;
    struct bomp_icv_data *icv;
};
#endif




#endif/* _LIBBOMP_H */
