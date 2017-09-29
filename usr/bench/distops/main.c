/**
 * \file
 * \brief Distops common server/client framework
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/bench_distops_defs.h>

#include "benchapi.h"

//{{{1 Shared local state

static const char *service_name = "bench_distops_svc";
static coreid_t my_core_id = -1;

//{{{1 Mgmt node

//{{{2 Mgmt node: benchmark-independent state

struct benchmark_state {
    int clients_seen;
    int clients_total;
    struct bench_distops_binding **nodes;
    void *st;
};

struct mgmt_node_state {
    void *global;
    uint32_t coreid;
    void *st;
};

// Only for mgmt node
static struct benchmark_state *bench_state = NULL;

//{{{2 Mgmt node: broadcast helper functions

void broadcast_cmd(uint32_t cmd, uint32_t arg)
{
    errval_t err;
    if (!bench_state) {
        printf("Benchmark not initialized, cannot broadcast\n");
        return;
    }
    if (bench_state->clients_seen < bench_state->clients_total) {
        printf("Not all clients registered, broadcast not yet possible\n");
        return;
    }
    for (int i = 0; i < bench_state->clients_total; i++) {
        assert(bench_state->nodes[i]);
        err = bench_distops_cmd__tx(bench_state->nodes[i], NOP_CONT, cmd, arg);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "sending cmd msg to binding %p\n", bench_state->nodes[i]);
        }
    }
    return;
}

void broadcast_caps(uint32_t cmd, uint32_t arg, struct capref cap1)
{
    errval_t err;
    if (!bench_state) {
        printf("Benchmark not initialized, cannot broadcast\n");
        return;
    }
    if (bench_state->clients_seen < bench_state->clients_total) {
        printf("Not all clients registered, broadcast not yet possible\n");
        return;
    }
    for (int i = 0; i < bench_state->clients_total; i++) {
        assert(bench_state->nodes[i]);
        err = bench_distops_caps__tx(bench_state->nodes[i], NOP_CONT, cmd, arg, cap1);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "sending caps msg to binding %p\n", bench_state->nodes[i]);
        }
    }
    return;
}

//{{{2 Mgmt node multicast helper functions
void multicast_caps(uint32_t cmd, uint32_t arg, struct capref cap1,
                    coreid_t *cores, int corecount)
{
    errval_t err;
    if (!bench_state) {
        printf("Benchmark not initialized, cannot multicast\n");
        return;
    }
    if (bench_state->clients_seen < bench_state->clients_total) {
        printf("Not all clients registered, multicast not yet possible\n");
        return;
    }
    for (int i = 0; i < bench_state->clients_total; i++) {
        assert(bench_state->nodes[i]);
        struct mgmt_node_state *ns = bench_state->nodes[i]->st;
        for (int c = 0; c < corecount; c++) {
            if (cores[c] == ns->coreid) {
                err = bench_distops_caps__tx(bench_state->nodes[i], NOP_CONT,
                        cmd, arg, cap1);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "sending caps msg to binding %p\n", bench_state->nodes[i]);
                }
            }
        }
    }
    return;
}

//#define USE_NODEID_AS_ID
//{{{2 Mgmt node unicast helper functions
void unicast_cmd(coreid_t nodeid, uint32_t cmd, uint32_t arg)
{
    errval_t err;
    if (!bench_state) {
        printf("Benchmark not initialized, cannot unicast\n");
        return;
    }
    if (bench_state->clients_seen < bench_state->clients_total) {
        printf("Not all clients registered, unicast not yet possible\n");
        return;
    }
#ifdef USE_NODEID_AS_ID
    for (int i = 0; i < bench_state->clients_total; i++) {
        assert(bench_state->nodes[i]);
        struct mgmt_node_state *ns = bench_state->nodes[i]->st;
        if (ns->coreid == nodeid) {
            err = bench_distops_cmd__tx(bench_state->nodes[i], NOP_CONT, cmd, arg);
            PANIC_IF_ERR(err, "cmd unicast");
            break;
        }
    }
#else
    // use local index, to keep client code unchanged
    struct mgmt_node_state *ns = bench_state->nodes[nodeid-1]->st;
    printf("# %s: sending cmd %d, to node %d\n",
            __FUNCTION__, cmd, ns->coreid);
    err = bench_distops_cmd__tx(bench_state->nodes[nodeid-1], NOP_CONT, cmd, arg);
#endif
    return;
}

//{{{2 Mgmt node message handlers

static void mgmt_rx_hello(struct bench_distops_binding *b, uint32_t coreid)
{
    DEBUG("mgmt rx_hello from core %"PRIu32"\n", coreid);

    bench_state->clients_seen ++;

    struct mgmt_node_state *ns = b->st;
    ns->coreid = coreid;

    if (bench_state->clients_seen == bench_state->clients_total) {
        mgmt_run_benchmark(bench_state->st);
    }
}

static void mgmt_rx_cmd(struct bench_distops_binding *b, uint32_t cmd, uint32_t arg)
{
    DEBUG("server rx_cmd %"PRIu32"\n", arg);

    // Execute command requested by node: implemented in <distop.c>
    mgmt_cmd(cmd, arg, b);
}

static void mgmt_rx_caps(struct bench_distops_binding *b, uint32_t cmd, uint32_t arg,
                         struct capref cap1)
{
    // Execute receive caps sent by node: implemented in <distop.c>
    mgmt_cmd_caps(cmd, arg, cap1, b);
}

static struct bench_distops_rx_vtbl mgmt_rx_vtbl = {
    .hello = mgmt_rx_hello,
    .cmd = mgmt_rx_cmd,
    .caps = mgmt_rx_caps,
};

// {{{2 Mgmt node export & connect

static void export_cb(void *st, errval_t err, iref_t iref)
{
    PANIC_IF_ERR(err, "export failed");

    printf("service exported at iref %"PRIuIREF"\n", iref);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    PANIC_IF_ERR(err, "nameservice_register failed");
}

static errval_t connect_cb(void *st, struct bench_distops_binding *b)
{
    printf("service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = mgmt_rx_vtbl;

    // Create a server state struct for this connection
    b->st = malloc(sizeof(struct mgmt_node_state));
    assert(b->st);
    if (!b->st) {
        USER_PANIC("state malloc() in mgmt node");
    }
    struct mgmt_node_state *ns = b->st;
    ns->global = bench_state;

    errval_t err = mgmt_init_node(&ns->st);
    PANIC_IF_ERR(err, "init node");

    // add node binding to list of bindings, for later broadcasts, we don't
    // care about ordering here, so don't assume that node at index i is
    // running on core i.
    static int nidx = 0;
    bench_state->nodes[nidx++] = b;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void run_benchmark(int nodecount)
{
    errval_t err;

    bench_state = malloc(sizeof(*bench_state));
    if (!bench_state) {
        USER_PANIC("malloc failed");
    }

    // Initialize benchmark state
    bench_state->clients_seen = 0;
    bench_state->clients_total = nodecount;
    bench_state->nodes = malloc(nodecount * sizeof(struct bench_distops_binding *));
    if (!bench_state->nodes) {
        USER_PANIC("malloc failed");
    }

    err = mgmt_init_benchmark(&bench_state->st, nodecount);
    PANIC_IF_ERR(err, "init benchmark");

    err = bench_distops_export(NULL,
            export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
    PANIC_IF_ERR(err, "export failed");
}

void *get_global_state(struct bench_distops_binding *b)
{
    struct mgmt_node_state *ns = b->st;
    struct benchmark_state *bs = ns->global;
    return bs->st;
}

//{{{1 Node

//{{{2 Node message handlers

// We use test.basic to signal that server has done it's retypes
static void node_rx_cmd(struct bench_distops_binding *b, uint32_t cmd, uint32_t arg)
{
    DEBUG("client rx_cmd %"PRIu32": b->st = %p\n", arg, b->st);

    // handle cmd message on node: implemented in <distop.c>
    node_cmd(cmd, arg, b);
}

static void node_rx_caps(struct bench_distops_binding *b, uint32_t cmd, uint32_t arg,
                         struct capref cap1)
{
    DEBUG("node %d rx_caps: cmd=%"PRIu32"\n", my_core_id, cmd);

    // handle rx_caps message on node: implemented in <distop.c>
    node_cmd_caps(cmd, arg, cap1, b);
}

static struct bench_distops_rx_vtbl client_rx_vtbl = {
    .cmd = node_rx_cmd,
    .caps = node_rx_caps,
};

//{{{2 Node bind

static void bind_cb(void *st, errval_t err, struct bench_distops_binding *b)
{
    PANIC_IF_ERR(err, "bind failed");

    printf("node %d bound!\n", my_core_id);

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = client_rx_vtbl;

    // Initialize node state: implemented in <distop>.c
    init_node(b);

    // Send hello message
    printf("%s: node %d sending hello msg\n", __FUNCTION__, my_core_id);
    err = bench_distops_hello__tx(b, NOP_CONT, my_core_id);
    PANIC_IF_ERR(err, "in node %d: sending cap to server", my_core_id);
}

static void run_node(void)
{
    errval_t err;
    iref_t iref;

    printf("node %d looking up '%s' in name service...\n", my_core_id, service_name);
    err = nameservice_blocking_lookup(service_name, &iref);
    PANIC_IF_ERR(err, "nameservice_blocking_lookup failed");

    printf("node %d binding to %"PRIuIREF"...\n", my_core_id, iref);

    err = bench_distops_bind(iref, bind_cb, NULL, get_default_waitset(),
                             IDC_BIND_FLAGS_DEFAULT);
    PANIC_IF_ERR(err, "bind failed");
}

//{{{1 Main
int main(int argc, char *argv[])
{
    char *role;

    my_core_id = disp_get_core_id();

#ifndef NDEBUG
    printf("Running with assertions ENABLED!!!\n");
#endif

    // TODO: more args
    if (argc < 2) {
        printf("distops benchmark needs an argument of \"mgmt\" or \"node\"\n");
        return 1;
    }
    if (strncmp(argv[1], "mgmt", 4) == 0) {
        printf("we are the orchestrator\n");
        role = "server";
        if (argc < 3) {
            printf("Orchestrator needs number of nodes as 2nd argument\n");
            return 2;
        }
        run_benchmark(atoi(argv[2]));
    }
    if (strncmp(argv[1], "node", 4) == 0) {
        printf("we are node %d\n", my_core_id);
        role = "client";
        run_node();
    }

    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch(ws);
        PANIC_IF_ERR(err, "in %s: event_dispatch", role);
    }

    return 0;
}
