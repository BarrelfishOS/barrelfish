/**
 * \file
 * \brief Routing microbenchmark
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <routing/routing.h>
#include <bench/bench.h>

#define MAX_COUNT 1000
struct timestamp {
    cycles_t time0;
    cycles_t time1;
};
static struct timestamp timestamps[MAX_COUNT];

static bool connect_complete;

static char my_name[128];
static coreid_t my_core_id;
static coreid_t num_cores;
static routeid_t routeid;
static uint8_t global_payload;

/* ------------------------------------ LIST ----------------------------- */

struct connect_neighbor {
    bool present;
    coremask_t mask;
};

struct node {
    bool present;
    struct connect_neighbor n[MAX_CPUS];
};

static struct node node[MAX_CPUS];

struct payload {
    coreid_t bcast_init_core;
};

struct ccast_record {
    bool success;
};

/* ---------------------------- SKB RELATED FUNCTIONS ----------------------- */

void initialize_list_qemu(void);
void initialize_list_qemu(void)
{
    // 0 connected to 1, and can reach 1 through this link
    node[0].present = true;
    node[0].n[1].present = true;
    node[0].n[1].mask = (coremask_t)1 << 1;

    
    // 1 connected to 0, and can reach 0 through this link
    node[1].present = true;
    node[1].n[0].present = true;
    node[1].n[0].mask = (coremask_t)1 << 0;
}

void get_num_cores_qemu(void);
void get_num_cores_qemu(void)
{
    num_cores = 2;
}

static void experiment(void)
{
    errval_t err;
    static int i = 0;
    static bool flag = false;

    // Experiment finished
    if (i == MAX_COUNT - 1) {
        timestamps[i].time1 = bench_tsc();

        for (int j = MAX_COUNT / 10; j < MAX_COUNT; j++) {
            printf("page %d took %" PRIuCYCLES "\n", j,
                   timestamps[j].time1 - bench_tscoverhead() -
                   timestamps[j].time0);
        }

        printf("client done\n");
        return;
    }

    if (!flag) { // Start experiment
        timestamps[i].time0 = bench_tsc();
        flag = true;
    } else { // Continue experiment
        timestamps[i].time1 = bench_tsc();
        timestamps[i+1].time0 = timestamps[i].time1;
        i++;
    }

    err = routing_send_bcast(routeid, &global_payload, 1);
    assert(err_is_ok(err));
}


/* ----------------------- Routing Library Callbacks ----------------------- */

static bool bcast_recv(void *state, routeid_t arg_routeid, uint8_t *payload_arg,
                       size_t size)
{
    errval_t err;
    if (my_core_id == 1) {
        struct waitset *ws = get_default_waitset();
        while (!connect_complete) {
            err = event_dispatch(ws);
            assert(err_is_ok(err));
        }

        err = routing_send_bcast(routeid, &global_payload, 1);
        assert(err_is_ok(err));
    } else {
        experiment();
    }

    // forward bcast
    return true;
}

/* ------------------------ Connection / Init Related ----------------------- */

static void connect_done(void *rs, routeid_t id, errval_t err) {
    assert(err_is_ok(err));
    connect_complete = true;
}

static void connect(void)
{
    errval_t err;

    struct neighbor * neighbors [MAX_CPUS];
    int neighbors_count = 0;

    printf("Core %d connecting\n", my_core_id);
    connect_complete = false;

    for (int i = 0; i < MAX_CPUS; i++) {
        if (!node[my_core_id].n[i].present) {
            continue;
        }

        printf("**%d is neighbor of %d\n", i, my_core_id);
        neighbors[neighbors_count++] = routing_new_neighbor(i, 
                                                  node[my_core_id].n[i].mask);  
    }
    err = routing_connect_to_neighbors(routeid, neighbors, neighbors_count,
                                       connect_done);
    assert(err_is_ok(err));

    struct waitset *ws = get_default_waitset();
    while (!connect_complete) {
        err = event_dispatch(ws);
        assert(err_is_ok(err));
    }
    printf("Core %d connected\n", my_core_id);
}


static bool connect_request(void *state, routeid_t arg_routeid, coreid_t init_core, 
                            coreid_t bcast_parent, coremask_t childcores, 
                            struct neighbor ** children, coreid_t * children_count, 
                            struct neighbor ** ccast_parent) 
{
    assert(arg_routeid = routeid);
    
    // remove myself from reachable childcores on this link
    childcores = childcores & ~(1 << my_core_id);

    // connect to next hop for each of the children reachable on this link
    if (childcores != 0) {
        *children_count = 0;
        
        for (int i = 0; (i < MAX_CPUS) && childcores; i++) {
            if (!node[my_core_id].n[i].present) {
                continue;
            }

            if (node[my_core_id].n[i].mask & childcores) {
                printf("**%d is child of %d on bcast from %d\n", i, my_core_id,
                       init_core);

                // this some of the children are reachable from this neighbor
                coremask_t reachable_mask = node[my_core_id].n[i].mask & childcores;
                children[(*children_count)++] = routing_new_neighbor(i, 
                                                                reachable_mask);

                // remove these children from remaining childcores
                childcores = childcores &~ reachable_mask;
            }
        }
    }
    
    // leave ccast_parent the same as bcast_parent

    // allow connection
    return true;
}

/* ------------------------- Domain spawning code -------------------------- */

static errval_t spawn_other_cores(int argc, char *argv[]) {
    errval_t err;
    strcpy(my_name, argv[0]);

    char route_id_char[32];
    snprintf(route_id_char, sizeof(route_id_char), " %d", routeid);
    route_id_char[sizeof(route_id_char) - 1] = '\0';

    char *xargv[] = {my_name, route_id_char, NULL};
    err = spawn_program(1, my_name, xargv, NULL,
                        SPAWN_FLAGS_DEFAULT, NULL);
    if (err_is_fail(err)) {
	USER_PANIC_ERR(err, "error spawning other core");
    }

    return SYS_ERR_OK;
}

/* ------------------------------------ MAIN ----------------------------- */

struct routing_cb_vtbl routing_vtbl = {
    .unicast_recv_cb          = NULL,
    .bcast_recv_cb            = bcast_recv,
    .neighbor_connect_req_cb  = connect_request,
};

static bool join_complete;

static void route_joined(void * rs, routeid_t id, errval_t err) {
    assert(err_is_ok(err));
    join_complete = true;
}

int main(int argc, char *argv[])
{
    errval_t err;
    // Initialize globals
    my_core_id = disp_get_core_id();

    bench_init();

    // skb related functions
    initialize_list_qemu();
    get_num_cores_qemu();

    // join our route
    if (argc == 1) {
        join_complete = false;
        err = routing_new_route(&routeid, routing_vtbl, NULL,
                                route_joined, true);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "alloc new route got an error");
            abort();
        }
        // wait for route join to complete
        struct waitset *ws = get_default_waitset();
        while (!join_complete) {
            err = event_dispatch(ws);
            assert(err_is_ok(err));
        }

        // spawn other cores
        err = spawn_other_cores(argc, argv);
        
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error in bsp spawning other core");
            abort();
        }
    } else {
        // get route id from argument
        routeid = atoi(argv[1]);
        join_complete = false;
        err = routing_join_route(routeid, routing_vtbl, NULL,
                                 route_joined, true);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "join new route got an error");
            abort();
        }
        // wait for route join to complete
        struct waitset *ws = get_default_waitset();
        while (!join_complete) {
            err = event_dispatch(ws);
            assert(err_is_ok(err));
        }
        
    }

    // connect to our neighbors
    connect();

    if (argc == 1) {
        
        printf("Running route_lib between core %d and core %d\n", my_core_id, 1);
        experiment();
    }

    messages_handler_loop();

    return 0;
}
