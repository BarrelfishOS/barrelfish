/**
 * \file  Very simple routetest for a fully connected 4x4 broadcast network
 * \brief
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

#define NEIGHBORS_COUNT 4

static char my_name[128];
static coreid_t my_core_id;
routeid_t routeid;

struct payload {
    coreid_t bcast_init_core;
};

/* ----------------------- Routing Library Callbacks ----------------------- */

static bool bcast_recv(void *state, routeid_t arg_routeid, uint8_t *payload_arg,
                       size_t size)
{
    assert (size == sizeof(struct payload));
    struct payload * payload = (struct payload *) payload_arg;
    printf("********bcast_recv on core %d from core %d\n", my_core_id, 
           payload->bcast_init_core);

    // forward bcast
    return true;
}

/* ------------------------ Connection / Init Related ----------------------- */

static bool connect_done;

static void route_connected(void *rs, routeid_t id, errval_t err) {
    assert(err_is_ok(err));
    connect_done = true;
}

static void connect(void)
{
    errval_t err;

    struct neighbor * neighbors [NEIGHBORS_COUNT];
    int neighbors_count = 0;

    printf("Core %d connecting\n", my_core_id);

    // connect to all other cores directly (no child links)
    for (int i = 0; i < NEIGHBORS_COUNT; i++) {
        if (i != my_core_id) {
            printf("**%d is neighbor of %d\n", i, my_core_id);
            neighbors[neighbors_count++] = routing_new_neighbor(i, 1<<i);
        }  
    }
    connect_done = false;
    err = routing_connect_to_neighbors(routeid, neighbors, neighbors_count,
                                       route_connected);
    assert(err_is_ok(err));

    struct waitset *ws = get_default_waitset();
    while (!connect_done) {
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
    // allow connection, use defaults for children (none) and ccast_parent (same
    // as bcast parent)
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

    err = spawn_program_on_all_cores(false, my_name, xargv, NULL,
                                     SPAWN_FLAGS_DEFAULT, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error spawning other core");
        abort();
    }

    return SYS_ERR_OK;
}

/* ------------------------------------ MAIN ----------------------------- */

struct routing_cb_vtbl routing_vtbl = {
    .unicast_recv_cb          = NULL,
    .bcast_recv_cb            = bcast_recv,
    .bcast_with_ccast_recv_cb = NULL,
    .ccast_recv_cb            = NULL,
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

    // join our route
    if (argc == 1) {
        // we are the primary domain
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
        // we are a slave domain, get route id from argument
        routeid = atoi(argv[1]);
        join_complete = false;
        err = routing_join_route(routeid, routing_vtbl, NULL,
                                 route_joined, true);
        // wait for route join to complete
        struct waitset *ws = get_default_waitset();
        while (!join_complete) {
            err = event_dispatch(ws);
            assert(err_is_ok(err));
        }
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "join new route got an error");
            abort();
        }
        
    }

    // connect to our neighbors
    connect();

    if (argc == 1) {
        // send a test broadcast
        struct payload payload = {
            .bcast_init_core = my_core_id,
        };
        
        printf("Core %d sending bcast\n", my_core_id);
        
        // perform broadcast
        err = routing_send_bcast(routeid, (uint8_t *)&payload, sizeof(payload));
    }

    messages_handler_loop();

    return 0;
}
