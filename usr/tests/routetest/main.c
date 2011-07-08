/**
 * \file
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
#include <barrelfish/nameservice_client.h>
#include <if/monitor_defs.h>
#include <routing/routing.h>

static char my_name[128];
static coreid_t my_core_id;
static coreid_t num_cores;
routeid_t routeid;

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
    // 0 connected to 1, and can reach 1 or 2 through this link
    node[0].present = true;
    node[0].n[1].present = true;
    node[0].n[1].mask = (coremask_t)1 << 1 | (coremask_t)1 << 2;

    
    // 1 connected to 2, and can reach 0 or 2 through this link
    node[1].present = true;
    node[1].n[2].present = true;
    node[1].n[2].mask = (coremask_t)1 << 0 | (coremask_t)1 << 2;


    // 2 connected to 0, and can reach 0 or 1 through this link
    node[2].present = true;
    node[2].n[0].present = true;
    node[2].n[0].mask = (coremask_t)1 << 0 | (coremask_t)1 << 1;

}

void get_num_cores_qemu(void);
void get_num_cores_qemu(void)
{
    num_cores = 3;
}


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

static bool bcast_with_ccast_recv(void *route_state, routeid_t arg_routeid, 
                                  recordid_t recordid, void ** record_state,
                                  uint8_t *payload_arg,
                                  size_t size)
{
    assert (size == sizeof(struct payload));
    struct payload * payload = (struct payload *) payload_arg;
    printf("********bcast_with_ccast_recv on core %d from core %d\n", 
           my_core_id, payload->bcast_init_core);

    // setup ccast
    *record_state = malloc (sizeof(struct ccast_record));
    struct ccast_record * ccast_record = (struct ccast_record *)(*record_state);

    // dummy op was a success
    ccast_record->success = true;

    // send ccast (may wait in routing library until child reponses are aggregated)
    routing_send_ccast(arg_routeid, recordid, (uint8_t*)&ccast_record->success, 
                       sizeof(ccast_record->success));

    // forward bcast
    return true;
}

static bool ccast_recv(void *state, routeid_t arg_routeid, void * ccast_arg,
                       coreid_t children_remaining, uint8_t *payload,
                       size_t size) 
{
    struct ccast_record * ccast_record = (struct ccast_record *)ccast_arg;

    printf("********ccast_recv on core %d, payload %d\n", my_core_id, *payload);

    /* agregate response */
    ccast_record->success &= *payload;
    
    if (children_remaining == 0) {
        printf("********ccast_recv on core %d received all responses, sending payload %d\n",
               my_core_id, ccast_record->success);

        // update payload
        *payload = ccast_record->success;

        // don't require the record state any longer
        free(ccast_record);
        
        // send aggregated resonse to parent
        return true;
    } else {
        // otherwise wait for children
        return false;
    }
}

static void ccast_complete(void *state, routeid_t arg_routeid, void * ccast_arg,
                           uint8_t *payload_arg, size_t size) {
    
    printf("********ccast_complete from core %d, payload %d\n", my_core_id, 
           *payload_arg);
}
/* ------------------------ Connection / Init Related ----------------------- */

static void connect(void)
{
    errval_t err;

    struct neighbor * neighbors [MAX_CPUS];
    int neighbors_count = 0;

    printf("Core %d connecting\n", my_core_id);

    for (int i = 0; i < MAX_CPUS; i++) {
        if (!node[my_core_id].n[i].present) {
            continue;
        }

        printf("**%d is neighbor of %d\n", i, my_core_id);
        neighbors[neighbors_count++] = routing_new_neighbor(i, 
                                                  node[my_core_id].n[i].mask);  
    }
    err = routing_connect_to_neighbors(routeid, neighbors, neighbors_count);
    assert(err_is_ok(err));

    struct waitset *ws = get_default_waitset();
    while (!routing_connect_to_neighbors_done(routeid)) {
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

static bool spawn_request_done;

static void spawn_domain_reply(struct monitor_binding *st, errval_t err)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "spawn_domain_reply got an error");
    }

    assert(!spawn_request_done);
    spawn_request_done = true;
}

static errval_t spawn_other_cores(int argc, char *argv[]) {
    errval_t err;
    strcpy(my_name, argv[0]);
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.spawn_domain_reply = spawn_domain_reply;
    
    char args[128], route_id_char[32];
    strcpy(args, my_name);
    snprintf(route_id_char, sizeof(route_id_char), " %d", routeid);
    strcat(args, route_id_char);
    spawn_request_done = false;
    err = mb->tx_vtbl.spawn_domain_request(mb, NOP_CONT,
                                           my_name, args, 0, 2);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error spawning other core");
        abort();
    }
    while(!spawn_request_done) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/* ------------------------------------ MAIN ----------------------------- */

struct routing_cb_vtbl routing_vtbl = {
    .unicast_recv_cb          = NULL,
    .bcast_recv_cb            = bcast_recv,
    .bcast_with_ccast_recv_cb = bcast_with_ccast_recv,
    .ccast_recv_cb            = ccast_recv,
    .neighbor_connect_req_cb  = connect_request,
};

int main(int argc, char *argv[])
{
    errval_t err;
    // Initialize globals
    my_core_id = disp_get_core_id();

    // skb related functions
    initialize_list_qemu();
    get_num_cores_qemu();

    // join our route
    if (argc == 1) {
        err = routing_new_route(&routeid, routing_vtbl, NULL, true);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "alloc new route got an error");
            abort();
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
        err = routing_join_route(routeid, routing_vtbl, NULL, true);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "join new route got an error");
            abort();
        }
        
    }

    // connect to our neighbors
    connect();

    if (argc == 1) {
        // send a test broadcast with convergecast
        struct payload payload = {
            .bcast_init_core = my_core_id,
        };
        
        printf("Core %d sending bcast with ccast\n", my_core_id);
        
        // setup state for ccast callbacks
        struct ccast_record * ccast_record = malloc (sizeof(struct ccast_record));
        ccast_record->success = true;  // we think response should be true initially
        
        // perform broadcast, then wait for ccast to return
        err = routing_send_bcast_with_ccast(routeid, (uint8_t *)&payload,
                                            sizeof(payload), ccast_complete,
                                            ccast_record);
    }

    messages_handler_loop();

    return 0;
}
