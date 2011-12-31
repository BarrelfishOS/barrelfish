/**
 * \file
 * \brief Routing benchmark of a barrier
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <routing/routing.h>
#include <bench/bench.h>
#include <trace/trace.h>
#include <skb/skb.h>
#include "queries.h"

static int idx;
static int num_cores;

static bool is_bsp;
static char my_name[128];
static coreid_t my_core_id;
static routeid_t routeid;
static uint64_t msg[2];
static uint64_t start_time;

#define TIME_OFFSET     10000000UL

enum MsgTypes {
    START_TIME,
    BARRIER_DONE
};

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

static void set_skb_present(char *str)
{
    while (*str != '\0') {
        if (!isdigit((int)*str)) {
            str++;
            continue;
        }
        strtol(str, &str, 10);
        num_cores++;
    }
}

static void process_neighbor(char *result, int parent, int neighbor)
{
    char *p = strchr(result, 'r');

    while (1) {
        if (p == NULL || *p == '\n') {
            break;
        }

        // Get the coreid
        p = strchr(p, '(');
        p++;
        int coreid = strtol(p, &p, 10);

        // If not #neighbor, continue
        if (coreid != neighbor) {
            p = strchr(p, 'r');
            continue;
        }

        // Increment to reachable
        p = strchr(p, '[');
        p++;

        // Empty reachable
        if (!isdigit((int)*p)) {
            break;
        }

        while(1) {
            int reachable = strtol(p, &p, 10);
            //printf("neighbor %d can reach %d\n", neighbor, reachable);
            node[parent].n[neighbor].mask |= (coremask_t)1 << reachable;
            if (*p == ',') {
                p+=2;
            } else {
                break;
            }
        }
    }
}

static void process_result(char *result)
{
    char *p = strchr(result, 'r');
    while(1) {
        if (p == NULL || *p == '\n') {
            break;
        }

        // increment over route(
        p = strchr(p, '(');
        p++;

        // coreid
        int coreid = strtol(p, &p, 10);
        //printf("core %d is present\n", coreid);
        node[coreid].present = true;

        // Increment to neighbors
        p = strchr(p, ']');
        p = strchr(p, '[');
        p++;

        if (isdigit((int)*p)) { // Has neighbors
            while(1) {
                int neighbor = strtol(p, &p, 10);
                //printf("%d has neighbor %d\n", coreid, neighbor);
                node[coreid].n[neighbor].present = true;
                node[coreid].n[neighbor].mask |= (coremask_t)1 << neighbor;

                // process the coremask of the neighbor
                process_neighbor(result, coreid, neighbor);

                if (*p == ',') {
                    p +=2;
                } else {
                    break;
                }
            }
        }
        p = strchr(p, 'r');
    }
}

static __attribute__ ((unused)) void skb_init(void)
{
    errval_t err;
    char *result, *str_err;
    int int_err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_client_connect failed");
        abort();
    }

    /* Wait for pci to finish ACPI enumeration.
       This uses the nameserver as a lock server.
       When pci is done enumeration, it will add this to the server.
    */
    iref_t iref;
    err = nameservice_blocking_lookup("pci_discovery_done", &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "chips_blocking_lookup failed");
        exit(EXIT_FAILURE);
    }

#if 0
    /* Get a route */
    if (is_bsp) { // bsp core will load the prolog code
        err = skb_evaluate("[route_tree_radix].", &result, &str_err, &int_err);
        assert(err_is_ok(err));
        assert(int_err == 0);
        free(result);
        free(str_err);
    }

    char query[2048];
    snprintf(query, 2048, "route_allcores(%d,0,R),write(R).", radix);

    while (1) {
        err = skb_evaluate(query, &result, &str_err, &int_err);
        assert(err_is_ok(err));
        if (int_err == 0) {
            free(result);
            free(str_err);
            break;
        }
        printf("%d running query again query %s result %s str_err %s int_err %d\n",
               my_core_id, query, result, str_err, int_err);
        free(result);
        free(str_err);
    }

#else

    //Get the list of APIC IDs
    err = skb_evaluate("get_apic_id_list(L),write(L).",
                       &result, &str_err, &int_err);
    assert(err_is_ok(err));
    set_skb_present(result);
    free(result);
    free(str_err);

    result = queries[num_cores][idx].str[0];
    process_result(result);
    result = queries[num_cores][idx].str[1];
    process_result(result);
    result = queries[num_cores][idx].str[2];
    process_result(result);
    result = queries[num_cores][idx].str[3];
    process_result(result);

#endif
}

/* ---------------------------- SKB RELATED FUNCTIONS DONE------------------- */

/* ---------------------------- EXPERIMENT START ------------------- */
#define MAX_COUNT 100
struct timestamp {
    cycles_t time0;
    cycles_t time1;
};
static struct timestamp timestamps[MAX_COUNT];
static uint64_t global_payload;

// According to Akhi this has to be set to zero,
// "or the routing library will go crazy!"
#define COORDINATOR 0

struct barrier {
    coreid_t max;
    coreid_t count;
};
static struct barrier barrier;
static bool barrier_done;

static void barrier_wait(void)
{
    errval_t err;

    if (my_core_id == COORDINATOR) { // Coordinator
        // Accumulate msgs from all cores
        barrier.count++;
        while (barrier.count != barrier.max) {
            messages_wait_and_handle_next();
        }
        barrier.count = 0;

        /* Send msg to every core */
        msg[0] = BARRIER_DONE;
        err = routing_send_bcast(routeid, (uint8_t *)msg, 16);
        assert(err_is_ok(err));
    } else { // Non coordinator
        // Send msg to coordinator
        barrier_done = false;
        err = routing_send_unicast(routeid, COORDINATOR, (uint8_t *)&global_payload,
                                   8);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "calling routing_send_unicast()");
        }
        assert(err_is_ok(err));

        // Wait for done msg
        while (!barrier_done) {
            messages_wait_and_handle_next();
        }
    }
}

static void experiment(void)
{
    static int i = 0;

    while(bench_tsc() < start_time);
    timestamps[i].time0 = bench_tsc();
    barrier_wait();
    timestamps[i].time1 = bench_tsc();
    start_time += TIME_OFFSET;

    // Experiment finished
    if (i == MAX_COUNT - 1) {

        trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_STOP, 0);

#if CONFIG_TRACE
        char *buf = malloc(4096*4096);
        trace_dump(buf, 4096*4096);
        printf("%s\n", buf);
#endif

        for (int j = MAX_COUNT / 10; j < MAX_COUNT; j++) {
            printf("page %d took %zu\n", j,
                   timestamps[j].time1 - timestamps[j].time0);
        }

        printf("client done\n");
        abort();
        return;
    }
    i++;
}

static bool bcast_recv(void *state, routeid_t rid, uint8_t *payload, size_t size)
{
    uint64_t *rmsg = (void *)payload;

    switch(rmsg[0]) {
    case START_TIME:
        start_time = rmsg[1];
        break;

    case BARRIER_DONE:
        assert(my_core_id != COORDINATOR);
        barrier_done = true;
        break;
    }

    return true;
}

static void unicast_recv(void *state, routeid_t rid, uint8_t *payload, size_t size)
{
    assert(my_core_id == COORDINATOR);
    barrier.count++;
}

/* ---------------------------- EXPERIMENT DONE ------------------- */

/* ------------------------ Connection / Init Related ----------------------- */

static bool connect_complete;

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
                printf("**%d is child of %d on bcast from %d\n", i, my_core_id, init_core);

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

static errval_t spawn_other_cores(void)
{
    errval_t err;

    char indexstr[10];
    snprintf(indexstr, sizeof(indexstr), "%d", idx);
    indexstr[sizeof(indexstr) - 1] = '\0';

    char routeidstr[10];
    snprintf(routeidstr, sizeof(routeidstr), "%d", routeid);
    routeidstr[sizeof(routeidstr) - 1] = '\0';

    char *argv[] = {my_name, indexstr, routeidstr, NULL};
    err = spawn_program_on_all_cores(false, my_name, argv, NULL,
                                     SPAWN_FLAGS_DEFAULT, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error spawning other core");
    }

    return SYS_ERR_OK;
}

/* ------------------------------------ Emulated drop-in code ----------------------------- */

void initialize_list_qemu(void);
void initialize_list_qemu(void)
{
    // 0 connected to 1, and can reach 1 or 2 or 3 through this link
    node[0].present = true;
    node[0].n[1].present = true;
    node[0].n[1].mask = (coremask_t)1 << 1 | (coremask_t)1 << 2 | (coremask_t)1 << 3;

    
    // 1 connected to 2, and can reach 0 or 2 or 3 through this link
    node[1].present = true;
    node[1].n[2].present = true;
    node[1].n[2].mask = (coremask_t)1 << 0 | (coremask_t)1 << 2 | (coremask_t)1 << 3;

    // 3 connected to 0, and can reach 0 or 1 or 2 through this link
    node[3].present = true;
    node[3].n[0].present = true;
    node[3].n[0].mask = (coremask_t)1 << 0 | (coremask_t)1 << 1 | (coremask_t)1 << 2;


    // 2 connected to 3, and can reach 0 or 1 or 3 through this link
    node[2].present = true;
    node[2].n[3].present = true;
    node[2].n[3].mask = (coremask_t)1 << 0 | (coremask_t)1 << 1 | (coremask_t)1 << 3;

}

void get_num_cores_qemu(void);
void get_num_cores_qemu(void)
{
    num_cores = 4;
}

/* ------------------------------------ MAIN ----------------------------- */

struct routing_cb_vtbl routing_vtbl = {
    .unicast_recv_cb          = unicast_recv,
    .bcast_recv_cb            = bcast_recv,
    .neighbor_connect_req_cb  = connect_request,
};

static bool join_complete;

static void joined_route(void * rs, routeid_t id, errval_t err) {
    assert(err_is_ok(err));
    join_complete = true;
}

int main(int argc, char *argv[])
{
    errval_t err;

    strcpy(my_name, argv[0]);
    idx = strtol(argv[1], NULL, 10);

    // Initialize globals
    my_core_id = disp_get_core_id();

    printf("%d running\n", my_core_id);

    bench_init();
    queries_init();

    // skb related functions
    skb_init();
/*     initialize_list_qemu(); */
/*     get_num_cores_qemu(); */

    // Init barrier
    barrier.max   = num_cores;
    barrier.count = 0;

    // join our route
    if (argc == 2) {
        is_bsp = true;
        join_complete = false;
        err = routing_new_route(&routeid, routing_vtbl, NULL,
                                joined_route, true);
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
        err = spawn_other_cores();

        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error in bsp spawning other core");
            abort();
        }
    } else {
        is_bsp = false;
        join_complete = false;
        // get route id from argument
        routeid = atoi(argv[2]);
        err = routing_join_route(routeid, routing_vtbl, NULL,
                                 joined_route, true);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "join new route got an error");
            abort();
        }
        // wait for route join to complete
        struct waitset *ws = get_default_waitset();
        while (!join_complete) {
            printf("%d is waiting\n", my_core_id);
            err = event_dispatch(ws);
            assert(err_is_ok(err));
        }

        while (start_time == 0) {
            err = event_dispatch(ws);
            assert(err_is_ok(err));
        }
    }

    // connect to our neighbors
    connect();

    // run experiments
    if (is_bsp) {
        printf("radix is %d\n", queries[num_cores][idx].radix);

        // Tell everyone when to start
        start_time = bench_tsc() + TIME_OFFSET;
        msg[0] = START_TIME;
        msg[1] = start_time;
        err = routing_send_bcast(routeid, (uint8_t *)msg, 16);
        assert(err_is_ok(err));

#if CONFIG_TRACE
        trace_reset_all();
        err = trace_control(TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                        TRACE_EVENT_ROUTE_BENCH_START, 0),
                            TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                        TRACE_EVENT_ROUTE_BENCH_STOP, 0), 10 * 30 * 2000000);
        assert(err_is_ok(err));
#endif

        trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_START, 0);

        for(;;) {
            experiment();
        }
    } else {
        // Go into endless barriers
        for(;;) {
            while(bench_tsc() < start_time);
            barrier_wait();
            start_time += TIME_OFFSET;
        }
    }

    messages_handler_loop();

    return 0;
}
