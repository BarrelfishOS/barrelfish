/**
 * \file
 * \brief Routing benchmark of different trees
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
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <routing/routing.h>
#include <trace/trace.h>
#include <bench/bench.h>
#include <skb/skb.h>
#include <if/monitor_defs.h>
#include "queries.h"

static int idx;
static int num_cores;

static char my_name[128];
static coreid_t my_core_id;
static routeid_t routeid;

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
static bool num_cores_request;

static void num_cores_reply(struct monitor_binding *b, uint8_t num)
{
    num_cores = num;
    assert(!num_cores_request);
    num_cores_request = true;
}

#if 0
static void set_skb_present(char *str)
{
    while (*str != '\0') {
        if (!isdigit(*str)) {
            str++;
            continue;
        }
        strtol(str, &str, 10);
        num_cores++;
    }
}
#endif

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
                node[coreid].n[neighbor].mask = (coremask_t)1 << neighbor;

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

static __attribute__((unused)) void skb_init(void)
{
    errval_t err;

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

    /* Get the list of APIC IDs */

    num_cores_request = false;
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.num_cores_reply = num_cores_reply;
    err = mb->tx_vtbl.num_cores_request(mb, NOP_CONT);
    assert(err_is_ok(err));
    while (!num_cores_request) {
        messages_wait_and_handle_next();
    }

    char *result;
    result = queries[num_cores][idx].str[1];
    assert(result);
    process_result(result);

/*     for (int i = 0; i < num_cores; i++) { */
/*         result = queries[num_cores][idx].str[i]; */
/*         assert(result); */
/*         process_result(result); */
/*     } */

#endif
}

/* ---------------------------- SKB RELATED FUNCTIONS DONE------------------- */

/* ---------------------------- EXPERIMENT START ------------------- */
#define MAX_COUNT 100
struct timestamp {
    cycles_t time0;
    cycles_t time1;
    cycles_t time2;
    cycles_t time3;
};
static struct timestamp timestamps[MAX_COUNT];
static uint64_t global_payload;

static void ccast_complete(void *state, routeid_t arg_routeid, void * ccast_arg,
                           uint8_t *payload, size_t size);

static void experiment(void)
{
    errval_t err;
    static int i = 0;
    static bool flag = false;

    printf("experiment %d\n", i);

    if (i == MAX_COUNT / 10 + 2) {
        trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_STOP, 0);
    }

    // Experiment finished
    if (i == MAX_COUNT - 1) {

#if CONFIG_TRACE
        char *buf = malloc(4096*4096);
        trace_dump(buf, 4096*4096);
        printf("%s\n", buf);
#endif

        for (int j = MAX_COUNT / 10; j < MAX_COUNT - 1; j++) {
            printf("%d total %" PRIuCYCLES "\n", j,
                   timestamps[j].time3 - timestamps[j].time0);
        }

        printf("client done\n");
        return;
    }

    if (!flag) { // Start experiment

#if CONFIG_TRACE
        err = trace_control(TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                        TRACE_EVENT_ROUTE_BENCH_START, 0),
                            TRACE_EVENT(TRACE_SUBSYS_ROUTE,
                                        TRACE_EVENT_ROUTE_BENCH_STOP, 0), 0);
        assert(err_is_ok(err));
#endif

        

        timestamps[i].time0 = bench_tsc();
        flag = true;
    } else { // Continue experiment
        timestamps[i].time3 = bench_tsc();
        timestamps[i+1].time0 = timestamps[i].time3;
        i++;
    }

    if (i == MAX_COUNT / 10) {
        trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_ROUTE_BENCH_START, 0);
    }

    //printf("experiment: sending\n");
    // perform broadcast, then wait for ccast to return
    err = routing_send_bcast_with_ccast(routeid, (uint8_t *)&global_payload,
                                        sizeof(global_payload), ccast_complete,
                                        NULL);
    assert(err_is_ok(err));

}

static void ccast_complete(void *state, routeid_t arg_routeid, void * ccast_arg,
                           uint8_t *payload_arg, size_t size)
{
    //printf("********ccast_complete from core %d\n", my_core_id);
    experiment();
}

static bool bcast_with_ccast_recv(void *route_state, routeid_t arg_routeid,
                                  recordid_t recordid, void ** record_state,
                                  uint8_t *payload,
                                  size_t size)
{
    //printf("********bcast_with_ccast_recv on core %d \n", my_core_id);

    // send ccast (may wait in routing library until child reponses are aggregated)
    routing_send_ccast(arg_routeid, recordid,(uint8_t *)&global_payload,
                       sizeof(global_payload));

    // forward bcast
    return true;
}

static bool ccast_recv(void *state, routeid_t arg_routeid, void * ccast_arg,
                       coreid_t children_remaining, uint8_t *payload,
                       size_t size)
{
    //printf("********ccast_recv on core %d\n", my_core_id);


    if (children_remaining == 0) {
        //printf("********ccast_recv on core %d received all responses\n", my_core_id);

        // send same payload as receieved onto parent
        return true;
    } else {
        // otherwise wait for children
        return false;
    }
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

    char indexstr[16], routeidstr[16];
    snprintf(indexstr, sizeof(indexstr), "%d", idx);
    indexstr[sizeof(indexstr) - 1] = '\0';
    snprintf(routeidstr, sizeof(routeidstr), "%d", routeid);
    routeidstr[sizeof(routeidstr) - 1] = '\0';
    char *argv[] = {my_name, indexstr, routeidstr, NULL};

    err = spawn_program_on_all_cores(false, my_name, argv, NULL,
                                     SPAWN_FLAGS_DEFAULT, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error spawning other core");
        abort();
    }

    return SYS_ERR_OK;
}

/* ------------------------------------ Emulated drop-in code ----------------------------- */

void initialize_list_qemu(void);
void initialize_list_qemu(void)
{
    node[0].present = true;
    node[0].n[1].present = true;
    node[0].n[1].mask = (coremask_t)1 << 1;
    node[0].n[2].present = true;
    node[0].n[2].mask = (coremask_t)1 << 2;
    node[0].n[3].present = true;
    node[0].n[3].mask = (coremask_t)1 << 3;
    node[0].n[4].present = true;
    node[0].n[4].mask = (coremask_t)1 << 4;
    node[0].n[5].present = true;
    node[0].n[5].mask = (coremask_t)1 << 5;
    node[0].n[6].present = true;
    node[0].n[6].mask = (coremask_t)1 << 6;
    node[0].n[7].present = true;
    node[0].n[7].mask = (coremask_t)1 << 7;
    /* node[0].n[8].present = true; */
    /* node[0].n[8].mask = (coremask_t)1 << 8; */
    /* node[0].n[9].present = true; */
    /* node[0].n[9].mask = (coremask_t)1 << 9; */
    /* node[0].n[10].present = true; */
    /* node[0].n[10].mask = (coremask_t)1 << 10; */
    /* node[0].n[11].present = true; */
    /* node[0].n[11].mask = (coremask_t)1 << 11; */
    /* node[0].n[12].present = true; */
    /* node[0].n[12].mask = (coremask_t)1 << 12; */
    /* node[0].n[13].present = true; */
    /* node[0].n[13].mask = (coremask_t)1 << 13; */
    /* node[0].n[14].present = true; */
    /* node[0].n[14].mask = (coremask_t)1 << 14; */
    /* node[0].n[15].present = true; */
    /* node[0].n[15].mask = (coremask_t)1 << 15; */
    /* node[0].n[16].present = true; */
    /* node[0].n[16].mask = (coremask_t)1 << 16; */
    /* node[0].n[17].present = true; */
    /* node[0].n[17].mask = (coremask_t)1 << 17; */
    /* node[0].n[18].present = true; */
    /* node[0].n[18].mask = (coremask_t)1 << 18; */
    /* node[0].n[19].present = true; */
    /* node[0].n[19].mask = (coremask_t)1 << 19; */
    /* node[0].n[20].present = true; */
    /* node[0].n[20].mask = (coremask_t)1 << 20; */
    /* node[0].n[21].present = true; */
    /* node[0].n[21].mask = (coremask_t)1 << 21; */
    /* node[0].n[22].present = true; */
    /* node[0].n[22].mask = (coremask_t)1 << 22; */
    /* node[0].n[23].present = true; */
    /* node[0].n[23].mask = (coremask_t)1 << 23; */
}

void get_num_cores_qemu(void);
void get_num_cores_qemu(void)
{
    /* num_cores = 24; */
    num_cores = 8;
}

/* ------------------------------------ MAIN ----------------------------- */

struct routing_cb_vtbl routing_vtbl = {
    .unicast_recv_cb          = NULL,
    .bcast_with_ccast_recv_cb = bcast_with_ccast_recv,
    .ccast_recv_cb            = ccast_recv,
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
    my_core_id = disp_get_core_id();
    printf("%d, running\n", my_core_id);

    if (argc == 1) {
        printf("Not enough cmdline args, quiting\n");
        abort();
    }

    idx = strtol(argv[1], NULL, 10);

    bench_init();
    queries_init();
    skb_init();
    /* initialize_list_qemu(); */
    /* get_num_cores_qemu(); */

    // join our route
    if (my_core_id == 1) {
        assert(argc == 2);

        join_complete = false;
        err = routing_new_route(&routeid, routing_vtbl, NULL,
                                joined_route, true);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "alloc new route got an error");
            abort();
        }
        // wait for route join to complete
        while (!join_complete) {
            messages_wait_and_handle_next();
        }

        // spawn other cores
        err = spawn_other_cores();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error in bsp spawning other core");
            abort();
        }

        connect();

    } else {
        assert(argc == 3);
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
        while (!join_complete) {
            messages_wait_and_handle_next();
        }
    }

    // run experiments
    if (my_core_id == 1) {
        printf("%d turning on nagling\n", my_core_id);

        printf("radix is %d\n", queries[num_cores][idx].radix);
        experiment();
    }

    messages_handler_loop();

    return 0;
}
