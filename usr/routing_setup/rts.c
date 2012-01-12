/** \file
 *  \brief Routing table set-up dispatcher
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
#include <if/monitor_defs.h>
#include <skb/skb.h>

// state of the rts
static int num_cores = 0;
static coreid_t **routing_table;
static coreid_t current_core = 0;

// are we done yet? RTS exits if this flag is set to true
static int done = false;

/**
 * The routing table set-up dispatcher (RTS) is responsible to create the routing
 * table (for multi-hop messaging) and send them to the first monitor that
 * is booted. It uses informations from the System Knowledge Base (SKB).
 * The routing table is used to determine where to forward a multi-hop channel
 * set-up request.
 *
 * We currently support three routing modes:
 *
 * 1) DIRECT:   Always take the direct path
 *
 * 2) RING:     Route over all cores in the system.
 *              Core i will route to core (i + 1) mod num_cores
 *
 * 3) FAT_TREE: Route direct between cores on the same CPU socket. On each socket, 
 *              there is a "leader" (core with lowest ID on that socket). We 
 *              route directly between all leader. Routes between sockets 
 *              lead through the two leaders.
 *
 */

// the used routing mode --> change this
enum {
    MULTIHOP_ROUTE_DIRECT, MULTIHOP_ROUTE_RING, MULTIHOP_ROUTE_FAT_TREE
} routing_mode = MULTIHOP_ROUTE_DIRECT;

// send the routing table to the monitor
static void rts_send_routing_table_to_monitor(void *arg)
{

    errval_t err;
    struct monitor_binding *b = get_monitor_binding();
    assert(current_core <= num_cores);

    if (current_core == num_cores) {
        // let everybody know that we are done by registering rts_done
        // with the nameservice
        err = nameservice_register("rts_done", 0);
        assert(err_is_ok(err));
        done = true;
        return;
    }

    // send a part of the routing table
    err = monitor_multihop_routing_table_set_request__tx(b, NOP_CONT, num_cores,
            current_core, routing_table[current_core],
            sizeof(coreid_t) * num_cores);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                    MKCONT(rts_send_routing_table_to_monitor, NULL));
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "routing-setup: could not send routing table to monitor\n");
    } else {
        current_core++;
    }
}

// handler for acknowledgments from the monitor
static void rts_routing_table_ack(struct monitor_binding *b)
{
    // continue sending the routing table
    rts_send_routing_table_to_monitor(NULL);
}

/* ------------------------------ MAIN ------------------------------ */

int main(int argc, char *argv[])
{

    errval_t err;
    iref_t iref;
    char *result, *str_err;
    int int_err;

    if (routing_mode == MULTIHOP_ROUTE_DIRECT) {
        // don't do anything, as direct routing is anyway
        // the default

        err = nameservice_register("rts_done", 0);
        assert(err_is_ok(err));
        return EXIT_SUCCESS;

    } else if (routing_mode == MULTIHOP_ROUTE_RING
            || routing_mode == MULTIHOP_ROUTE_FAT_TREE) {
        // we need to use the SKB for those two cases

        // Wait for pci to finish ACPI enumeration.
        // This uses the nameserver as a lock server.
        err = nameservice_blocking_lookup("pci_discovery_done", &iref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
        }

        // connect to the system knowledge base (SKB)
        err = skb_client_connect();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "skb_client_connect failed");
            abort();
        }

        // get the number of cores from SKB
        err = skb_evaluate("available_nr_cores(Nr),write(Nr).", &result,
                &str_err, &int_err);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not get number of cores from SKB\n");
            abort();
        } else if (int_err != 0) {
            USER_PANIC("could not get number of cores from SKB: %s\n", str_err);
            abort();
        }

        num_cores = atoi(result);
        printf("routing-setup: discovered number of cores: %d\n", num_cores);
        free(str_err);
        free(result);

        if (routing_mode == MULTIHOP_ROUTE_RING) {
            // we have enough information for this case, construct routing table
            routing_table = malloc(sizeof(coreid_t *) * num_cores);
            for (coreid_t i = 0; i < num_cores; i++) {
                routing_table[i] = malloc(sizeof(coreid_t) * num_cores);
                for (coreid_t j = 0; j < num_cores; j++) {
                    routing_table[i][j] = (i + 1) % num_cores;
                }
            }
        } else {
            // we need to know the number of cores per socket
            int cores_per_socket = 0;
            err = skb_evaluate("setof(C,cpu_affinity(C,_,A),Set),length(Set,L),write(L).", &result,
                    &str_err, &int_err);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(
                        err,
                        "routing_setup: could not get number of cores per socket from SKB\n");
            } else if (int_err != 0) {
                // information about CPU affinity is not present in the SKB
                // use 1 as default
                cores_per_socket = 1;
                printf(
                        "routing_setup: could not find information about CPU affinity in SKB, using one core per socket\n");
            } else {
                cores_per_socket = atoi(result);
                printf("routing-setup: discovered number of cores per socket: %d\n", cores_per_socket);
            }
            free(str_err);
            free(result);

            // construct routing table
            routing_table = malloc(sizeof(coreid_t *) * num_cores);
            for (coreid_t i = 0; i < num_cores; i++) {

                routing_table[i] = malloc(sizeof(coreid_t) * num_cores);

                if (i % cores_per_socket == 0) {
                    // this is a master node --> always route to the master of a socket...
                    for (coreid_t j = 0; j < num_cores; j++) {
                      routing_table[i][j] = j - (j % cores_per_socket);
                    }

                    // ... except in our subtree, where we create a full mesh
                    for (coreid_t j = i; j < i + cores_per_socket; j++) {
                    routing_table[i][j] = j;
                    }

                } else {
                    // this node is not the master of a socket
 
                    // current master node
                    coreid_t master = i - (i % cores_per_socket);

                    // we always route to our master...
                    for (coreid_t j = 0; j < num_cores; j++) {
                        routing_table[i][j] = master;
                    }

                    // ... except in our subtree, where we create a full mesh
                    for (coreid_t j = master; j < master + cores_per_socket; j++) {
                    routing_table[i][j] = j;
                    }
                }
            }
        }
    } else {
        USER_PANIC("routing_setup: unknown routing mode\n");
    }

    // send send the routing table to the monitor
    assert(routing_table != NULL);
    current_core = 0;
    get_monitor_binding()->rx_vtbl.multihop_routing_table_response =
            &rts_routing_table_ack;
    rts_send_routing_table_to_monitor(NULL);

    // handle messages
    struct waitset *ws = get_default_waitset();
    while (!done) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
    return EXIT_SUCCESS;
}
