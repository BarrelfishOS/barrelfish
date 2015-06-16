/**
 * \file
 * \brief General Numa functions
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>

#include <numa.h>

#include "numa_internal.h"

/**
 * \brief dumps the numa topology structure
 *
 * \param topology pointer to the topology to dump
 */
void numa_dump_topology(struct numa_topology *topology)
{
    if (topology->nodes == NULL) {
        printf("NUMA TOPOLOGY INVALID\n");
        return;
    }

    printf("dumping NUMA topology\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n\n");

    printf("Cores: %" PRIuCOREID "  Nodes: %" PRIuNODEID" \n", topology->num_cores,
           topology->num_nodes);

    printf("---------------------------------------\n");
    for (nodeid_t nodeid = 0; nodeid < topology->num_nodes; ++nodeid) {
        struct numa_node *node = &topology->nodes[nodeid];
        printf(" # Node %" PRIuNODEID ":  [0x%016" PRIxLPADDR ", 0x%016" PRIxLPADDR
               "] of %" PRIu64 " MB\n", nodeid, node->mem_base, node->mem_limit,
               (node->mem_limit - node->mem_base) >> 20);
        for (coreid_t coreid = 0; coreid < node->num_cores; ++coreid) {
            struct numa_core *core = &node->cores[coreid];
            printf("  + Core %-3" PRIuCOREID ": [apic=%-3" PRIu16 ", node=%-3"
                        PRIuNODEID "]\n", core->id, core->apicid, core->node->id);
        }
    }

    printf("---------------------------------------\n");
    for (coreid_t coreid = 0; coreid < topology->num_cores; ++coreid) {
        struct numa_core *core = topology->cores[coreid];
        printf(" # Core %-3" PRIuCOREID ": [apic=%-3" PRIu16 ", node=%-3"
               PRIuNODEID "]\n", coreid, core->apicid, core->node->id);
    }

    printf("---------------------------------------\n");
    printf("Locality map:\n");
    printf("     ");
    for (coreid_t node_from = 0; node_from < topology->num_nodes; ++node_from) {
        printf("%02" PRIuNODEID " ", node_from);
    }
    printf("\n");
    for (coreid_t node_from = 0; node_from < topology->num_nodes; ++node_from) {
        printf("  %02" PRIuNODEID  " ", node_from);
        for (coreid_t node_to = 0; node_to < topology->num_nodes; ++node_to) {
            printf("%02" PRIu32 " ",
                   topology->distances[node_from * topology->num_nodes + node_to]);
        }
        printf("\n");
    }

    printf("\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n");
}

/**
 * \brief obtains the system topology from the SKB
 *
 * \param topology pointer to the topology information structure
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_get_topology_from_skb(struct numa_topology *topology)
{
    errval_t err;

    /* don't query if no return pointer is specified */
    if (topology == NULL) {
        return SYS_ERR_OK;
    }

    NUMA_DEBUG_INIT("getting topology from SKB...\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return err_push(err, NUMA_ERR_SKB);
    }

    err = skb_execute_query("get_system_topology(Nnodes,Ncores,Lnodes,Lcores,Llocalities),"
                            "writeln(num(nodes(Nnodes),cores(Ncores))),"
                            "writeln(Lnodes),writeln(Lcores), writeln(Llocalities).");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb query failed");
        return err_push(err, NUMA_ERR_SKB);
    }

    uint32_t core = 0;
    uint32_t node = 0;
    err = skb_read_output("num(nodes(%d), cores(%d))", &node, &core);
    if (err_is_fail(err)) {
        return err_push(err, NUMA_ERR_SKB_DATA);
    }

    NUMA_DEBUG_INIT("discovered topology with %" PRIuNODEID " nodes, %" PRIuCOREID
                    " cores\n", node, core);

    if (!core || !node || node > core) {
        NUMA_ERROR("invalid number of cores %" PRIu32 " or nodes %" PRIu32 ".",
                   core, node);
        return err_push(err, NUMA_ERR_SKB_DATA);
    }

    if (core >= MAX_COREID || node >= NUMA_MAX_NUMNODES) {
        NUMA_ERROR("too large number of cores %" PRIu32 " or nodes %" PRIu32 ".",
                   core, node);
        return err_push(err, NUMA_ERR_SKB_DATA);
    }

    topology->num_cores = (coreid_t) core;
    topology->num_nodes = (nodeid_t) node;

    topology->preferred = NUMA_POLICY_DEFAULT;
    topology->strict = NUMA_POLICY_DEFAULT;

    topology->nodes = malloc(node * sizeof(struct numa_node)
                                + node * node * sizeof(uint32_t)
                                + core * sizeof(struct numa_core)
                                + core * sizeof(void *));
    if (topology->nodes == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct numa_core *cores_array = (struct numa_core *)(topology->nodes + node);

    topology->cores = (struct numa_core **) (cores_array + core);
    topology->distances = (uint32_t*)(topology->cores + core);

    /* skip over the initial node and core information */
    char *output = strchr(skb_get_output(), '\n') + 1;
    uint32_t parsed = 0;

    /* read the node list */
    struct list_parser_status parser;
    skb_read_list_init_offset(&parser, output, 0);

    lpaddr_t base, limit;

    NUMA_DEBUG_INIT("parsing node information...\n");
    while (skb_read_list(&parser, "node(%" PRIu32 ", %" PRIuLPADDR ", %" PRIuLPADDR ")",
                         &node, &base, &limit)) {
        if (parsed == topology->num_nodes) {
            parsed++;
            break;
        }

        // XXX: assume the IDs are labelled 0..n-1
        assert(parsed == node);

        topology->nodes[parsed].num_cores = 0;
        topology->nodes[parsed].id = node;
        topology->nodes[parsed].mem_base = base;
        topology->nodes[parsed].mem_limit = limit;
        topology->nodes[parsed].apicid = (uint16_t) -1;
        topology->nodes[parsed].cores = NULL;

        // TODO: topology->nodes[node].coresbm = allocbm()

        NUMA_DEBUG_INIT("  > node %" PRIuNODEID " [0x%016" PRIxLPADDR", 0x%016"
                        PRIxLPADDR "] (%" PRIuLPADDR " MB)\n",
                        node, base, limit, (limit-base) >> 20);
        parsed++;
    }

    if ((nodeid_t) parsed != topology->num_nodes) {
        NUMA_DEBUG_INIT("node list incomplete: %" PRIuNODEID ", %" PRIuNODEID "\n",
                        parsed, topology->num_nodes);
        err = NUMA_ERR_SKB_DATA;
        goto error_out;
    }

    char arch[10];
    uint32_t apic = 0;

    /* parse the numa core list */
    output = strchr(output, '\n') + 1;
    skb_read_list_init_offset(&parser, output, 0);
    parsed = 0;

    NUMA_DEBUG_INIT("parsing core information...\n");
    while (skb_read_list(&parser, "cpu(%" PRIu32 ", %" PRIu32 ", %" PRIu32 ", %[^,)] , "
                         "dummy)", &node, &core, &apic,arch)) {
        if (parsed == topology->num_cores) {
            parsed++;
            break;
        }
        if (!(node < topology->num_nodes)) {
            NUMA_DEBUG_INIT("core %" PRIuCOREID " invalid node id %" PRIuNODEID "\n",
                            core, node);
            err = NUMA_ERR_SKB_DATA;
            goto error_out;
        }

        topology->nodes[node].num_cores++;

        /* the cores come sorted by nodes. The first one sets the cores pointer*/
        if (topology->nodes[node].cores == NULL) {
            topology->nodes[node].cores = &cores_array[parsed];
        }

        // TODO:  set bitmask topology->nodes[node].coresbm
        cores_array[parsed].id = (coreid_t) core;
        cores_array[parsed].apicid = (uint16_t) apic;
        cores_array[parsed].node = &topology->nodes[node];
        cores_array[parsed].arch = archstr_to_cputype(arch);

        // set the entry in the cores array
        topology->cores[core] = &cores_array[parsed];

        if (cores_array[parsed].arch == CPU_TYPE_NUM) {
            err = SYS_ERR_ARCHITECTURE_NOT_SUPPORTED;
            goto error_out;
        }

        NUMA_DEBUG_INIT("  > %s core %" PRIuCOREID" apic=%" PRIu32 ", node=%"
                        PRIuNODEID "\n", arch, (coreid_t )core, apic, node);
        parsed++;
    }

    if ((coreid_t) parsed != topology->num_cores) {
        NUMA_DEBUG_INIT("core list incomplete: %" PRIuCOREID ", %" PRIuCOREID "\n",
                        (coreid_t )parsed, topology->num_cores);
        err = NUMA_ERR_SKB_DATA;
        goto error_out;
    }

    output = strchr(output, '\n') + 1;
    skb_read_list_init_offset(&parser, output, 0);
    parsed = 0;

    uint32_t from, to;
    uint32_t distance;
    NUMA_DEBUG_INIT("parsing locality information...\n");
    while (skb_read_list(&parser, "node_distance(%" PRIuNODEID ", %" PRIuNODEID ", %" PRIu32 ")",
                          &from, &to, &distance)) {\
        NUMA_DEBUG_INIT("  > [%" PRIuNODEID "] -> [%" PRIuNODEID "] = %" PRIu32"\n",
                        from, to, distance);
        topology->distances[from * topology->num_nodes + to] = distance;
        parsed++;
    }

    if (parsed == 0) {
        NUMA_DEBUG_INIT("locality list not existent setting all to 10...\n");
        for (nodeid_t i = 0; i < topology->num_nodes; ++i) {
            for (nodeid_t j = 0; j < topology->num_nodes; ++j) {
                topology->distances[from * topology->num_nodes + to] = 10;
            }
        }
    } else if (parsed != (uint32_t)topology->num_nodes * topology->num_nodes) {
        NUMA_DEBUG_INIT("locality list incomplete: %" PRIu32" / %" PRIu32 "\n",
                        parsed, (uint32_t)topology->num_nodes * topology->num_nodes);
        err = NUMA_ERR_SKB_DATA;
        goto error_out;
    }

    return SYS_ERR_OK;

    error_out:
    free(topology->nodes);
    return err;
}

/**
 * \brief frees the numa topology structure
 *
 * \param topology pointer to the topology information structure
 */
void numa_free_topology(struct numa_topology *topology)
{
    if (topology && topology->nodes) {
        free(topology->nodes);
    }
    memset(topology, 0, sizeof(*topology));
}
