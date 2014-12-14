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


// skb_machine_mem_range
/*
static errval_t skb_get_percore_mem_range(coreid_t core, genpaddr_t *base, genpaddr_t *limit)
{
    assert(base != NULL);
    assert(limit != NULL);

    int retval;

    genpaddr_t b, l;
    struct list_parser_status iterator;

    *base = -1;
    *limit = 0;

    retval = skb_execute_query("local_memory_affinity(%d,List),"
        "write(List).",core);
    if (retval != 0) {
    	debug_printf("skb_execute_query(local_memory_affinity(d,List)) failed %s\n", err_getstring(retval));
    	debug_printf("error cod %u: %s\n", skb_read_error_code(),  skb_get_error_output());
        return SKB_ERR_EXECUTION;
    }

    skb_read_list_init(&iterator);
    while (skb_read_list(&iterator, "range(%" PRIuGENPADDR ", %" PRIuGENPADDR ")",
            &b, &l)) {

        if (*base == -1 || b < *base) {
            *base = b;
        }
        if (l > *limit) {
            *limit = l;
        }
    }

    if (*base == -1) {
        *base = 0;
    }

    return SYS_ERR_OK;
}
*/

// skb_machine_num_cores
/*static errval_t skb_num_cores(uint32_t *num_cores)
{
    assert(num_cores != NULL);

    errval_t err;

    err = skb_execute_query("available_nr_cores(Nr),""write(Nr).");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_execute_query");
        return err_push(err, SKB_ERR_EVALUATE);
    }

    // parse #cores
    int i;
    err = skb_read_output("%d", &i);
    *num_cores = i;

    return SYS_ERR_OK;
}
*/

/*

static errval_t skb_get_node_of_core(coreid_t core, uint32_t *node_id) {
    errval_t err;

    *node_id = -1;

    err = skb_execute_query(
        "corename(%d,_,apic(CoreName)),"
        "cpu_affinity(CoreName,_,ProxDomain),"
        "write(ProxDomain).",core);
    if(err_is_fail(err)) {
    	 DEBUG_ERR(err, "skb_execute_query");
        return err;
    }

    err = skb_read_output("%d",node_id);
    if(err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;

} */


/*
 * corename(0, x86_64, apic(0))
 * memory_region(9df2000,13,8192,1,0)
 *
 *
 */

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

    err = skb_execute_query("get_system_topology(Nnodes,Ncores,Lnodes,Lcores),writeln(num(nodes(Nnodes),cores(Ncores))),writeln(Lnodes),writeln(Lcores).");
    if(err_is_fail(err)) {
    	DEBUG_ERR(err, "executing SKB query\n");
    }

    uint32_t num_cores = 0;
    uint32_t num_nodes = 0;
    err = skb_read_output("num(nodes(%d), cores(%d))", &num_nodes, &num_cores);
    if (err_is_fail(err)) {
    	DEBUG_ERR(err, "parsing number information\n");
    }

    NUMA_DEBUG_INIT("discovered topology with %u nodes, %u cores\n",
    				(nodeid_t)num_nodes, (coreid_t)num_cores);

    if (!num_nodes || !num_cores) {
    	USER_PANIC("invalid data returned\n");
    }

    topology->num_cores = (coreid_t)num_cores;
    topology->num_nodes = (nodeid_t)num_nodes;

    topology->preferred = NUMA_POLICY_DEFAULT;
    topology->strict = NUMA_POLICY_DEFAULT;

    topology->nodes = malloc(num_nodes * sizeof(struct numa_node)
    							+ num_cores * sizeof(struct numa_core));
    if (topology->nodes == NULL) {
    	return LIB_ERR_MALLOC_FAIL;
    }

    topology->cores = (struct numa_core *) (topology->nodes + num_nodes);

    /* parse the numa node list */


    /* parse the numa core list */


    debug_printf("===============\n");




    assert(!"NYI");
    return SYS_ERR_OK;
}

