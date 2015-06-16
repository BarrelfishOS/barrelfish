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

#include <numa.h>
#include <bitmap.h>
#include "numa_internal.h"

uint8_t numa_initialized = 0x0;

/**
 * \brief bitmask that is allocated by the library with bits representing all nodes
 *        on which the calling task may allocate memory.
 */
struct bitmap *numa_all_nodes_ptr;

/**
 * \brief points to a bitmask that is allocated by the library and left all zeroes.
 */
struct bitmap *numa_no_nodes_ptr;

/**
 * \brief points to a bitmask that is allocated by the library with bits
 *        representing all cpus on which the calling task may execute.
 */
struct bitmap *numa_all_cpus_ptr;

/**
 * \brief data structure representing the numa topology
 */
struct numa_topology numa_topology;


/**
 * \brief checks if numa support is available
 *
 * \returns NUMA_ERR_NOT_AVAILABLE  value all other functions are undefined
 *          SYS_ERR_OK:             NUMA functionality is available
 *
 * this function must be called before any of the other functions of libnuma.
 * during the call to numa_available the library also gets initialized
 */
errval_t numa_available(void)
{
    errval_t err;

    if (numa_initialized) {
        return (numa_initialized == 0xff) ? NUMA_ERR_LIB_INIT : SYS_ERR_OK;
    }

    NUMA_DEBUG_INIT("Initializing libnuma...\n");

    err = numa_get_topology_from_skb(&numa_topology);
    if (err_is_fail(err)) {
        goto out_err;
    }

#if NUMA_DEBUG_ENABLED
    numa_dump_topology(&numa_topology);
#endif

    numa_all_cpus_ptr = numa_allocate_cpumask();
    if(numa_all_cpus_ptr == NULL) {
        err =  LIB_ERR_MALLOC_FAIL;
        goto out_err1;
    }

    for (coreid_t i = 0; i < numa_topology.num_cores; ++i) {
        bitmap_set_bit(numa_all_cpus_ptr, numa_topology.cores[i]->id);
    }

#if NUMA_DEBUG_ENABLED
    bitmap_dump(numa_all_cpus_ptr);
#endif

    numa_all_nodes_ptr = numa_allocate_nodemask();
    if(numa_all_nodes_ptr == NULL) {
        err =  LIB_ERR_MALLOC_FAIL;
        goto out_err2;
    }

    for (nodeid_t i = 0; i < numa_topology.num_nodes; ++i) {
        bitmap_set_bit(numa_all_nodes_ptr, numa_topology.nodes[i].id);
    }

#if NUMA_DEBUG_ENABLED
    bitmap_dump(numa_all_nodes_ptr);
#endif

    numa_no_nodes_ptr = numa_allocate_nodemask();
    if(numa_no_nodes_ptr == NULL) {
        err =  LIB_ERR_MALLOC_FAIL;
        goto out_err3;
    }

    numa_alloc_bind_mask = numa_allocate_nodemask();
    if(numa_alloc_bind_mask == NULL) {
        err =  LIB_ERR_MALLOC_FAIL;
        goto out_err4;
    }
    numa_alloc_interleave_mask = numa_allocate_nodemask();
    if(numa_alloc_interleave_mask == NULL) {
        err =  LIB_ERR_MALLOC_FAIL;
        goto out_err5;
    }

#if NUMA_DEBUG_ENABLED
    bitmap_dump(numa_no_nodes_ptr);
#endif

    numa_initialized = 0x1;

    return SYS_ERR_OK;

    /* cleanup in case of error */
    out_err5:
    free(numa_alloc_bind_mask);
    out_err4:
    free(numa_no_nodes_ptr);
    out_err3:
    free(numa_all_nodes_ptr);
    out_err2:
    free(numa_all_cpus_ptr);
    out_err1:
    numa_free_topology(&numa_topology);
    out_err:
    numa_initialized = 0xff;
    return err_push(err, NUMA_ERR_LIB_INIT);
}

/**
 * \brief returns the highest node number available on the current system.
 *
 * \returns ID of the max NUMA node
 */
nodeid_t numa_max_node(void)
{
    numa_check_init();

    // XXX: assume nodes are 0..n-1
    return numa_topology.num_nodes - 1;
}

/**
 * \brief returns the highest ID of the present cores
 *
 * \returns the maximum number of cores in the system
 */
coreid_t numa_max_core(void)
{
    numa_check_init();

    // XXX: assume the IDs are 0...n-1
    return numa_topology.num_cores - 1;
}

/**
 * \brief returns the current node the domain is running on
 *
 * \return ID of the current node
 */
nodeid_t numa_current_node(void)
{
    numa_check_init();

    // XXX: do we need disp_get_core_id() here?
    return numa_topology.cores[disp_get_current_core_id()]->node->id;
}

/**
 * \brief returns the size of the node mask
 *
 * \return size of the node mask
 */
nodeid_t numa_num_possible_nodes(void)
{
    numa_check_init();

    return NUMA_MAX_NUMNODES;
}

/**
 * \brief Obtains the number of all memory nodes in the system
 *
 * \return number of memory nodes in the system
 *
 * returns the number of memory nodes in the system. This count includes any nodes
 * that are currently disabled.
 */
nodeid_t numa_num_configured_nodes(void)
{
    numa_check_init();

    // XXX: we have all nodes configures
    return numa_topology.num_nodes;
}

/**
 * \brief obtains the nodes the domain is allowed to allocate memory from
 *
 * \returns bitmask representing the allowing nodes
 *
 * returns the mask of nodes from which the process is allowed to allocate memory
 * in it's current cpuset context.
 */
struct bitmap *numa_get_mems_allowed(void)
{
    numa_check_init();

    /* we don't have restriction yet. */
    return numa_all_nodes_ptr;
}

/**
 * \brief returns the total numberof CPUs in the system
 *
 * \returns total number of CPUs in the system
 *
 * returns the number of cpus in the system. This count includes any cpus that are
 * currently disabled.
 */
coreid_t numa_num_configured_cpus(void)
{
    numa_check_init();

    // XXX we assume that we can schedule all cores
    return numa_topology.num_cores;
}

/**
 * \brief returns the number of cpus that the calling domain is allowed to use.
 *
 * \returns number of CPUs the domain is allowed to use
 */
coreid_t numa_num_task_cpus(void)
{
    numa_check_init();

    // XXX: we do not have any restrictions yet, return all cores
    return numa_topology.num_cores;
}

/**
 * \brief returns the number of nodes on which the calling domain is allowed to
 *        allocate memory
 *
 * \returns number of nodes the domain is allowed to use
 */
nodeid_t numa_num_task_nodes(void)
{
    numa_check_init();

    // XXX: We do not have any restrictions yet. just return all nodes
    return numa_topology.num_nodes;
}

/**
 * \brief obtains the size of a node
 *
 * \param node  ID of the NUMA node
 * \param freep returns the number of available bytes of the node
 *
 * \returns size of the node in bytes
 *
 * returns the memory size of a node. If the argument freep is not NULL, it used
 * to return the amount of free memory on the node. On error it returns
 * NUMA_NODE_INVALID
 */
size_t numa_node_size(nodeid_t node, uintptr_t *freep)
{
    numa_check_init();
    numa_check_node_id(node);

    if (freep) {
        // TODO: figure out how much memory is left in the node
    }

    return (numa_topology.nodes[node].mem_limit - numa_topology.nodes[node].mem_base);
}

/**
 * \brief obtains the base address of the numa node
 *
 * \returns physical address of the start of the numa node
 *          NUMA_NODE_INVALID if the node does not exist
 */
lpaddr_t numa_node_base(nodeid_t node)
{
    numa_check_init();
    numa_check_node_id(node);

    return numa_topology.nodes[node].mem_base;
}

/**
 * \brief returns the preferred node of the current task.
 *
 * \returns node ID where memory is preferably allocated
 */
nodeid_t numa_preferred(void)
{
    numa_check_init();
    return numa_current_node();
}

/**
 * \brief  sets the preferred node for the current task to node
 *
 * \param node  ID of the node to set preferred
 *
 * The system will attempt to allocate memory from the preferred node, but will
 * fall back to other nodes if no memory is available on the the preferred node
 *
 * Passing a node of -1 argument specifies local allocation
 */
void numa_set_preferred(nodeid_t node)
{
    numa_check_init();

    if (node >= numa_topology.num_nodes) {
        NUMA_WARNING("Node ID exceeds number of available nodes %" PRIuNODEID "/%"
                     PRIuNODEID, node, numa_topology.num_nodes);
        return;
    }

    numa_topology.preferred = node;
}


/**
 * \brief runs the current domain on a specific node.
 *
 * \param node  ID of the node to run the domain on
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 *
 * Passing -1 permits the kernel to schedule on all nodes again
 */
errval_t numa_run_on_node(nodeid_t node)
{
    numa_check_init();

    USER_PANIC("running the domain on a specific node is not supported yet\n");
    return 0;
}


/**
 * \brief runs the current domain only on nodes specified in nodemask.
 *
 * \param nodemask bitmap representing the nodes to run the domain on
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_run_on_node_mask(struct bitmap *nodemask)
{
    numa_check_init();

    USER_PANIC("running the domain on a specific node is not supported yet\n");
    return 0;
}


/**
 * \brief returns a mask of CPUs on which the current task is allowed to run.
 *
 * \returns bitmap represening the coreids the domain is allowed to run
 */
struct bitmap *numa_get_run_node_mask(void)
{
    numa_check_init();

    return numa_all_nodes_ptr;
}


/**
 * \brief specify the memory bind policy
 *
 * \param strict numa policy to apply
 *
 * specifies whether calls that bind memory to a specific node should use the
 * preferred policy or a strict policy.
 */
void numa_set_bind_policy(numa_policy_t strict)
{
    numa_check_init();

    if (strict == NUMA_POLICY_STRICT) {
        numa_topology.bind = strict;
    } else {
        numa_topology.bind = NUMA_POLICY_PREFERRED;
    }
}


/**
 * \brief enable or disable the strict allocation policy
 *
 * \param strict numa policy to apply
 *
 * s a flag that says whether the functions allocating on specific nodes should
 * use a strict policy. Strict means the allocation will fail if the memory cannot
 * be allocated on the target node.
 */
void numa_set_strict(numa_policy_t strict)
{
    numa_check_init();

    if (strict == NUMA_POLICY_STRICT) {
        numa_topology.strict = strict;
    } else {
        numa_topology.strict = NUMA_POLICY_PREFERRED;
    }
}


/**
 * \brief reports the distance in the machine topology between two nodes
 *
 * \param from source node to measure the distance
 * \param to   target node to measure the distance
 *
 * \returns distance between two nodes
 *          0 iff cannot be deterimed
 *
 * The factors are a multiple of 10.  A node has distance 10 to itself.
 */
uint32_t numa_distance(nodeid_t from, nodeid_t to)
{
    numa_check_init();

    if (from >= numa_topology.num_nodes || to >= numa_topology.num_nodes) {
        return (uint32_t)NUMA_NODE_INVALID;
    }

    return numa_topology.distances[from * numa_topology.num_nodes + to];
}


/**
 * \brief retrieves a bitmask of the cpus on which a domain may run
 *
 * \param did   domain ID
 * \param mask  returned bitmask
 *
 * \returns SYS_ERR_OK on success
 *          errval on FAILURE
 */
errval_t numa_sched_getaffinity(domainid_t did, struct bitmap *mask)
{
    numa_check_init();

    assert(!"NYI");
    return 0;
}


/**
 * \brief sets a domain's allowed cpu's to those cpu's specified in mask.
 *
 * \param did   domain ID
 * \param mask  bitmap representing the CPUs
 *
 * \returns SYS_ERR_OK on success
 *          errval on FAILURE
 */
errval_t numa_sched_setaffinity(domainid_t did, struct bitmap *mask)
{
    numa_check_init();

    assert(!"NYI");
    return 0;
}


/**
 * \brief returns the page size
 *
 * \returns the number of bytes in a page
 */
size_t numa_pagesize(void)
{
    numa_check_init();

    return numa_topology.pagesize;
}



/**
 * \brief converts a node number to a bitmask of CPUs
 *
 * \param node  the ID of the node
 * \param mask  bitmap representing the CPUs of this node
 *
 * \return  SYS_ERR_OK on SUCCESS
 *          NUMA_ERR_BITMAP_RANGE on FAILURE (too small bitmap)
 *
 * The user must pass a bitmask structure with a mask buffer long enough to
 * represent all possible cpu's
 */
errval_t numa_node_to_cpus(nodeid_t node, struct bitmap *mask)
{
    numa_check_init();

    if (!(node < numa_topology.num_nodes)) {
        return NUMA_ERR_NODEID_INVALID;
    }

    if (bitmap_get_nbits(mask) < numa_topology.num_cores) {
        return NUMA_ERR_BITMAP_RANGE;
    }

    bitmap_clear_all(mask);

    struct numa_node *nnode = &numa_topology.nodes[node];
    for (coreid_t i = 0; i < nnode->num_cores; ++i) {
        bitmap_set_bit(mask, nnode->cores[i].id);
    }

#if NUMA_DEBUG_ENABLED
    bitmap_dump(mask);
#endif

    return SYS_ERR_OK;
}


/**
 * \brief returns the node that a cpu belongs to
 *
 * \param cpu   ID of the core
 *
 * \returns node ID on SUCCESS
 *          NUMA_NODE_INVALID on FAILURE
 */
nodeid_t numa_node_of_cpu(coreid_t cpu)
{
    numa_check_init();

    numa_check_core_id(cpu);

    return numa_topology.cores[cpu]->node->id;
}


/**
 * \brief gets the number of cores for the given numa node
 *
 * \param node NUMA node to get the number of cores
 *
 * \returns number of cores for the node
 */
coreid_t numa_num_node_cpus(nodeid_t node)
{
    if (node >= numa_topology.num_nodes) {
        NUMA_WARNING("Node ID exceeds number of available nodes: %" PRIuNODEID "/%"
                      PRIuNODEID, node, numa_topology.num_nodes);
        return 0;
    }

    return numa_topology.nodes[node].num_cores;
}
