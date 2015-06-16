/**
 * \file
 * \brief internal header of libnuma
 *
 * This is derived from:
 *
 * Linux man pages "numa"
 * libnuma from http://oss.sgi.com/projects/libnuma/
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef NUMA_INTERNAL_H_
#define NUMA_INTERNAL_H_

#include <barrelfish_kpi/cpu.h>

#include "numa_debug.h"

/*
 * ----------------------------------------------------------------------------
 * Library global variable definitions
 * ----------------------------------------------------------------------------
 */
extern uint8_t numa_initialized;

/*
 * ----------------------------------------------------------------------------
 * Data structure definitions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief numa topology information of the system
 */
struct numa_topology {
    nodeid_t num_nodes;        ///< number of nodes in the system
    coreid_t num_cores;        ///< number of cores in the system
    nodeid_t preferred;        ///< the preferred node of the domain
    numa_policy_t strict;      ///< numa policy
    numa_policy_t bind;        ///< memory bind policy
    size_t pagesize;           ///< numa page size
    uint32_t *distances;       ///< numa distances
    struct numa_node *nodes;   ///< nodes in the system
    struct numa_core **cores;  ///< cores in the system (sorted by core id)
};

/**
 * \brief represents a numa node
 */
struct numa_node {
    nodeid_t id;               ///< id of the node
    uint16_t apicid;           ///< apic id for the node (core 0)
    coreid_t num_cores;        ///< number of cores within the
    struct numa_core *cores;   ///< pointer to the cores array
    struct bitmask *coresbm;   ///< bitmask for the cores
    lpaddr_t mem_base;         ///< base address of the memory
    lpaddr_t mem_limit;         ///< size of the memory region
};

/**
 * \brief represents a core
 */
struct numa_core {
    coreid_t id;               ///< id of the core
    uint16_t apicid;           ///< apic id of the core
    enum cpu_type arch;        ///< architecture
    struct numa_node *node;    ///< node of the core
};

///< stores the topology information
extern struct numa_topology numa_topology;

///< numa interleave mask for allocations
extern struct bitmap *numa_alloc_interleave_mask;

///< numa bind mask for allocations
extern struct bitmap *numa_alloc_bind_mask;

/*
 * ----------------------------------------------------------------------------
 * Queriying the SKB
 * ----------------------------------------------------------------------------
 */

/**
 * \brief obtains the system topology from the SKB
 *
 * \param topology pointer to the topology information structure
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_get_topology_from_skb(struct numa_topology *topology);

/**
 * \brief frees the numa topology structure
 *
 * \param topology pointer to the topology information structure
 */
void numa_free_topology(struct numa_topology *topology);

/**
 * \brief dumps the numa topology structure
 *
 * \param topology pointer to the topology to dump
 */
void numa_dump_topology(struct numa_topology *topology);

/*
 * ----------------------------------------------------------------------------
 * macros for library checks
 * ----------------------------------------------------------------------------
 */

#define NUMA_CHECK_STRICT 1

#if NUMA_CHECK_STRICT
#define numa_check_init() \
    if (numa_initialized != 0x1) { \
        USER_PANIC("NUMA library has not been initialized\n"); \
    }

#define numa_check_node_id(_id) \
    if (_id >= numa_topology.num_nodes) { \
        NUMA_WARNING("Node ID exceeds number of available nodes: %" PRIuNODEID "/%" \
                      PRIuNODEID, _id, numa_topology.num_nodes); \
        return NUMA_NODE_INVALID; \
    }

#define numa_check_core_id(_id) \
    if (_id >= numa_topology.num_cores) { \
        NUMA_WARNING("Core ID exceeds number of available cores: %"PRIuCOREID \
                     "/%" PRIuCOREID, _id, numa_topology.num_cores); \
        return (coreid_t)-1; \
    }

#else
#define numa_check_init()
#define numa_check_node_id(_id)
#define numa_check_core_id(_id)
#endif

#endif /* NUMA_INTERNAL_H_ */
