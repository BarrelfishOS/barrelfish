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
    nodeid_t num_nodes;      ///< number of nodes in the system
    coreid_t num_cores;      ///< number of cores in the system
    nodeid_t preferred;      ///< the preferred node of the domain
    numa_policy_t strict;    ///< numa policy
    numa_policy_t bind;      ///< memory bind policy
    size_t pagesize;         ///< numa page size
    struct numa_node *nodes; ///< nodes in the system
    struct numa_core *cores; ///< cores in the system
};

/**
 * \brief represents a numa node
 */
struct numa_node {
    nodeid_t id;             ///< id of the node
    uint16_t apicid;         ///< apic id for the node (core 0)
    coreid_t num_cores;      ///< number of cores within the
    struct numa_core *cores; ///< pointer to the cores array
    struct bitmask *coresbm; ///< bitmask for the cores
    lpaddr_t mem_base;       ///< base address of the memory
    lpaddr_t mem_size;       ///< size of the memory region
};

/**
 * \brief represents a core
 */
struct numa_core {
    coreid_t id;             ///< id of the core
    uint16_t apicid;         ///< apic id of the core
    struct numa_node *node;  ///< node of the core
};

extern struct numa_topology numa_topology;

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



#endif /* NUMA_INTERNAL_H_ */
