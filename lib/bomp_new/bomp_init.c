/**
 * \file
 * \brief API to use the bomp library
 */

/*
 * Copyright (c)2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>


#include <bomp_internal.h>
/*
 * NOTES
 *
 * - nodes and worker store their local state in the TLS or binding
 * - master thread stores it in global variable
 *
 *      master threads
 *         - list of nodes -> execute on
 *         - if having list of workers -> execute on
 *
 *
 */


/**
 * \brief initializes the BOMP library using the indicated cores of the BM
 *
 * \param coresbm		bitmap representing the cores to run on
 * \param stack_size	size of the thread's stack in bytes
 *
 * \returns	0 on SUCCESS
 *          non-zero on FAILURE
 */
int bomp_init_cores(void*coresbm, size_t stack_size)
{
	assert(!"NYI");
	return 0;
}

/**
 * \brief initializes the BOMP library with the given stack sizes
 *
 * \param stack_size	size of the thread's stack in bytes
 *
 * \returns	0 on SUCCESS
 *          non-zero on FAILURE
 *
 * This function will use the first nthreads cores to run on
 */
int bomp_init_varstack(unsigned int nthreads, size_t stack_size)
{
	/// get the maximum number of cores
	if (numa_available() != 0) {
        return -1;
	}

	if (nthreads == BOMP_THREADS_ALL) {
	    nthreads = numa_num_configured_cpus();
	}

    nodeid_t node_current = numa_current_node();
    nodeid_t node_count = 1;

#if 0

    if (nthreads > numa_num_node_cpus(node_current)) {
        coreid_t threads_remaining = nthreads - numa_num_node_cpus(node_current);
        /* determine the number of needed nodes */
        for (nodeid_t node = 0; node <= numa_max_node(); ++node) {
            if (node == node_current) {
                continue;
            }
            node_count++;
            if (threads_remaining < numa_num_node_cpus(node)) {
                threads_remaining = 0;
                break;
            } else {
                threads_remaining -= numa_num_node_cpus(node);
            }

        }

        if (threads_remaining) {
            /* Not enough cores availabel to serve the nthreads request */
            BOMP_ERROR("not enough cores available: need %" PRIuCOREID " more \n",
                       threads_remaining);
            return -2;
        }
    }
#endif


    BOMP_DEBUG_INIT("Initializing BOMP with a %" PRIuNODEID " nodes of %" PRIuCOREID
                    " threads\n", node_count, nthreads);

    bomp_icv_init_default((coreid_t)nthreads);

    struct bomp_tls *tls = calloc(1, sizeof(struct bomp_tls));
    if (tls == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    tls->role = BOMP_THREAD_ROLE_MASTER;
    tls->self = thread_self();

    if (node_count > 1) {
        tls->r.master.nodes = calloc(node_count, sizeof(struct bomp_node));
        if (tls->r.master.nodes == NULL) {
            free(tls);
            return LIB_ERR_MALLOC_FAIL;
        }
        tls->r.master.num_nodes = node_count - 1;
    } else {
        tls->r.master.num_nodes = 0;
        tls->r.master.nodes = NULL;
    }

    tls->icv.global = &g_omp_icv_global_default;
    tls->icv.device = &g_omp_icv_device_default;
    tls->icv.task = NULL;

    thread_set_tls(tls);

    // divide the threads equally among the nodes
    coreid_t threads_per_node = (coreid_t)(nthreads / node_count);

#if 0
    coreid_t master_threads = numa_num_node_cpus(node_current);
#else
    coreid_t master_threads = nthreads;
#endif

    if (master_threads > threads_per_node) {
        master_threads = threads_per_node;
    }

    nthreads -= master_threads;

    nodeid_t numa_node = 0;
    for (nodeid_t node = 1; node < node_count; ++node) {
        if (numa_node == node_current) {
            numa_node++;
        }
        coreid_t num_threads = numa_num_node_cpus(node);
        if (num_threads > threads_per_node) {
            num_threads = threads_per_node;
        }

        bomp_node_init(BOMP_NODE_LOCAL, numa_node, node, num_threads,
                       stack_size, &tls->r.master.nodes[node]);

        nthreads -= num_threads;
    }

    /* now all the other threads should have been initialized */
    assert(nthreads == 0);

    /* initialize the local node */
    bomp_node_init(BOMP_NODE_MASTER, node_current, 0, master_threads,
                   stack_size, &tls->r.master.local);

    // the master thread is active
    tls->r.master.local.threads_active = 1;
    tls->r.master.nodes_active = 1;

    return 0;
}
