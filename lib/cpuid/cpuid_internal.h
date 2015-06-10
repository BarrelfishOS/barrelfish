/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBCPUID_INTERNAL_H_
#define LIBCPUID_INTERNAL_H_ 1

#include <barrelfish/barrelfish.h>
#include <cpuid/cpuid.h>

/*
 * ===============================================================================
 * library configuration
 * ===============================================================================
 */
///< enables verbose output of library operations
#define CPUID_VERBOSE_OUTPUT 0


/*
 * ===============================================================================
 * declarations
 * ===============================================================================
 */

///< function table for vendor specific handlers
struct cpuid_functions
{
    errval_t (*proc_family)(struct cpuid_proc_family *fmly);
    errval_t (*proc_name)(char *buf, size_t len);
    uint32_t (*proc_max_input_basic)(void);
    uint32_t (*proc_max_input_extended)(void);
    errval_t (*frequency_info)(struct cpuid_freqinfo *fi);

    uint16_t (*cache_line_size)(void);
    errval_t (*cache_info)(struct cpuid_cacheinfo *ci, uint32_t idx);

    errval_t (*tlb_info)(struct cpuid_tlbinfo *ti, uint32_t idx);

    errval_t (*thread_info)(struct cpuid_threadinfo *ti);
    errval_t (*topology_info)(struct cpuid_topologyinfo *topo, uint8_t idx);

    errval_t (*feature_info)(struct cpuid_featureinfo *fi);

    errval_t (*address_space_info)(struct cpuid_adressspaceinfo *ai);
};


///< maximum input value for basic CPUID information
extern uint32_t cpuid_g_max_input_basic;

///< maximum input value for extended CPUID information
extern uint32_t cpuid_g_max_input_extended;

///< the vendor of the core this code is executed
extern cpuid_vendor_t cpuid_g_vendor;

///< function pointer table for vendor specific handlers
extern struct cpuid_functions cpuid_fn;

///< cpu cache names in readable representation
extern char *cpuid_cache_names[4][4];

/*
 * ===============================================================================
 * macros
 * ===============================================================================
 */

#if CPUID_VERBOSE_OUTPUT
#define CPUID_PRINTF(x...) debug_printf("libcpuid: " x)
#else
#define CPUID_PRINTF(x...)
#endif

///< masks the extended value
#define CPUID_EXTENDED_INPUT_MASK(x) (x & 0x7fffffff)

/*
 * ===============================================================================
 * functions
 * ===============================================================================
 */

static inline uint8_t cpuid_bits_needed(uint8_t count)
{
    uint8_t mask = 0x80;
    uint8_t cnt = 8;

    while ((cnt > 0) && ((mask & count) != mask)) {
        mask >>= 1;
        cnt--;
    }
    return (cnt);
}

#include "cpuid_amd.h"
#include "cpuid_intel.h"

#endif /* CPUID_INTERNAL_H_ */
