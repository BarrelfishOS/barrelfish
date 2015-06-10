/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <cpuid_internal.h>

/*
 * ===============================================================================
 * library internal, global variables
 * ===============================================================================
 */

///< maximum input value for basic CPUID information
uint32_t cpuid_g_max_input_basic = 0;

///< maximum input value for extended CPUID information
uint32_t cpuid_g_max_input_extended = 0;

///< the vendor of the core this code is executed
cpuid_vendor_t cpuid_g_vendor = CPUID_VENDOR_UNKNOWN;

///< function pointer table for vendor specific handlers
struct cpuid_functions cpuid_fn;

///< cpu cache names in readable representation
char *cpuid_cache_names[4][4] = {
    {""},
    {"", "l1i","l1d","l1"},
    {"", "l2i","l2d","l2"},
    {"", "l3i","l3d","l3"}
};

/*
 * ===============================================================================
 * library initialization
 * ===============================================================================
 */

/**
 * \brief initializes the cpuid library to handle vendor specific stuff
 */
errval_t cpuid_init(void)
{
    cpuid_g_vendor = cpuid_vendor();

    assert(cpuid_fn.proc_max_input_basic);
    cpuid_g_max_input_basic = cpuid_fn.proc_max_input_basic();
    cpuid_g_max_input_extended = cpuid_fn.proc_max_input_extended();

    CPUID_PRINTF("initializing for %s CPU. Max input = [0x%08" PRIx32 " / 0x%08"
                 PRIx32"]\n", cpuid_vendor_string(), cpuid_g_max_input_basic,
                 cpuid_g_max_input_extended);

    return SYS_ERR_OK;
}


/*
 * ===============================================================================
 * vendor information
 * ===============================================================================
 */

/**
 * \brief reads the CPU vendor string and returns the vendor information
 *
 * \return CPU vendor information CPUID_VENDOR_*
 *
 * This function also updates the function table pointer based on the read values
 */
cpuid_vendor_t cpuid_vendor(void)
{
    if (cpuid_intel_check_vendor()) {
        cpuid_intel_set_handlers(&cpuid_fn);
        return CPUID_VENDOR_INTEL;
    } else if (cpuid_amd_check_vendor()) {
        cpuid_amd_set_handlers(&cpuid_fn);
        return CPUID_VENDOR_AMD;
    }
    return CPUID_VENDOR_UNKNOWN;
}

/**
 * \brief returns a string representation of the vendor
 *
 * \return string representation of the vendor
 */
char *cpuid_vendor_string(void)
{
    switch(cpuid_g_vendor) {
        case CPUID_VENDOR_AMD :
            return "amd";
            break;
        case CPUID_VENDOR_INTEL :
            return "intel";
            break;
        default:
            return "unknown";
    }
}


/*
 * ===============================================================================
 * basic processor information
 * ===============================================================================
 */

/**
 * \brief obtains the family information from the CPU
 *
 * \param memory to fill in the family information
 */
errval_t cpuid_proc_family(struct cpuid_proc_family *fmly)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.proc_family);

    return cpuid_fn.proc_family(fmly);
}

/**
 * \brief obtains the processor name
 *
 * \param buf   buffer where to store the processor name string
 * \param len   length of the buffer
 */
errval_t cpuid_proc_name(char *buf, size_t len)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.proc_name);

    return cpuid_fn.proc_name(buf, len);
}

/**
 * \brief returns the maximum input value for basic CPUID functions
 *
 * \return integer representing the maximum input
 */
uint32_t cpuid_proc_max_input_basic(void)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return 0;
    }
    cpuid_g_max_input_basic = cpuid_fn.proc_max_input_basic();

    return cpuid_g_max_input_basic;
}

/**
 * \brief returns the maximum input value for extended CPUID functions
 *
 * \return integer representing the maximum input
 */
uint32_t cpuid_proc_max_input_extended(void)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return 0;
    }
    cpuid_g_max_input_extended = cpuid_fn.proc_max_input_extended();

    return cpuid_g_max_input_extended;
}

/**
 * \brief obtains the processor frequency information
 *
 * \param fi returned frequency information
 *
 * \returns SYS_ERR_OK on success
 *          CPUID_ERR_*on failure
 */
errval_t cpuid_frequency_info(struct cpuid_freqinfo *fi)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return 0;
    }
    assert(cpuid_fn.frequency_info);

    return cpuid_fn.frequency_info(fi);
}

/*
 * ===============================================================================
 * cache topology information
 * ===============================================================================
 */

/**
 * \brief calculates the system coherency size (cacheline size)
 *
 * \returns the coherency size of the core in bytes
 */
uint16_t cpuid_system_coherency_size(void)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.cache_line_size);

    return cpuid_fn.cache_line_size();
}

/**
 * \brief obtains cache parameters for a given index
 *
 * \param ci   memory to be filled in with information
 * \param idx  index of the cache to obtain information for
 *
 * \return  SYS_ERR_OK  on success
 *          CPUID_ERR_NO_SUCH_INDEX if there is no cache associated with the index
 */
errval_t cpuid_cache_info(struct cpuid_cacheinfo *ci, uint32_t idx)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.cache_info);

    return cpuid_fn.cache_info(ci, idx);
}

/**
 * \brief returns a string representation of the cache type
 *
 * \param ct    type of the cache
 *
 * \return cache name string
 */
char *cpuid_cache_type_string(cpuid_cachetype_t ct)
{
    switch(ct) {
        case CPUID_CACHE_TYPE_DATA:
            return "data";
            break;
        case CPUID_CACHE_TYPE_INSTR:
            return "instr";
            break;
        case CPUID_CACHE_TYPE_UNIFIED:
            return "unified";
            break;
        default:
            return "invalid";
            break;
    }
}


/*
 * ===============================================================================
 * TLB information
 * ===============================================================================
 */

/**
 * \brief obtains the TLB topology information
 *
 * \param ti
 * \param idx
 *
 * \returns SYS_ERR_OK on success
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_tlb_info(struct cpuid_tlbinfo *ti, uint32_t idx)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.tlb_info);

    return cpuid_fn.tlb_info(ti, idx);
}

/*
 * ===============================================================================
 * thread and topology information
 * ===============================================================================
 */


/**
 * \brief obtains the topology information for a given thread
 *
 * \param ti    returns the thread information
 *
 * \return  SYS_ERR_OK on success
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_thread_info(struct cpuid_threadinfo *ti)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.thread_info);

    return cpuid_fn.thread_info(ti);
}

/**
 * \brief obtains topology information from the CPU
 *
 * \param topo  pointer to store the information
 * \param idx   topology level index
 *
 * \return SYS_ERR_OK on success
 *         CPUID_ERR_* on failure
 */
errval_t cpuid_topology_info(struct cpuid_topologyinfo *topo, uint8_t idx)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.topology_info);

    return cpuid_fn.topology_info(topo, idx);
}


/*
 * ===============================================================================
 * feature information
 * ===============================================================================
 */
/**
 * \brief obtains the CPU's feature information
 *
 * \param fi    structure to be filled in
 *
 * \return  SYS_ERR_OK on success
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_feature_info(struct cpuid_featureinfo *fi)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.feature_info);

    return cpuid_fn.feature_info(fi);
}


/*
 * ===============================================================================
 * address space information
 * ===============================================================================
 */

/**
 * \brief obtains the address space size information of the CPU
 *
 * \param ai  struct to be filled in with adderss space information
 *
 * \returns SYS_ERR_OK on susccess
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_address_space_info(struct cpuid_adressspaceinfo *ai)
{
    if (cpuid_g_vendor == CPUID_VENDOR_UNKNOWN) {
        return CPUID_ERR_UNKNOWN_VENDOR;
    }
    assert(cpuid_fn.address_space_info);

    return cpuid_fn.address_space_info(ai);
}
