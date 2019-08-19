/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CPUID_H_
#define CPUID_H_


/*
 * ===============================================================================
 * functions and definitions for accessing the raw cpuid output
 * ===============================================================================
 */

///< struct representing the registers used for the cpuid instruction
struct cpuid_regs
{
    uint32_t eax;   ///< eax register [in: function, out: return value]
    uint32_t ebx;   ///< ebx register [in: none,     out: return value]
    uint32_t ecx;   ///< ecx register [in: argument, out: return value]
    uint32_t edx;   ///< edx register [in: none,     out: return value]
};

///< macro declaration for initializing the cpuid_regs struct
#define CPUID_REGS_INITIAL(_EAX, _ECX) {_EAX, 0, _ECX, 0}


/*
 * ===============================================================================
 * library initialization
 * ===============================================================================
 */

/**
 * \brief initializes the cpuid library to handle vendor specific stuff
 */
errval_t cpuid_init(void);


/*
 * ===============================================================================
 * vendor information
 * ===============================================================================
 */

///< vendor string for Intel cpus as stored in {ebx, ecx, edx}
#define CPUID_VENDOR_STRING_INTEL   "GenuntelineI"  //"GenuineIntel"

///< vendor string for AMD cpus as stored in {ebx, ecx, edx}
#define CPUID_VENDOR_STRING_AMD     "AuthcAMDenti"  //“AuthenticAMD”

///< maximum size of the vendor string
#define CPUID_VENDOR_STRING_LENGTH (3 * sizeof(uint32_t))

///< type declaration for recognized cpu vendors
typedef enum cpuid_vendor
{
    CPUID_VENDOR_UNKNOWN = 0, ///< the CPU has an unknown vendor
    CPUID_VENDOR_AMD     = 1, ///< the CPU is an AuthenticAMD
    CPUID_VENDOR_INTEL   = 2, ///< the CPU is GenuineIntel
} cpuid_vendor_t;

/**
 * \brief reads the CPU vendor string and returns the vendor information
 *
 * \return CPU vendor information CPUID_VENDOR_*
 */
cpuid_vendor_t cpuid_vendor(void);

/**
 * \brief returns a string representation of the vendor
 *
 * \return string representation of the vendor
 */
char *cpuid_vendor_string(void);

/*
 * ===============================================================================
 * basic processor information
 * ===============================================================================
 */

///< maximum length the processor name can have
#define CPUID_PROC_NAME_LENGTH (3 * 4 * sizeof(uint32_t))

///< represents the processor family information of the cpu
struct cpuid_proc_family
{
    uint16_t model;     ///< processor model
    uint16_t family;    ///< processor family
    uint8_t  stepping;  ///< processor stepping
    uint8_t  type;      ///< processor type
};

///< processor frequency information
struct cpuid_freqinfo
{
    uint16_t base;  ///< processor base frequency in MHz
    uint16_t max;   ///< processor maximum frequency in MHz
    uint16_t bus;   ///< bus (reference) frequeny in Mhz
};

/**
 * \brief obtains the family information from the CPU
 *
 * \param memory to fill in the family information
 */
errval_t cpuid_proc_family(struct cpuid_proc_family *fmly);

/**
 * \brief obtains the processor name
 *
 * \param buf   buffer where to store the processor name string
 * \param len   length of the buffer
 */
errval_t cpuid_proc_name(char *buf, size_t len);

/**
 * \brief returns the maximum input value for basic CPUID functions
 *
 * \return integer representing the maximum input
 */
uint32_t cpuid_proc_max_input_basic(void);

/**
 * \brief returns the maximum input value for extended CPUID functions
 *
 * \return integer representing the maximum input
 */
uint32_t cpuid_proc_max_input_extended(void);

/**
 * \brief obtains the processor frequency information
 *
 * \param fi returned frequency information
 *
 * \returns SYS_ERR_OK on success
 *          CPUID_ERR_*on failure
 */
errval_t cpuid_frequency_info(struct cpuid_freqinfo *fi);

/*
 * ===============================================================================
 * cache topology information
 * ===============================================================================
 */

///< enumration of possible cache types
typedef enum cpuid_cache_type
{
    CPUID_CACHE_TYPE_INVALID    = 0,    ///< this cache information is invalid
    CPUID_CACHE_TYPE_DATA       = 1,    ///< cache is for data only
    CPUID_CACHE_TYPE_INSTR      = 2,    ///< cache is for instructions only
    CPUID_CACHE_TYPE_UNIFIED    = 3,    ///< cache is unified data & instr
} cpuid_cachetype_t;

///< cache information obtained from the cpu
struct cpuid_cacheinfo
{
    cpuid_cachetype_t type;          ///< the type of the cache
    char             *name;          ///< readable representation of the name
    size_t            size;          ///< size of the entire cache in bytes
    uint16_t          linesize;      ///< size of a cache line
    uint16_t          associativity; ///< associativity information
    uint32_t          sets;          ///< number of sets
    uint8_t           level;         ///< level
    uint8_t           shared;        ///< number of cores sharing that cache
    uint8_t           inclusive;     ///< chached data is inclusive
};

/**
 * \brief calculates the cache line size
 *
 * \returns the cacheline size of the core in bytes
 */
uint16_t cpuid_system_coherency_size(void);

/**
 * \brief obtains cache parameters for a given index
 *
 * \param ci   memory to be filled in with information
 * \param idx  index of the cache to obtain information for
 *
 * \return  SYS_ERR_OK  on success
 *          CPUID_ERR_NO_SUCH_INDEX if there is no cache associated with the index
 */
errval_t cpuid_cache_info(struct cpuid_cacheinfo *ci, uint32_t idx);

/**
 * \brief returns a string representation of the cache type
 *
 * \param ct    type of the cache
 *
 * \return cache name string
 */
char *cpuid_cache_type_string(cpuid_cachetype_t ct);


/*
 * ===============================================================================
 * TLB information
 * ===============================================================================
 */
///< tlb information structure
struct cpuid_tlbinfo
{
    cpuid_cachetype_t type;             ///< type of this tlb
    uint32_t          pagesize;         ///< page size
    uint32_t          entries;          ///< number of entries
    uint32_t          associativity;    ///< associativity
    uint8_t           level;            ///< level
};

/**
 * \brief obtains the TLB topology information
 *
 * \param ti    tlb info structure to be filled in
 * \param idx   index of the TLB
 *
 * \returns SYS_ERR_OK on success
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_tlb_info(struct cpuid_tlbinfo *ti, uint32_t idx);

/*
 * ===============================================================================
 * thread and topology information
 * ===============================================================================
 */

///< topology information this thread belongs to
struct cpuid_threadinfo
{
    coreid_t package;       ///< id of the package / socket
    coreid_t core;          ///< the core id relative to the socket
    coreid_t hyperthread;   ///< the thread id relative to the core
};

///< topology hierarchy information structure
struct cpuid_topologyinfo
{
    int x2apic;
    int nextshift;
};

/**
 * \brief obtains the topology information for a given thread
 *
 * \param ti    returns the thread information
 *
 * \return  SYS_ERR_OK on success
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_thread_info(struct cpuid_threadinfo *ti);

/**
 * \brief obtains topology information from the CPU
 *
 * \param topo  pointer to store the information
 * \param idx   topology level index
 *
 * \return SYS_ERR_OK on success
 *         CPUID_ERR_* on failure
 */
errval_t cpuid_topology_info(struct cpuid_topologyinfo *topo, uint8_t idx);

/*
 * ===============================================================================
 * feature information
 * ===============================================================================
 */

struct cpuid_featureinfo
{
    /* CPU features */
    uint32_t lm      : 1;   ///< CU supports long mode
    uint32_t htt     : 1;   ///< CPU has hyper-threading technology
    uint32_t apic    : 1;   ///< CPU has an APIC and it is enabled
    uint32_t x2apic  : 1;   ///< CPU support x2APIC feature
    uint32_t tsc     : 1;   ///< CPU has RDTSC and RDTSCP instruction support
    uint32_t dca     : 1;   ///< CPU supports direct cache access
    /* instructions */
    uint32_t monitor : 1;   ///< CPU has monitor/mwait instructions
    uint32_t cmov    : 1;   ///< CPU supports conditional move instructions

    /* virtual memory */
    uint32_t pse36  : 1;    ///< CPU has page-size extensions
    uint32_t pse    : 1;    ///< CPU has page-size extensions
    uint32_t pae    : 1;    ///< CPU supports physical address extensions
    uint32_t pat    : 1;    ///< CPU has page attribute table
    uint32_t pge    : 1;    ///< CPU has page global extension
    uint32_t mtrr   : 1;    ///< CPU has memory-type range registrs
    uint32_t page1G : 1;    ///< CPU supports huge-pages
    uint32_t page2M : 1;
    uint32_t nx     : 1;    ///< CPU supports no-execute bit

    /* cache control */
    uint32_t clsh    : 1;
    uint32_t cnxt_id : 1;   ///< L1 context ID

    /* virtualization technology */
    uint32_t vmx    : 1;    ///< CPU has virtual machine extensions
    uint32_t vme    : 1;    ///< CPU has virtual machine enhancemente
    uint32_t svm    : 1;    ///< secure virtual machine support

    /* vector instructions */
    uint32_t mmx   : 1;     ///< CPU has MMX support
    uint32_t sse   : 1;     ///< CPU has SSE support
    uint32_t sse2  : 1;     ///< CPU has SSE2 support
    uint32_t sse3  : 1;     ///< CPU has SSE3 support
    uint32_t sse41 : 1;     ///< CPU has SSE4.1 support
    uint32_t sse42 : 1;     ///< CPU has SSE4.1 support
    uint32_t avx   : 1;     ///< CPU has AVX support
};

/**
 * \brief obtains the CPU's feature information
 *
 * \param fi    structure to be filled in
 *
 * \return  SYS_ERR_OK on success
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_feature_info(struct cpuid_featureinfo *fi);

/*
 * ===============================================================================
 * address space information
 * ===============================================================================
 */

///< address space size information
struct cpuid_adressspaceinfo
{
    uint8_t physical;       ///< Maximum physical byte address size in bits
    uint8_t virtual;        ///< Maximum linear byte address size in bits
    uint8_t guest_physical; ///< maximum guest physical byte address size in bits
};

/**
 * \brief obtains the address space size information of the CPU
 *
 * \param ai  struct to be filled in with adderss space information
 *
 * \returns SYS_ERR_OK on susccess
 *          CPUID_ERR_* on failure
 */
errval_t cpuid_address_space_info(struct cpuid_adressspaceinfo *ai);


#endif // CPUID_H_

