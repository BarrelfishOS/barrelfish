/*
 * \brief demandpaging.h
 *
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef LIB_DEMANDPAGING_INCLUDE_DEMANDPAGING_INTERNAL_H_
#define LIB_DEMANDPAGING_INCLUDE_DEMANDPAGING_INTERNAL_H_

#include <demandpaging.h>

/* forward declarations */
struct dp_frame;
struct dp_page;
struct demand_paging_region;


#define DP_DEBUG_ENABLE 1
#define DP_DEBUG_PRINT_LINENUMBERS 0

#define DP_DEBUG_MAP_ENABLE 1
#define DP_DEBUG_SWAP_ENABLE 1
#define DP_DEBUG_HANDLER_ENABLE 1

/* default debug print utility */
#if !defined(NDEBUG) && DP_DEBUG_ENABLE
#if DP_DEBUG_PRINT_LINENUMBERS
#define DP_DEBUG_PRINT(sub, fmt, ...) \
                debug_printf("[dp] [%s] %s:%u - " fmt, sub, \
                             __FUNCTION__, __LINE__, ##__VA_ARGS__)
#else
#define DP_DEBUG_PRINT(sub, fmt, ...) \
                debug_printf("[dp] [%s] " fmt, sub, ##__VA_ARGS__)
#endif
#else
#define DP_DEBUG_PRINT(fmt, ...)
#endif


#if DP_DEBUG_SWAP_ENABLE
#define DP_DEBUG_SWAP(fmt, ... ) \
                DP_DEBUG_PRINT( "swap", fmt, ##__VA_ARGS__)
#else
#define DP_DEBUG_SWAP(fmt, ... )
#endif

#if DP_DEBUG_MAP_ENABLE
#define DP_DEBUG_MAP(fmt, ... ) \
                DP_DEBUG_PRINT( "map", fmt, ##__VA_ARGS__)
#else
#define DP_DEBUG_MAP(fmt, ... )
#endif

#if DP_DEBUG_HANDLER_ENABLE
#define DP_DEBUG_HANDLER(fmt, ... ) \
                DP_DEBUG_PRINT( "handler", fmt, ##__VA_ARGS__)
#else
#define DP_DEBUG_HANDLER(fmt, ... )
#endif

#if DP_DEBUG_MGMT_ENABLE
#define DP_DEBUG_MGMT(fmt, ... ) \
                DP_DEBUG_PRINT( "mgmt", fmt, ##__VA_ARGS__)
#else
#define DP_DEBUG_MGMT(fmt, ... )
#endif


/*
 * XXX: for accessing the dirty and accessed bits
 */

#ifdef __k1om__
#define X86_64_PHYSADDR_BITS 40 // TODO: Take that from offsets target
#else
#define X86_64_PHYSADDR_BITS 48
#endif

#define X86_64_PAGING_ENTRY_SIZE 64
#define X86_64_PAGING_AVAIL2_BITS 11
#define X86_64_PAGING_FLAG_BITS 12
#define X86_64_PAGING_LARGE_FLAGE_BITS 21
#define X86_64_PAGING_RESERVED_BITS \
                (X86_64_PAGING_ENTRY_SIZE - X86_64_PHYSADDR_BITS - \
                 X86_64_PAGING_AVAIL2_BITS - 1)
#define X86_64_PAGING_LARGE_BASE_BITS \
                (X86_64_PHYSADDR_BITS - X86_64_PAGING_LARGE_FLAGE_BITS)
#define X86_64_PAGING_BASE_BASE_BITS \
                (X86_64_PHYSADDR_BITS - X86_64_PAGING_FLAG_BITS)

/**
 * A page table entry.
 */
union x86_64_ptable_entry {
    uint64_t raw;
    struct {
        uint64_t        present         :1;
        uint64_t        read_write      :1;
        uint64_t        user_supervisor :1;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        accessed        :1;
        uint64_t        dirty           :1;
        uint64_t        always1         :1;
        uint64_t        global          :1;
        uint64_t        available       :3;
        uint64_t        attr_index      :1;
        uint64_t        reserved        :17;
        uint64_t        base_addr       :10;
        uint64_t        reserved2       :12;
        uint64_t        available2      :11;
        uint64_t        execute_disable :1;
    } huge;
    struct {
        uint64_t        present         :1;
        uint64_t        read_write      :1;
        uint64_t        user_supervisor :1;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        accessed        :1;
        uint64_t        dirty           :1;
        uint64_t        always1         :1;
        uint64_t        global          :1;
        uint64_t        available       :3;
        uint64_t        attr_index      :1;
        uint64_t        reserved        :8;
        uint64_t        base_addr       :X86_64_PAGING_LARGE_BASE_BITS;
        uint64_t        reserved2       :X86_64_PAGING_RESERVED_BITS;
        uint64_t        available2      :11;
        uint64_t        execute_disable :1;
    } large;
    struct {
        uint64_t        present         :1;
        uint64_t        read_write      :1;
        uint64_t        user_supervisor :1;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        accessed        :1;
        uint64_t        dirty           :1;
        uint64_t        attr_index      :1;
        uint64_t        global          :1;
        uint64_t        available       :3;
        uint64_t        base_addr       :X86_64_PAGING_BASE_BASE_BITS;
        uint64_t        reserved2       :X86_64_PAGING_RESERVED_BITS;
        uint64_t        available2      :11;
        uint64_t        execute_disable :1;
    } base;
};


#define EXCEPTION_STACK_SIZE (32UL * 1024)
#define EXCEPTION_STACK_MIN_SIZE (4UL * 1024)

#define DEMAND_PAGING_SWAP_FILE "/swap"
#define DEMAND_PAGING_SWAP_FILE_PATHLEN (5 + 18 + 1)

typedef enum {
    DEMAND_PAGING_PST_UNTOUCHED,
    DEMAND_PAGING_PST_MEMORY,
    DEMAND_PAGING_PST_FILE,
} dp_page_state_t;

///< represent a virtual frame
struct dp_page
{
    lvaddr_t vaddr;
    size_t pagenr;
    struct demand_paging_region *dpr;

    dp_page_state_t state;    ///< this slot is page out
    size_t offset;            ///< offset into the file
    struct dp_frame *frame;

    void *vnode;
    union x86_64_ptable_entry *vnode_entry;
};

///< represent a physical frame
struct dp_frame
{
    struct capref frame;
    lpaddr_t paddr;
    struct dp_page *page;
    union x86_64_ptable_entry *vnode_entry;
    struct dp_frame *next; /// to hold the free list
};

struct demand_paging_region
{
    struct vregion vreg;   ///< the vregion of this object

    /* swap file */
    char swapname[DEMAND_PAGING_SWAP_FILE_PATHLEN];
    vfs_handle_t swapfile;

    /* frames */
    size_t pagesize;
    struct dp_page *pages;

    size_t frames_count;            ///< total length of all frames
    size_t frames_victim;            ///< total length of all frames
    struct dp_frame *frames;        ///< array of all frames
    struct dp_frame *frames_free;

    size_t vnodes_count;
    void **vnodes;


    /* next regions */
    struct demand_paging_region *next;
};


#endif /* LIB_DEMANDPAGING_INCLUDE_DEMANDPAGING_INTERNAL_H_ */
