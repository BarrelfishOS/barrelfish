/**
 * \file
 * \brief init user-space domain special structures.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_INIT_H
#define BARRELFISH_KPI_INIT_H

#include <stdbool.h>

/**
 * Size of bootinfo mapping.
 */
#define BOOTINFO_SIZEBITS       (BASE_PAGE_BITS + 2)
#define BOOTINFO_SIZE           (1UL << BOOTINFO_SIZEBITS)

/**
 * Address (in spawned domain) of page with command-line arguments and environment
 */
#define ARGS_FRAME_BITS    17 // 128 kB
#define ARGS_SIZE          ((genpaddr_t)1 << ARGS_FRAME_BITS)
#define MAX_CMDLINE_ARGS   128
#define MAX_ENVIRON_VARS   128

/**
 * Size of page with inherited file descriptors
 */
#define FDS_SIZE          (32 * 1024)  // estimate worst case here.


/**
 * Size of dispatcher frame
 */
#define DISPATCHER_SIZE         ((genpaddr_t)1 << DISPATCHER_FRAME_BITS)

/**
 * Size of initial page to carry out monitor URPC
 */
// Supports at least #MON_URPC_CHANNEL_LEN
// Change #MON_URPC_CHANNEL_LEN if changing this
#define MON_URPC_SIZE           (2 * BASE_PAGE_SIZE)

/**

 * Maximum possible number of entries in the memory regions array. This is
 * limited by the page size minus the size of the initial part of the bootinfo
 * structure.
 */
#define MAX_MEM_REGIONS         ((BOOTINFO_SIZE - sizeof(struct bootinfo)) / \
                                 sizeof(struct mem_region))

/* Root CNode */
#define ROOTCN_SLOT_TASKCN       0   ///< Taskcn slot in root cnode
#define ROOTCN_SLOT_PAGECN       1   ///< Pagecn slot in root cnode
#define ROOTCN_SLOT_BASE_PAGE_CN 2   ///< Slot for a cnode of BASE_PAGE_SIZE frames
#define ROOTCN_SLOT_SUPERCN      3   ///< Slot for a cnode of SUPER frames
#define ROOTCN_SLOT_SEGCN        4   ///< SegCN slot in root cnode
#define ROOTCN_SLOT_PACN         5   ///< PhysAddr cnode slot in root cnode
#define ROOTCN_SLOT_MODULECN     6   ///< Multiboot modules cnode slot in root cnode
#define ROOTCN_SLOT_SLOT_ALLOC0  7   ///< Root of slot alloc0
#define ROOTCN_SLOT_SLOT_ALLOC1  8   ///< Root of slot alloc1
#define ROOTCN_SLOT_SLOT_ALLOC2  9   ///< Root of slot alloc2
#define ROOTCN_SLOT_ARGCN        10  ///< Argcn slot in root cnode
#define ROOTCN_SLOT_BSPKCB       11  ///< BSP KCB cap to fix reverse lookup issues
#define ROOTCN_SLOTS_USER        12  ///< First free slot in root cnode for user

/* Size of CNodes in Root CNode if not the default size */
#define SLOT_ALLOC_CNODE_BITS   L2_CNODE_BITS
#define SLOT_ALLOC_CNODE_SLOTS  L2_CNODE_SLOTS

/* Task CNode */
#define TASKCN_SLOT_TASKCN      0   ///< Task CNode in itself (XXX)
#define TASKCN_SLOT_DISPATCHER  1   ///< Dispatcher cap in task cnode
#define TASKCN_SLOT_ROOTCN      2   ///< RootCN slot in task cnode
#define TASKCN_SLOT_DISPFRAME   4   ///< Dispatcher frame cap in task cnode
#define TASKCN_SLOT_IRQ         5   ///< IRQ cap in task cnode
#define TASKCN_SLOT_IO          6   ///< IO cap in task cnode
#define TASKCN_SLOT_BOOTINFO    7   ///< Bootinfo frame slot in task cnode
#define TASKCN_SLOT_KERNELCAP   8   ///< Kernel cap in task cnode
#define TASKCN_SLOT_TRACEBUF    9   ///< Trace buffer cap in task cnode
#define TASKCN_SLOT_ARGSPAGE    10  ///< ?
#define TASKCN_SLOT_MON_URPC    11  ///< Frame cap for urpc comm.
#define TASKCN_SLOT_SESSIONID   12  ///< Session ID domain belongs to
#define TASKCN_SLOT_FDSPAGE     13  ///< cap for inherited file descriptors
#define TASKCN_SLOT_PERF_MON    14  ///< cap for performance monitoring
#define TASKCN_SLOT_SYSMEM      15  ///< ???
#define TASKCN_SLOT_COREBOOT    16  ///< Copy of realmode section used to bootstrap a core
#define TASKCN_SLOT_IPI         17  ///< Copy of IPI cap
#define TASKCN_SLOTS_USER       18  ///< First free slot in taskcn for user

/* Page CNode */
#define PAGECN_SLOT_VROOT       0 ///< First slot of page cnode is root page table

#define ROOTCN_SLOT_LEVEL       CSPACE_LEVEL_L1
#define ROOTCN_SLOT_ADDR(slot)  ((slot) << L2_CNODE_BITS)

// Cspace addresses for well-defined L2 CNodes
#define CPTR_TASKCN_BASE        ROOTCN_SLOT_ADDR(ROOTCN_SLOT_TASKCN)
#define CPTR_BASE_PAGE_CN_BASE  ROOTCN_SLOT_ADDR(ROOTCN_SLOT_BASE_PAGE_CN)
#define CPTR_SUPERCN_BASE       ROOTCN_SLOT_ADDR(ROOTCN_SLOT_SUPERCN)
#define CPTR_PHYADDRCN_BASE     ROOTCN_SLOT_ADDR(ROOTCN_SLOT_PACN)
#define CPTR_MODULECN_BASE      ROOTCN_SLOT_ADDR(ROOTCN_SLOT_MODULECN)
#define CPTR_PAGECN_BASE        ROOTCN_SLOT_ADDR(ROOTCN_SLOT_PAGECN)

/**
 * Memory region types.
 */
enum region_type {
    /// Empty memory: describes a RAM cap in supercn
    RegionType_Empty,
    /// Code/Data of init itself: describes a Frame cap in segcn
    RegionType_RootTask,
    /// Physical address range (not RAM): describes a PhysAddr cap in physaddrcn
    RegionType_PhyAddr,
    /// BIOS tables and platform-specific data: describes a PhysAddr cap in physaddrcn
    RegionType_PlatformData,
    /// Multiboot module: describes multiple Frame caps in modulecn
    RegionType_Module,
    ///< describes memory region that is an ACPI table
    RegionType_ACPI_TABLE,
    RegionType_Max ///< Must be last
};

/**
 * A memory region.
 */
struct mem_region {
    genpaddr_t       mr_base;///< Address of the start of the region
    enum region_type mr_type;///< Type of region
    gensize_t        mr_bytes;///< Size in bytes
    bool             mr_consumed;///< Flag for user code to mark region consumed
    size_t           mrmod_size;///< Size in bytes (module type only)
    ptrdiff_t        mrmod_data;///< Offset of module string (module type only)
    int              mrmod_slot;///< First slot containing caps (module only)
};

/**
 * This structure holds essential information for the init process to
 * allocate and manage its address space.
 */
struct bootinfo {
    /// For __k1om__
    uint64_t host_msg;
    uint8_t host_msg_bits;

    /// Number of entries in regions array
    size_t              regions_length;
    /// Amount of memory required to spawn another core
    size_t              mem_spawn_core;
    /// Memory regions array
    struct mem_region   regions[];
};

#endif
