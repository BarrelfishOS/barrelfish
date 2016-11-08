/**
 * \file
 * \brief Essential capability definitions.
 */

/*
 * Copyright (c) 2007-2012, 2016, ETH Zurich.
 * Copyright (c) 2015, 2016 Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_CAPABILITIES_H
#define BARRELFISH_CAPABILITIES_H

/* FIXME: OBJBITS and OBJSIZE defines must match sizes in Hamlet's capabilities/caps.hl */

// Size of CNode entry
#define OBJBITS_CTE             6

/// Number of entries in L2 CNode in bits
#define L2_CNODE_BITS           8
/// Number of entries in L2 CNode
#define L2_CNODE_SLOTS          (1UL << L2_CNODE_BITS)

#ifndef __ASSEMBLER__

#include <assert.h>
#include <stdbool.h>
#include <barrelfish_kpi/types.h>

#define CAPRIGHTS_READ          (1 << 0)
#define CAPRIGHTS_WRITE         (1 << 1)
#define CAPRIGHTS_EXECUTE       (1 << 2)
#define CAPRIGHTS_GRANT         (1 << 3)
#define CAPRIGHTS_IDENTIFY      (1 << 4)
#define CAPRIGHTS_NUM           5

#define CAPRIGHTS_ALLRIGHTS     ((1 << CAPRIGHTS_NUM) - 1)
#define CAPRIGHTS_READ_WRITE    (CAPRIGHTS_READ | CAPRIGHTS_WRITE)
#define CAPRIGHTS_NORIGHTS      0

typedef uint8_t         CapRights;
#define PRIuCAPRIGHTS PRIu8
#define PRIxCAPRIGHTS PRIx8

struct dcb;

// capbits needs CapRights and dcb;
#include <barrelfish_kpi/capbits.h>

STATIC_ASSERT((L2_CNODE_SLOTS  * (1UL << OBJBITS_CTE)) == OBJSIZE_L2CNODE,
        "l2 cnode size doesn't match cte size");

static inline bool type_is_vnode(enum objtype type)
{
    STATIC_ASSERT(48 == ObjType_Num, "Check VNode definitions");

    return (type == ObjType_VNode_x86_64_pml4 ||
            type == ObjType_VNode_x86_64_pdpt ||
            type == ObjType_VNode_x86_64_pdir ||
            type == ObjType_VNode_x86_64_ptable ||
            type == ObjType_VNode_x86_32_pdpt ||
            type == ObjType_VNode_x86_32_pdir ||
            type == ObjType_VNode_x86_32_ptable ||
            type == ObjType_VNode_AARCH64_l3 ||
            type == ObjType_VNode_AARCH64_l2 ||
            type == ObjType_VNode_AARCH64_l1 ||
            type == ObjType_VNode_AARCH64_l0 ||
            type == ObjType_VNode_ARM_l2 ||
            type == ObjType_VNode_ARM_l1
           );
}

static inline bool type_is_vroot(enum objtype type)
{
    STATIC_ASSERT(48 == ObjType_Num, "Check VNode definitions");

    return (type == ObjType_VNode_x86_64_pml4 ||
#ifdef CONFIG_PAE
            type == ObjType_VNode_x86_32_pdpt ||
#else
            type == ObjType_VNode_x86_32_pdir ||
#endif
            type == ObjType_VNode_AARCH64_l0 ||
            type == ObjType_VNode_ARM_l1
           );
}
/**
 * Return size of vnode in bits. This is the size of a page table page.
 *
 * @param type Object type.
 *
 * @return Number of bits a VNode object occupies.
 */
static inline size_t vnode_objbits(enum objtype type)
{
    // This function should be emitted by hamlet or somesuch.
    STATIC_ASSERT(48 == ObjType_Num, "Check VNode definitions");

    if (type == ObjType_VNode_x86_64_pml4 ||
        type == ObjType_VNode_x86_64_pdpt ||
        type == ObjType_VNode_x86_64_pdir ||
        type == ObjType_VNode_x86_64_ptable ||
        type == ObjType_VNode_x86_32_pdpt ||
        type == ObjType_VNode_x86_32_pdir ||
        type == ObjType_VNode_x86_32_ptable)
    {
        return 12;
    }
    else if (type == ObjType_VNode_AARCH64_l0 ||
             type == ObjType_VNode_AARCH64_l1 ||
             type == ObjType_VNode_AARCH64_l2 ||
             type == ObjType_VNode_AARCH64_l3)
    {
        return 12;
    }
    else if (type == ObjType_VNode_ARM_l1)
    {
        return 14;
    }
    else if (type == ObjType_VNode_ARM_l2)
    {
        return 10;
    }

    assert(0 && !"Page table size unknown.");
    return 0;
}

/**
 * Return size of vnode in bytes. This is the size of a page table page.
 *
 * @param type Object type.
 *
 * @return Size of a VNode in bytes.
 *
 * XXX: this should probably just do 1UL << vnode_objbits(type) for vnode
 * objtypes -SG, 2016-07-06.
 */
static inline size_t vnode_objsize(enum objtype type)
{
    // This function should be emitted by hamlet or somesuch.
    STATIC_ASSERT(48 == ObjType_Num, "Check VNode definitions");

    if (type == ObjType_VNode_x86_64_pml4 ||
        type == ObjType_VNode_x86_64_pdpt ||
        type == ObjType_VNode_x86_64_pdir ||
        type == ObjType_VNode_x86_64_ptable ||
        type == ObjType_VNode_x86_32_pdpt ||
        type == ObjType_VNode_x86_32_pdir ||
        type == ObjType_VNode_x86_32_ptable)
    {
        // XXX: cannot use BASE_PAGE_SIZE here because asmoffsets does not
        // include the right files
        return 4096; // BASE_PAGE_SIZE
    }
    else if (type == ObjType_VNode_AARCH64_l0 ||
             type == ObjType_VNode_AARCH64_l1 ||
             type == ObjType_VNode_AARCH64_l2 ||
             type == ObjType_VNode_AARCH64_l3)
    {
        return 4096;
    }
    else if (type == ObjType_VNode_ARM_l1)
    {
        // ARMv7 L1 page table is 16kB.
        return 16384;
    }
    else if (type == ObjType_VNode_ARM_l2)
    {
        return 1024;
    }

    assert(0 && !"Page table size unknown.");
    return 0;
}

/**
 * Return number of page table entries for vnode in bits.
 * @param type Object type.
 * @return Number of page table entries in bits
 */
static inline size_t vnode_entry_bits(enum objtype type) {
    // This function should be emitted by hamlet or somesuch.
    STATIC_ASSERT(48 == ObjType_Num, "Check VNode definitions");

    if (type == ObjType_VNode_x86_64_pml4 ||
        type == ObjType_VNode_x86_64_pdpt ||
        type == ObjType_VNode_x86_64_pdir ||
        type == ObjType_VNode_x86_64_ptable)
    {
        return 9;      // log2(X86_64_PTABLE_SIZE)
    }
#ifdef CONFIG_PAE
    if (type == ObjType_VNode_x86_32_pdpt)
    {
        return 2;       // log2(X86_32_PDPTE_SIZE)
    }
    else if (type == ObjType_VNode_x86_32_pdir ||
             type == ObjType_VNode_x86_32_ptable)
    {
        return 9;       // log2(X86_32_PTABLE_SIZE) == log2(X86_32_PDIR_SIZE)
    }
#else
    if (type == ObjType_VNode_x86_32_pdir ||
        type == ObjType_VNode_x86_32_ptable)
    {
        return 10;      // log2(X86_32_PTABLE_SIZE) == log2(X86_32_PDIR_SIZE)
    }
#endif

    if (type == ObjType_VNode_AARCH64_l0 ||
        type == ObjType_VNode_AARCH64_l1 ||
        type == ObjType_VNode_AARCH64_l2 ||
        type == ObjType_VNode_AARCH64_l3)
    {
        return 9;       // log2(ARM_MAX_ENTRIES)
    }

    if (type == ObjType_VNode_ARM_l2)
    {
        return 9;       // log2(ARM_L2_MAX_ENTRIES)
    }
    else if (type == ObjType_VNode_ARM_l1)
    {
        return 12;      // log2(ARM_L1_MAX_ENTRIES)
    }

    assert(!"unknown page table type");
    return 0;
}

/**
 * Return number of slots for cnode in bits.
 * @param type Object type.
 * @return Number of page table entries in bits
 */
static inline size_t cnode_get_slots(struct capability *cnode) {
    STATIC_ASSERT(48 == ObjType_Num, "Check CNode definitions");

    switch (cnode->type) {
        case ObjType_L1CNode:
            return cnode->u.l1cnode.allocated_bytes / (1UL << OBJBITS_CTE);
        case ObjType_L2CNode:
            return L2_CNODE_SLOTS;
        default:
            assert(!"not a cnode");
            return 0;
    }
}

static inline enum objtype get_mapping_type(enum objtype captype)
{
    STATIC_ASSERT(48 == ObjType_Num, "Knowledge of all mapping types");

    switch (captype) {
        case ObjType_Frame:
            return ObjType_Frame_Mapping;
        case ObjType_DevFrame:
            return ObjType_DevFrame_Mapping;
        case ObjType_VNode_x86_64_pml4:
            return ObjType_VNode_x86_64_pml4_Mapping;
        case ObjType_VNode_x86_64_pdpt:
            return ObjType_VNode_x86_64_pdpt_Mapping;
        case ObjType_VNode_x86_64_pdir:
            return ObjType_VNode_x86_64_pdir_Mapping;
        case ObjType_VNode_x86_64_ptable:
            return ObjType_VNode_x86_64_ptable_Mapping;
        case ObjType_VNode_x86_32_pdpt:
            return ObjType_VNode_x86_32_pdpt_Mapping;
        case ObjType_VNode_x86_32_pdir:
            return ObjType_VNode_x86_32_pdir_Mapping;
        case ObjType_VNode_x86_32_ptable:
            return ObjType_VNode_x86_32_ptable_Mapping;
        case ObjType_VNode_ARM_l1:
            return ObjType_VNode_ARM_l1_Mapping;
        case ObjType_VNode_ARM_l2:
            return ObjType_VNode_ARM_l2_Mapping;
        case ObjType_VNode_AARCH64_l0:
            return ObjType_VNode_AARCH64_l0_Mapping;
        case ObjType_VNode_AARCH64_l1:
            return ObjType_VNode_AARCH64_l1_Mapping;
        case ObjType_VNode_AARCH64_l2:
            return ObjType_VNode_AARCH64_l2_Mapping;
        case ObjType_VNode_AARCH64_l3:
            return ObjType_VNode_AARCH64_l3_Mapping;
        /* all other types are not mappable */
        default:
            return ObjType_Null;
    }
}

static inline bool type_is_mapping(enum objtype type)
{
    STATIC_ASSERT(48 == ObjType_Num, "Knowledge of all mapping types");

    switch (type) {
        case ObjType_Frame_Mapping:
        case ObjType_DevFrame_Mapping:
        case ObjType_VNode_x86_64_pml4_Mapping:
        case ObjType_VNode_x86_64_pdpt_Mapping:
        case ObjType_VNode_x86_64_pdir_Mapping:
        case ObjType_VNode_x86_64_ptable_Mapping:
        case ObjType_VNode_x86_32_pdpt_Mapping:
        case ObjType_VNode_x86_32_pdir_Mapping:
        case ObjType_VNode_x86_32_ptable_Mapping:
        case ObjType_VNode_ARM_l1_Mapping:
        case ObjType_VNode_ARM_l2_Mapping:
        case ObjType_VNode_AARCH64_l0_Mapping:
        case ObjType_VNode_AARCH64_l1_Mapping:
        case ObjType_VNode_AARCH64_l2_Mapping:
        case ObjType_VNode_AARCH64_l3_Mapping:
            return true;

        /* all other types are not mapping types */
        default:
            return false;
    }
}

static inline bool type_is_mappable(enum objtype type)
{
    STATIC_ASSERT(48 == ObjType_Num, "Knowledge of all mappable types");

    switch (type) {
        case ObjType_Frame:
        case ObjType_DevFrame:
        case ObjType_VNode_x86_64_pml4:
        case ObjType_VNode_x86_64_pdpt:
        case ObjType_VNode_x86_64_pdir:
        case ObjType_VNode_x86_64_ptable:
        case ObjType_VNode_x86_32_pdpt:
        case ObjType_VNode_x86_32_pdir:
        case ObjType_VNode_x86_32_ptable:
        case ObjType_VNode_ARM_l1:
        case ObjType_VNode_ARM_l2:
        case ObjType_VNode_AARCH64_l0:
        case ObjType_VNode_AARCH64_l1:
        case ObjType_VNode_AARCH64_l2:
        case ObjType_VNode_AARCH64_l3:
            return true;

        /* all other types are not mappable */
        default:
            return false;
    }
}

/**
 * CNode capability commands.
 */
enum cnode_cmd {
    CNodeCmd_Copy,      ///< Copy capability
    CNodeCmd_Mint,      ///< Mint capability
    CNodeCmd_Retype,    ///< Retype capability
    CNodeCmd_Delete,    ///< Delete capability
    CNodeCmd_Revoke,    ///< Revoke capability
    CNodeCmd_Create,    ///< Create capability
    CNodeCmd_GetState,  ///< Get distcap state for capability
    CNodeCmd_GetSize,   ///< Get Size of CNode, only applicable for L1 Cnode
    CNodeCmd_Resize,    ///< Resize CNode, only applicable for L1 Cnode
};

enum vnode_cmd {
    VNodeCmd_Map,
    VNodeCmd_Unmap,
    VNodeCmd_Identify,   ///< Return the physical address of the VNode
};

/**
 * Mapping commands
 */
enum mapping_cmd {
    MappingCmd_Modify,
    MappingCmd_Destroy,
};

/**
 * Kernel capabilities commands.
 * Monitor's invocations of capability operations
 * which the kernel will not subject to cross core checks
 */
enum kernel_cmd {
    KernelCmd_Spawn_core,         ///< Spawn a new kernel
    KernelCmd_Identify_cap,       ///< Return the meta data of a capability
    KernelCmd_Identify_domains_cap, ///< Return the meta data of another domain's capability
    KernelCmd_Remote_relations,   ///< Set capability as being remote
    KernelCmd_Cap_has_relations,      ///< Return presence of local relations
    KernelCmd_Create_cap,         ///< Create a new capability
    KernelCmd_Copy_existing,
    KernelCmd_Get_core_id,        ///< Returns the id of the core the domain is on
    KernelCmd_Get_arch_id,        ///< Returns arch id of caller's core
    KernelCmd_Nullify_cap,        ///< Set the capability to NULL allowed it to be reused
    KernelCmd_Setup_trace,        ///< Set up trace buffer
    KernelCmd_Register,           ///< Register monitor notify endpoint
    KernelCmd_Domain_Id,          ///< Set domain ID of dispatcher
    KernelCmd_Get_cap_owner,
    KernelCmd_Set_cap_owner,
    KernelCmd_Lock_cap,
    KernelCmd_Unlock_cap,
    KernelCmd_Delete_last,
    KernelCmd_Delete_foreigns,
    KernelCmd_Revoke_mark_target,
    KernelCmd_Revoke_mark_relations,
    KernelCmd_Delete_step,
    KernelCmd_Clear_step,
    KernelCmd_Retype,
    KernelCmd_Has_descendants,
    KernelCmd_Is_retypeable,
    KernelCmd_Sync_timer,
    KernelCmd_IPI_Register,
    KernelCmd_IPI_Delete,
    KernelCmd_GetGlobalPhys,
    KernelCmd_Add_kcb,            ///< add extra kcb to be scheduled
    KernelCmd_Remove_kcb,         ///< remove kcb from scheduling ring
    KernelCmd_Suspend_kcb_sched,  ///< suspend/resume kcb scheduler
    KernelCmd_Get_platform,       ///< Get architecture platform
    KernelCmd_Count
};

/**
 * Specific commands for dispatcher capabilities.
 */
enum dispatcher_cmd {
    DispatcherCmd_Setup,            ///< Set dispatcher parameters
    DispatcherCmd_Properties,       ///< Set dispatcher properties
    DispatcherCmd_PerfMon,          ///< Performance monitoring
    DispatcherCmd_SetupGuest,       ///< Set up the DCB of a guest domain
    DispatcherCmd_DumpPTables,      ///< Dump hw page tables of dispatcher
    DispatcherCmd_DumpCapabilities, ///< Dump capabilities of dispatcher
    DispatcherCmd_Vmread,           ///< Execute vmread on the current and active VMCS      
    DispatcherCmd_Vmwrite,          ///< Execute vmwrite on the current and active VMCS
    DispatcherCmd_Vmptrld,          ///< Make VMCS clear and inactive
    DispatcherCmd_Vmclear,          ///< Make VMCS current and active 
};

/**
 * Frame capability commands.
 */
enum frame_cmd {
    FrameCmd_Identify,      ///< Return physical address of frame
};

/**
 * Kernel control block commands.
 */
enum kcb_cmd {
    KCBCmd_Clone = FrameCmd_Identify+1, ///< Duplicate core_data
};

/**
 * RAM capability commands
 */
enum ram_cmd {
    RAMCmd_Identify,      ///< Return physical address of frame
};

/**
 * IRQ Table capability commands.
 */
enum irqtable_cmd {
    IRQTableCmd_Alloc,  ///< Allocate new vector (XXX: HACK: this is x86 specific)
    IRQTableCmd_AllocDestCap,  ///< Allocate new dest capability (XXX: HACK: this is x86 specific)
    IRQTableCmd_Set,    ///< Set endpoint for IRQ# notifications
    IRQTableCmd_Delete  ///< Remove notification endpoint for IRQ#
};

/**
 * IRQ Vector commands.
 */

enum irqdest_cmd {
	IRQDestCmd_Connect,	///< Connect this capability to a messaging channel
	IRQDestCmd_GetVector, ///< Return the local interrupt vector
	IRQDestCmd_GetCpu ///< Return the local interrupt vector
};

/**
 * IRQ Vector commands.
 */

enum irqsrc_cmd {
    IRQSrcCmd_GetVecStart,   ///< Return vector range start
    IRQSrcCmd_GetVecEnd   ///< Return vector range high
};


/**
 * IO capability commands.
 */
enum io_cmd {
    IOCmd_Outb,         ///< Output byte to port
    IOCmd_Outw,         ///< Output word to port
    IOCmd_Outd,         ///< Output double word to port
    IOCmd_Inb,          ///< Input byte from port
    IOCmd_Inw,          ///< Input word from port
    IOCmd_Ind           ///< Input double word from port
};

/**
 * Notify capability commands.
 */
enum notify_cmd {
    NotifyCmd_Send
};


/**
 * Performance monitoring commands.
 * Seems to be already included in the Dispatcher capability.
 */
enum perfmon_cmd {
    PerfmonCmd_Activate,    ///< Activate performance counters
    PerfmonCmd_Deactivate,  ///< Deactivate performance counters
    PerfmonCmd_Write        ///< Read current performance counter values
};


/**
 * ID capability commands.
 */
enum id_cmd {
    IDCmd_Identify  ///< Return system-wide unique ID
};

/**
 * IPI capability commands
 */

enum ipi_cmd {
    IPICmd_Send_Start,     ///< Send Startup IPI to a destination core
    IPICmd_Send_Init,      ///< Send Init IPI to a destination core
};
/**
 * Maximum command ordinal.
 */
#define CAP_MAX_CMD KernelCmd_Count

/**
 * \brief Values returned from frame identify invocation
 */
struct frame_identity {
    genpaddr_t base;   ///< Physical base address of frame
    gensize_t  bytes;  ///< Size of frame, in bytes
};

/**
 * \brief Values returned from the VNode identify invocation
 */
struct vnode_identity {
    genpaddr_t base;   ///< Physical base address of the VNode
    uint8_t type;      ///< Type of VNode
};

#endif // __ASSEMBLER__

#endif // BARRELFISH_CAPABILITIES_H
