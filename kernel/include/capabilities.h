/**
 * \file
 * \brief Kernel capability management.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPABILITIES_H
#define CAPABILITIES_H

#include <barrelfish_kpi/capabilities.h>
#include <mdb/mdb.h>
#include <offsets.h>
#include <cap_predicates.h>
#include <paging_generic.h>

struct cte;

#define DELETE_LIST_SIZE (sizeof(uint64_t))
struct delete_list {
    struct cte *next;
    // make sure delete list is always the same size!
    char padding[DELETE_LIST_SIZE - sizeof(struct cte*)];
};

STATIC_ASSERT((sizeof(struct capability) + sizeof(struct mdbnode)
               + sizeof(struct delete_list) + sizeof(struct mapping_info))
               <= (1UL << OBJBITS_CTE),
              "cap+mdbnode fit in cte");

/**
 * \brief A CTE (Capability Table Entry).
 *
 * A CTE is an entry in a CNode, which in turn is an entry in CSpace, the
 * capability space. CSpace is a guarded tree structure. Refer to the seL4
 * reference manual for further information about how CSpace is implemented.
 */
struct cte {
    struct capability   cap;            ///< The capability
    struct mdbnode      mdbnode;        ///< MDB "root" node for the cap
    struct delete_list  delete_node;    ///< State for in-progress delete cascades
    struct mapping_info mapping_info;   ///< Mapping info for mapped pmem capabilities

    /// Padding to fill the struct out to the size required by OBJBITS_CTE
    char padding[(1UL << OBJBITS_CTE)
                 - sizeof(struct capability) - sizeof(struct mdbnode)
                 - sizeof(struct delete_list) - sizeof(struct mapping_info)];
};

STATIC_ASSERT_SIZEOF(struct cte, (1UL << OBJBITS_CTE));

static inline struct cte *caps_locate_slot(lpaddr_t cnode, cslot_t offset)
{
    return (struct cte *)(local_phys_to_mem(cnode) +
                          (1UL << OBJBITS_CTE) * offset);
}

int sprint_cap(char *buf, size_t len, struct capability *cap);
void caps_trace(const char *func, int line, struct cte *cte, const char *msg);
errval_t caps_create_new(enum objtype type, lpaddr_t addr, size_t bits,
                         size_t objbits, coreid_t owner, struct cte *caps);
errval_t caps_create_from_existing(struct capability *root, capaddr_t cnode_cptr,
                                   int cnode_vbits, cslot_t dest_slot,
                                   coreid_t owner, struct capability *src);
errval_t caps_copy_to_cnode(struct cte *dest_cnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, bool mint, uintptr_t param1,
                            uintptr_t param2);
errval_t caps_copy_to_cte(struct cte *dest_cte, struct cte *src_cte, bool mint,
                          uintptr_t param1, uintptr_t param2);
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count);
size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages);
errval_t page_mappings_unmap(struct capability *pgtable, struct cte *mapping,
                             size_t entry, size_t num_pages);
errval_t page_mappings_modify_flags(struct capability *mapping, size_t offset,
                                    size_t pages, size_t flags);
errval_t paging_modify_flags(struct capability *frame, uintptr_t offset,
                             uintptr_t pages, uintptr_t kpi_paging_flags);
void paging_dump_tables(struct dcb *dispatcher);

errval_t caps_retype(enum objtype type, size_t objbits,
                     struct capability *dest_cnode,
                     cslot_t dest_slot, struct cte *src_cte,
                     bool from_monitor);
errval_t is_retypeable(struct cte *src_cte,
                       enum objtype src_type,
                       enum objtype dest_type,
                       bool from_monitor);

errval_t caps_lookup_cap(struct capability *cnode_cap, capaddr_t cptr,
                         uint8_t vbits, struct capability **ret,
                         CapRights rights);
errval_t caps_lookup_slot(struct capability *cnode_cap, capaddr_t cptr,
                          uint8_t vbits, struct cte **ret, CapRights rights);

/*
 * Delete and revoke
 */

errval_t caps_delete_last(struct cte *cte, struct cte *ret_ram_cap);
errval_t caps_delete_foreigns(struct cte *cte);
errval_t caps_mark_revoke(struct capability *base, struct cte *revoked);
errval_t caps_delete_step(struct cte *ret_next);
errval_t caps_clear_step(struct cte *ret_ram_cap);
errval_t caps_delete(struct cte *cte);
errval_t caps_revoke(struct cte *cte);

/*
 * Cap tracing
 */

#ifdef TRACE_PMEM_CAPS
STATIC_ASSERT(ObjType_Num == 27, "knowledge of all cap types");
#define ALL_PMEM_TYPES \
    ((1ul<<ObjType_RAM) | \
     (1ul<<ObjType_Frame) | \
     (1ul<<ObjType_DevFrame) | \
     (1ul<<ObjType_CNode) | \
     (1ul<<ObjType_FCNode) | \
     (1ul<<ObjType_VNode_x86_64_pml4) | \
     (1ul<<ObjType_VNode_x86_64_pdpt) | \
     (1ul<<ObjType_VNode_x86_64_pdir) | \
     (1ul<<ObjType_VNode_x86_64_ptable) | \
     (1ul<<ObjType_VNode_x86_32_pdpt) | \
     (1ul<<ObjType_VNode_x86_32_pdir) | \
     (1ul<<ObjType_VNode_x86_32_ptable) | \
     (1ul<<ObjType_VNode_ARM_l1) | \
     (1ul<<ObjType_VNode_ARM_l2) | \
     (1ul<<ObjType_PhysAddr) | \
     (1ul<<ObjType_KernelControlBlock))

//#define TRACE_TYPES_ENABLED_INITIAL 0x0
#define TRACE_TYPES_ENABLED_INITIAL ALL_PMEM_TYPES
#define TRACE_PMEM_BEGIN_INITIAL    0x0
#define TRACE_PMEM_SIZE_INITIAL     (~(uint32_t)0)

extern uint64_t trace_types_enabled;
extern genpaddr_t TRACE_PMEM_BEGIN;
extern gensize_t TRACE_PMEM_SIZE;
void caps_trace_ctrl(uint64_t types, genpaddr_t start, gensize_t size);
static inline bool caps_should_trace(struct capability *cap)
{
    if (!(trace_types_enabled & (1ul<<cap->type))) {
        return false;
    }
    if (!(ALL_PMEM_TYPES & (1ul<<cap->type))) {
        return true;
    }
    genpaddr_t begin = get_address(cap);
    gensize_t size = get_size(cap);
    genpaddr_t end = begin+size;
    return (begin < TRACE_PMEM_BEGIN && end > TRACE_PMEM_BEGIN)
        || (begin >= TRACE_PMEM_BEGIN && begin < (TRACE_PMEM_BEGIN+TRACE_PMEM_SIZE));
}
#define TRACE_CAP_MSG(msg, trace_cte) do { \
    struct cte *__tmp_cte = (trace_cte); \
    if (__tmp_cte && caps_should_trace(&__tmp_cte->cap)) { \
        caps_trace(__func__, __LINE__, __tmp_cte, (msg)); \
    } \
} while (0)
#define TRACE_CAP(trace_cte) TRACE_CAP_MSG(NULL, trace_cte)
#else
#define TRACE_CAP_MSG(msg, trace_cte) ((void)0)
#define TRACE_CAP(trace_cte) ((void)0)
#endif

#endif
