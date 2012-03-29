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

#define TRACE_PMEM_CAPS
#define TRACE_PMEM_BEGIN 0x0
#define TRACE_PMEM_SIZE  (~(uint32_t)0)
struct cte;

struct delete_list {
    struct cte *next;
    cslot_t next_slot;
};

STATIC_ASSERT((sizeof(struct capability) + sizeof(struct mdbnode)
               + sizeof(struct delete_list)) <= (1UL << OBJBITS_CTE),
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
    struct mdbnode      mdbnode;        ///< MDB node for the cap
    struct delete_list  delete_node;    ///< State for in-progress delete cascades

    /// Padding to fill the struct out to the size required by OBJBITS_CTE
    char padding[(1UL << OBJBITS_CTE)
                 - sizeof(struct capability) - sizeof(struct mdbnode)
                 - sizeof(struct delete_list)];
};

static inline struct cte *caps_locate_slot(lpaddr_t cnode, cslot_t offset)
{
    return (struct cte *)(local_phys_to_mem(cnode) +
                          (1UL << OBJBITS_CTE) * offset);
}

int sprint_cap(char *buf, size_t len, struct capability *cap);
void caps_trace(const char *func, int line, struct cte *cte);
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
                            struct cte *src_cte, uintptr_t param1,
                            uintptr_t param2);
errval_t page_mappings_unmap(struct capability *pgtable, size_t entry);

errval_t caps_retype(enum objtype type, size_t objbits,
                     struct capability *dest_cnode,
                     cslot_t dest_slot, struct cte *src_cte,
                     bool from_monitor);
errval_t is_retypeable(struct cte *src_cte,
                       enum objtype src_type,
                       enum objtype dest_type,
                       bool from_monitor);

errval_t caps_delete_last(struct cte *cte, struct cte *ret_ram_cap);
errval_t caps_continue_clear(struct cte *ret_next);
errval_t caps_continue_revoke(struct cte *target, struct cte *ret_next);
errval_t caps_delete(struct cte *cte);
errval_t caps_revoke(struct cte *cte);

errval_t caps_lookup_cap(struct capability *cnode_cap, capaddr_t cptr,
                         uint8_t vbits, struct capability **ret,
                         CapRights rights);
errval_t caps_lookup_slot(struct capability *cnode_cap, capaddr_t cptr,
                          uint8_t vbits, struct cte **ret, CapRights rights);
void mdb_remove_recursively(struct cte *cte);
void mdb_insert_recursively(struct cte *cte);

#ifdef TRACE_PMEM_CAPS
static inline bool caps_should_trace(struct capability *cap)
{
    genpaddr_t begin = get_address(cap);
    gensize_t size = get_size(cap);
    genpaddr_t end = begin+size;
    return (begin < TRACE_PMEM_BEGIN && end > TRACE_PMEM_BEGIN)
        || (begin >= TRACE_PMEM_BEGIN && begin < (TRACE_PMEM_BEGIN+TRACE_PMEM_SIZE));
}
#define TRACE_CAP(trace_cte) do { \
    struct cte *__tmp_cte = (trace_cte); \
    if (caps_should_trace(&__tmp_cte->cap)) { \
        caps_trace(__func__, __LINE__, __tmp_cte); \
    } \
} while (0)
#else
#define TRACE_CAP(trace_cte) ((void)0)
#endif

#endif
