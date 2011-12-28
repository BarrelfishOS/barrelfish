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

/**
 * \brief A mapping database node.
 */
struct mdbnode {
    struct cte          *prev, *next;
    bool                revocable;
    bool                remote_relations;
};

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

    /// Padding to fill the struct out to the size required by OBJBITS_CTE
    char padding[(1UL << OBJBITS_CTE)
                 - sizeof(struct capability) - sizeof(struct mdbnode)];
};

static inline struct cte *caps_locate_slot(lpaddr_t cnode, cslot_t offset)
{
    return (struct cte *)(local_phys_to_mem(cnode) +
                          (1UL << OBJBITS_CTE) * offset);
}

errval_t caps_create_new(enum objtype type, lpaddr_t addr, size_t bits,
                         size_t objbits, struct cte *caps);
errval_t caps_create_from_existing(struct capability *root, capaddr_t cnode_cptr,
                                   int cnode_vbits, cslot_t dest_slot,
                                   struct capability *src);
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

errval_t caps_delete(struct cte *cte, bool from_monitor);
errval_t caps_revoke(struct cte *cte, bool from_monitor);

errval_t caps_lookup_cap(struct capability *cnode_cap, capaddr_t cptr,
                         uint8_t vbits, struct capability **ret,
                         CapRights rights);
errval_t caps_lookup_slot(struct capability *cnode_cap, capaddr_t cptr,
                          uint8_t vbits, struct cte **ret, CapRights rights);
void mdb_remove_recursively(struct cte *cte);
void mdb_insert_recursively(struct cte *cte);

#endif
