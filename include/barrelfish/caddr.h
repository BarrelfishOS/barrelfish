/**
 * \file
 * \brief Inline functions to allow manipulation of raw capability addresses
 *
 * This file is not part of the standard includes, because most user code should
 * treat #capref as an opaque value.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDEBARRELFISH_CADDR_H
#define INCLUDEBARRELFISH_CADDR_H

#include <stdbool.h>
#include <sys/cdefs.h>

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/capabilities.h>

#include <barrelfish/cspace.h>

#include <stdint.h>
#include <stdbool.h>

#include <bitmacros.h>

__BEGIN_DECLS

/**
 * \brief extract slot (L2 index) from capability address `addr`
 * \param addr the capability address
 * \return The slot number (L2 index) component of the address, i.e. the low
 *         bits.
 */
static inline cslot_t get_capaddr_slot(capaddr_t addr)
{
    return (cslot_t)(addr & MASK_T(capaddr_t, L2_CNODE_BITS));
}

/**
 * \brief extract CNode address component from capability address `addr`
 * \param addr the capability address
 * \return the cnode component of the address, i.e. the address with the slot
 *         (L2 index) bits set to zero.
 */
static inline capaddr_t get_capaddr_cnode_addr(capaddr_t addr)
{
    return addr & ~MASK_T(capaddr_t, L2_CNODE_BITS);
}

enum cnode_type {
    CNODE_TYPE_ROOT = 0,
    CNODE_TYPE_OTHER,
    CNODE_TYPE_COUNT,
} __attribute__((packed));

/**
 * \brief User-level representation of a CNode, this is essentially a capref
 * to a CNode.
 */
struct cnoderef {
    capaddr_t croot;
    capaddr_t cnode;
    enum cnode_type level;
} __attribute__((packed));

#define NULL_CNODE (struct cnoderef){ \
    /*croot*/ 0, /*cnode*/ 0, \
    /*level*/ CNODE_TYPE_ROOT  }

/**
 * \brief User-level representation of a capability and its CSpace address
 */

struct capref {
    struct cnoderef cnode;
    cslot_t slot;
};

#define NULL_CAP (struct capref){ /*cnode*/ NULL_CNODE, /*slot*/ 0 }

static inline bool cnoderef_is_null(struct cnoderef cnoderef)
{
    return cnoderef.croot == 0 && cnoderef.cnode == 0;
}

static inline bool capref_is_null(struct capref capref)
{
    return cnoderef_is_null(capref.cnode) && capref.slot == 0;
}

/* well-known cnodes */
extern struct cnoderef cnode_root, cnode_task, cnode_base, cnode_super,
                       cnode_page, cnode_module;

/* well-known capabilities */
extern struct capref cap_root, cap_monitorep, cap_irq, cap_io, cap_dispatcher,
                     cap_selfep, cap_kernel, cap_initep, cap_perfmon, cap_dispframe,
                     cap_sessionid, cap_ipi, cap_vroot, cap_argcn, cap_procmng,
                     cap_domainid;

/**
 * \brief Returns the depth in the CSpace address of a cap
 */
static inline uint8_t get_cap_level(struct capref cap)
{
    if (capref_is_null(cap)) {
        return 0;
    } else {
        return cap.cnode.level + 1;
    }
}

/**
 * \brief Returns the CSpace address of a cap
 */
static inline capaddr_t get_cap_addr(struct capref cap)
{
    if (!capref_is_null(cap)) {
        switch (cap.cnode.level) {
            case CNODE_TYPE_ROOT:
                return cap.slot << L2_CNODE_BITS;
            // capref is in L2 CNode
            case CNODE_TYPE_OTHER:
                return cap.cnode.cnode | cap.slot;
            default:
                assert(!"invalid level");
                return 0x0;
        }
    }
    return 0;
}

/**
 * \brief Returns the depth in the CSpace address of the CNode
 *        containing the given cap
 */
static inline uint8_t get_cnode_level(struct capref cap)
{
    return cap.cnode.level;
}

/**
 * \brief Returns the CSpace address of the CNode containing the given cap
 */
static inline capaddr_t get_cnode_addr(struct capref cap)
{
    switch (cap.cnode.level) {
        case CNODE_TYPE_ROOT:
            return cap.cnode.croot;
        case CNODE_TYPE_OTHER:
            return cap.cnode.cnode;
        default:
            assert(!"unknown cnoderef type");
            return 0x0;
    }
}


/**
 * \brief Returns the CSpace address of the cspace root cap of the given cap
 */
static inline capaddr_t get_croot_addr(struct capref cap)
{
    return cap.cnode.croot;
}

static inline struct capref get_croot_capref(struct capref cap)
{
    capaddr_t croot = get_croot_addr(cap);
    struct capref ref = {
        .cnode = {
            .croot = CPTR_ROOTCN,
            .cnode = get_capaddr_cnode_addr(croot),
            .level = CNODE_TYPE_OTHER,
        },
        .slot = get_capaddr_slot(croot),
    };
    return ref;
}

/**
 * \brief Compare two cnoderefs
 *
 * Two cnoderefs are equal if they have the same base address,
 * same number of valid bits and the same guard_size.
 */
static inline bool cnodecmp(struct cnoderef c1, struct cnoderef c2)
{
    return (c1.cnode == c2.cnode && c1.croot == c2.croot && c1.level == c2.level);
}

/**
 * \brief Compare two caprefs
 *
 * Two caprefs are equal if they have the same cnoderef and the same
 * slot.
 */
static inline bool capcmp(struct capref c1, struct capref c2)
{
    return c1.slot == c2.slot && cnodecmp(c1.cnode, c2.cnode);
}

/**
 * \brief Creates a new #cnoderef struct, performing address calculations.
 * XXX: TODO remove size_bits from signature
 */
static inline struct cnoderef build_cnoderef(struct capref cap,
                                             enum cnode_type cntype)
{
    assert(cntype < CNODE_TYPE_COUNT);

    struct cnoderef cnode = NULL_CNODE;
    switch(get_cnode_level(cap)) {
        // L2 cnode in our root cnode
        case CNODE_TYPE_ROOT:
            // cannot make cnoderef from non-invokable capref.
            assert(cap.cnode.croot == CPTR_ROOTCN);
            cnode.croot = CPTR_ROOTCN;
            cnode.cnode = get_cap_addr(cap);
            cnode.level = cntype;
            break;
        // CNode for another cspace
        case CNODE_TYPE_OTHER:
            cnode.level = cntype;
            switch (cntype) {
                // creating a cnoderef to a root cnode for another cspace
                case CNODE_TYPE_ROOT:
                    cnode.croot = get_cap_addr(cap);
                    cnode.cnode = 0;
                    break;
                case CNODE_TYPE_OTHER:
                    cnode.croot = get_croot_addr(cap);
                    cnode.cnode = get_cap_addr(cap);
                    break;
                default:
                    assert(!"build_cnoderef: provided cntype invalid");
                    return NULL_CNODE;
            }
            break;
        default:
            assert(!"cap level not valid");
            return NULL_CNODE;
    }
    return cnode;
}

__END_DECLS

#endif
