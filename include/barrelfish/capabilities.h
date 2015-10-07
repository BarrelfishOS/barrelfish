/**
 * \file
 * \brief Base capability/cnode handling functions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDEBARRELFISH_CAPABILITIES_H
#define INCLUDEBARRELFISH_CAPABILITIES_H

#include <stdint.h>
#include <sys/cdefs.h>

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/distcaps.h>
#include <barrelfish/invocations.h>
#include <barrelfish/slot_alloc.h>

__BEGIN_DECLS

errval_t cnode_create(struct capref *ret_dest, struct cnoderef *cnoderef,
                 cslot_t slots, cslot_t *retslots);
errval_t cnode_create_foreign_l2(struct capref dest_l1, cslot_t dest_slot, struct cnoderef *cnoderef);
errval_t cnode_create_l2(struct capref *ret_dest, struct cnoderef *cnoderef);
errval_t cnode_create_l1(struct capref *ret_dest, struct cnoderef *cnoderef);
errval_t cnode_create_raw(struct capref dest, struct cnoderef *cnoderef,
                          enum objtype cntype, cslot_t slots, cslot_t *retslots);
errval_t cnode_create_with_guard(struct capref dest, struct cnoderef *cnoderef,
                            cslot_t slots, cslot_t *retslots,
                            uint64_t guard, uint8_t guard_size);
errval_t cnode_create_from_mem(struct capref dest, struct capref src,
                               enum objtype cntype, struct cnoderef *cnoderef,
                               size_t slots);

errval_t root_cnode_resize(struct capref new, struct capref ret);

errval_t cap_retype(struct capref dest_start, struct capref src, gensize_t offset,
                    enum objtype new_type, gensize_t objsize, size_t count);
errval_t cap_create(struct capref dest, enum objtype type, size_t bytes);
errval_t cap_delete(struct capref cap);
errval_t cap_revoke(struct capref cap);
struct cspace_allocator;
errval_t cap_destroy(struct capref cap);

errval_t vnode_create(struct capref dest, enum objtype type);
errval_t frame_create(struct capref dest, size_t bytes, size_t *retbytes);
errval_t frame_alloc(struct capref *dest, size_t bytes, size_t *retbytes);
errval_t devframe_type(struct capref *dest, struct capref src, uint8_t bits);
errval_t dispatcher_create(struct capref dest);

typedef void (*handler_func_t)(void *);
struct lmp_endpoint;

errval_t endpoint_create(size_t buflen, struct capref *retcap,
                         struct lmp_endpoint **retep);

errval_t idcap_alloc(struct capref *dest);
errval_t idcap_create(struct capref dest);

errval_t cnode_build_cnoderef(struct cnoderef *cnoder, struct capref capr);
errval_t cnode_build_l1cnoderef(struct cnoderef *cnoder, struct capref capr);

/**
 * \brief Mint (Copy changing type-specific parameters) a capability
 *
 * \param dest    Location of destination slot, which must be empty
 * \param src     Location of source slot
 * \param param1  Type-specific parameter 1
 * \param param2  Type-specific parameter 2
 *
 * Consult the Barrelfish Kernel API Specification for the meaning of the
 * type-specific parameters.
 */
static inline errval_t
cap_mint(struct capref dest, struct capref src, uint64_t param1, uint64_t param2)
{
    capaddr_t dcs_addr = get_croot_addr(dest);
    capaddr_t dcn_addr = get_cnode_addr(dest);
    uint8_t dcn_level  = get_cnode_level(dest);
    capaddr_t scp_root = get_croot_addr(src);
    capaddr_t scp_addr = get_cap_addr(src);
    uint8_t scp_level  = get_cap_level(src);

    return invoke_cnode_mint(cap_root, dcs_addr, dcn_addr, dest.slot,
                             scp_root, scp_addr, dcn_level, scp_level,
                             param1, param2);
}

/**
 * \brief Perform mapping operation in kernel by minting a cap to a VNode
 *
 * \param dest destination VNode cap
 * \param src  source Frame cap
 * \param slot slot in destination VNode
 * \param attr Architecture-specific page (table) attributes
 * \param off Offset from source frame to map (must be page-aligned)
 */
static inline errval_t
vnode_map(struct capref dest, struct capref src, capaddr_t slot,
          uint64_t attr, uint64_t off, uint64_t pte_count,
          struct capref mapping)
{
    assert(get_croot_addr(dest) == CPTR_ROOTCN);

    capaddr_t sroot = get_croot_addr(src);
    capaddr_t saddr = get_cap_addr(src);
    uint8_t slevel  = get_cap_level(src);

    uint8_t mcn_level = get_cnode_level(mapping);
    capaddr_t mcn_addr = get_cnode_addr(mapping);
    capaddr_t mcn_root = get_croot_addr(mapping);

    return invoke_vnode_map(dest, slot, sroot, saddr, slevel, attr, off, pte_count,
                            mcn_root, mcn_addr, mcn_level, mapping.slot);
}

static inline errval_t vnode_unmap(struct capref pgtl, struct capref mapping)
{
    capaddr_t mapping_addr = get_cap_addr(mapping);
    uint8_t level = get_cap_level(mapping);

    return invoke_vnode_unmap(pgtl, mapping_addr, level);
}

static inline errval_t vnode_modify_flags(struct capref pgtl,
        size_t entry, size_t num_pages, uint64_t attr)
{
    return invoke_vnode_modify_flags(pgtl, entry, num_pages, attr);
}

/**
 * \brief Copy a capability between slots in CSpace
 *
 * \param dest    Location of destination slot, which must be empty
 * \param src     Location of source capability
 */
static inline errval_t cap_copy(struct capref dest, struct capref src)
{
    errval_t err;
    capaddr_t dcs_addr = get_croot_addr(dest);
    capaddr_t dcn_addr = get_cnode_addr(dest);
    capaddr_t scp_root = get_croot_addr(src);
    capaddr_t scp_addr = get_cap_addr(src);
    uint8_t dcn_level  = get_cnode_level(dest);
    uint8_t scp_level  = get_cap_level(src);

    err = invoke_cnode_copy(cap_root, dcs_addr, dcn_addr, dest.slot, scp_root,
                            scp_addr, dcn_level, scp_level);
    return err;
}

static inline errval_t cap_get_state(struct capref cap, distcap_state_t *state)
{
    capaddr_t caddr = get_cap_addr(cap);
    uint8_t level = get_cap_level(cap);

    return invoke_cnode_get_state(cap_root, caddr, level, state);
}

__END_DECLS

/**
 * \brief Identify a frame. This wraps the invocation so we can handle the
 *        case where the Frame cap is not invokable.
 * \param cap the capability to identify
 * \param ret A pointer to a `struct frame_identify` to fill in
 */
static inline errval_t frame_identify(struct capref frame, struct frame_identity *ret)
{
    errval_t err, err2;
    struct capref invokable = frame;

    if (get_croot_addr(invokable) != CPTR_ROOTCN) {
        err = slot_alloc(&invokable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_SLOT_ALLOC);
        }
        err = cap_copy(invokable, frame);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CAP_COPY);
        }
    }

    err = invoke_frame_identify(invokable, ret);

    if (!capcmp(invokable, frame)) {
        // made copy earlier, cleanup
        err2 = cap_destroy(invokable);
        assert(err_is_ok(err2));
    }

    return err;
}

#endif //INCLUDEBARRELFISH_CAPABILITIES_H
