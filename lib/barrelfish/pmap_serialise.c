/**
 * \file
 * \brief Pmap arch-independent code for serialisation
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pmap_ds.h>
#include <pmap_priv.h>

/*
 * The serialisation format is depressingly ad-hoc, and assumes a depth-first
 * walk of the tree. Each vnode is encoded as an entry in an array.
 *
 * We abuse the slot of the first entry, which is the root and thus is always
 * zero, to store the number of entries in the array.
 */
struct serial_entry {
    uint16_t entry;     ///< Entry #
    uint8_t depth;      ///< Depth of this node (0 = root)
    cslot_t slot;       ///< Slot number (in page cnode) of vnode cap
    cslot_t mapping;    ///< Slot number (in page cnode) of mapping cap for vnode
    enum objtype type;  ///< Type of the vnode cap
};

static errval_t serialise_tree(int depth, struct pmap *pmap,
                               struct vnode_public *v,
                               struct serial_entry *out,
                               size_t outlen, size_t *outpos)
{
    assert(v != NULL);
    errval_t err;

    // don't serialise leaf pages (yet!)
    if (!v->is_vnode) {
        return SYS_ERR_OK;
    }

    if (*outpos >= outlen) {
        return LIB_ERR_SERIALISE_BUFOVERFLOW;
    }

    // serialise this node
    out[(*outpos)++] = (struct serial_entry) {
        .depth = depth,
        .entry = v->entry,
        .type = v->type,
        .slot = v->cap.slot,
        .mapping = v->mapping.slot,
    };

    // TODO: we want a way to only ever see vnode_public part of vnode
    // depth-first walk
    struct vnode *c;
    pmap_foreach_child((struct vnode*)v, c) {
        if (c) {
            /* allocate slot in pagecn for mapping */
            struct capref mapping;
#ifdef PMAP_ARRAY
            /* here we rely on the fact that pmap_foreach_child uses `int i`
             * as its internal loop counter for PMAP_ARRAY. This is not very
             * clean, but may eliminate itself, if we move pmap serialization
             * to arch-independent code, which should be possible.
             */
            if (i == c->v.entry) {
                // only copy each mapping cap to page cn for the first page of
                // a multi-page mapping.
                err = pmap->slot_alloc->alloc(pmap->slot_alloc, &mapping);
                if (err_is_fail(err)) {
                    return err;
                }
                err = cap_copy(mapping, c->v.mapping);
                if (err_is_fail(err)) {
                    return err;
                }
                c->v.mapping = mapping;
            }
#else
            err = pmap->slot_alloc->alloc(pmap->slot_alloc, &mapping);
            if (err_is_fail(err)) {
                return err;
            }
            err = cap_copy(mapping, c->v.mapping);
            if (err_is_fail(err)) {
                return err;
            }
            c->v.mapping = mapping;
#endif
            err = serialise_tree(depth + 1, pmap, &c->v, out, outlen, outpos);
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Serialise vtree to a flat structure, for passing to another process
 *
 * This is used by spawn_vspace to communicate the vnode capabilities to the child.
 */
errval_t pmap_serialise(struct pmap *pmap, void *buf, size_t buflen)
{
    errval_t err;

    // XXX: check alignment of buffer
    assert((uintptr_t)buf % sizeof(uintptr_t) == 0);

    struct serial_entry *out = buf;
    size_t outlen = buflen / sizeof(struct serial_entry);
    size_t outpos = 0;

    err = serialise_tree(0, pmap, pmap_get_vroot(pmap), out, outlen, &outpos);
    if (err_is_ok(err)) {
        // store length in first entry's slot number
        assert(out[0].slot == 0);
        out[0].slot = outpos;
    }

    return err;
}

static errval_t deserialise_tree(struct pmap *pmap, struct serial_entry **in,
                                 size_t *inlen, int depth, struct vnode_public *parent)
{
    errval_t err;

    if (*inlen == 0) {
        return SYS_ERR_OK;
    }

    while (*inlen > 0 && (*in)->depth == depth) {
        // ensure slab allocator has sufficient space
        err = pmap_refill_slabs(pmap, 16);
        if (err_is_fail(err)) {
            return err;
        }

        // allocate storage for the new vnode
        struct vnode *n = slab_alloc(&pmap->m.slab);
        assert(n != NULL);

        // populate it and append to parent's list of children
        n->v.is_vnode              = true;
        n->v.entry                 = (*in)->entry;
        n->v.cap.cnode     = cnode_page;
        n->v.cap.slot      = (*in)->slot;
        n->v.u.vnode.invokable     = n->v.cap;
        pmap_vnode_init(pmap, n);
        pmap_vnode_insert_child((struct vnode *)parent, n);
        n->v.type = (*in)->type;

        /* XXX: figure out if we want this
        // Count cnode_page slots that are in use
        pmapx->used_cap_slots ++;
        */

#if GLOBAL_MCN
        /* allocate mapping cnodes */
        for (int i = 0; i < MCN_COUNT; i++) {
            err = cnode_create_l2(&n->u.vnode.mcn[i], &n->u.vnode.mcnode[i]);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_ALLOC_CNODE);
            }
        }
#endif

        set_mapping_cap(pmap, n, (struct vnode *)parent, n->v.entry);
        /* copy mapping cap into mapping cnode of parent */
        struct capref orig;
        orig.cnode = cnode_page;
        orig.slot  = (*in)->mapping;
        err = cap_copy(n->v.mapping, orig);
        if (err_is_fail(err)) {
            return err;
        }

        (*in)++;
        (*inlen)--;

        // is next entry a child of the last node?
        if (*inlen > 0 && (*in)->depth > depth) {
            assert((*in)->depth == depth + 1); // depth-first, no missing nodes
            err = deserialise_tree(pmap, in, inlen, depth + 1, &n->v);
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    assert((*in)->depth < depth);
    return SYS_ERR_OK;
}

/**
 * \brief Deserialise vtree from a flat structure, for importing from another process
 *
 * This is used in a newly-spawned child
 */
errval_t pmap_deserialise(struct pmap *pmap, void *buf, size_t buflen)
{
    errval_t err;

    // XXX: check alignment of buffer
    assert((uintptr_t)buf % sizeof(uintptr_t) == 0);

    // extract length and sanity-check
    struct serial_entry *in = buf;
    assert(buflen > sizeof(struct serial_entry));
    size_t inlen = in[0].slot;
    assert(inlen * sizeof(struct serial_entry) <= buflen);
    in++;
    inlen--;

    err = deserialise_tree(pmap, &in, &inlen, 1, pmap_get_vroot(pmap));
    if (err_is_ok(err)) {
        // XXX: now that we know where our vnodes are, we can support mappings
        // in the bottom of the address space. However, we still don't know
        // exactly where our text and data are mapped (because we don't yet
        // serialise vregions or memobjs), so instead we pad _end.
        extern char _end;
        pmap_set_min_mappable_va(pmap, ROUND_UP((lvaddr_t)&_end, 64 * 1024) + 64 * 1024);
    }

    return err;
}


