/**
 * \file
 * \brief pmap management
 * \buf Not implemented
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <stdio.h>
#include <inttypes.h>
#include <dcache.h>

#define PMAP_DEBUG if (0) debug_printf
 
/**
 * \brief Create page mappings
 *
 * \param pmap     The pmap object
 * \param vaddr    The virtual address to create the mapping for
 * \param frame    The frame cap to map in
 * \param offset   Offset into the frame cap
 * \param size     Size of the mapping
 * \param flags    Flags for the mapping
 * \param retoff   If non-NULL, filled in with adjusted offset of mapped region
 * \param retsize  If non-NULL, filled in with adjusted size of mapped region
 */
static errval_t map(struct pmap *pmap, genvaddr_t vaddr, struct capref frame,
                    size_t offset, size_t size, vregion_flags_t flags,
                    size_t *retoff, size_t *retsize)
{
    PMAP_DEBUG("pmap_beehive.c map: vaddr=%" PRIxGENVADDR
               " offset=%lu size=%lu flags=%" PRIuVREGIONFLAGS "\n",
               vaddr, offset, size, flags);

    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err))
	return err;

    PMAP_DEBUG("pmap_beehive.c map: id.base=%" PRIxGENPADDR
               " id.bits=%u (0x%zx)\n", 
               id.base, id.bits, ((size_t)1) << id.bits);
    
    if (offset != 0 || size != (1<<id.bits) || id.base != vaddr) {
        PMAP_DEBUG("BAD map() call: offset %x size %x base %"
                   PRIxGENPADDR" vaddr %"PRIxGENVADDR"\n",
                   offset, size, id.base, vaddr);
	return LIB_ERR_NOT_IMPLEMENTED;
    }

    if (retoff != NULL) *retoff = offset;
    if (retsize != NULL) *retsize = size;
    return SYS_ERR_OK;
}


/**
 * \brief Remove page mappings
 *
 * \param pmap     The pmap object
 * \param vaddr    The start of the virtual addres to remove
 * \param size     The size of virtual address to remove
 * \param retsize  If non-NULL, filled in with the actual size removed
 */
static errval_t unmap(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                      size_t *retsize)
{
    // XXX: This will flush the entire cache to memory
    bee_dcache_flush_all();

    if (retsize) {
        *retsize = size;
    }
    return SYS_ERR_OK;
}

/**
 * \brief Determine a suitable address for a given memory object
 *
 * \param pmap    The pmap object
 * \param memobj  The memory object to determine the address for
 * \param alignment Minimum alignment
 * \param vaddr   Pointer to return the determined address
 *
 * Relies on vspace.c code maintaining an ordered list of vregions
 */
static errval_t determine_addr(struct pmap *pmap, struct memobj *memobj,
                               size_t alignment, genvaddr_t *vaddr)
{
    struct frame_identity id = {0, 0};
    errval_t err;

    assert(memobj->type == ONE_FRAME ||
           memobj->type == ONE_FRAME_ONE_MAP);

    assert(alignment == 0); // XXX: unsupported

    if (memobj->type == ONE_FRAME) {
        struct memobj_one_frame *mof;
        mof = (struct memobj_one_frame*)memobj;
        err = invoke_frame_identify(mof->frame, &id);
    } else {
        struct memobj_one_frame_one_map *mofom;
        mofom = (struct memobj_one_frame_one_map*)memobj;
        err = invoke_frame_identify(mofom->frame, &id);
    } 
    if (err_is_fail(err)) {
        return err;
    }

    *vaddr = id.base;

    return SYS_ERR_OK;
}

/**
 * \brief Modify page mapping
 *
 * \param pmap     The pmap object
 * \param vaddr    The virtual address to unmap
 * \param flags    New flags for the mapping
 * \param retsize  If non-NULL, filled in with the actual size modified
 */
static errval_t modify_flags(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                             vregion_flags_t flags, size_t *retsize)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

/**
 * \brief Query existing page mapping
 *
 * \param pmap     The pmap object
 * \param vaddr    The virtual address to query
 * \param retvaddr Returns the base virtual address of the mapping
 * \param retsize  Returns the actual size of the mapping
 * \param retcap   Returns the cap mapped at this address
 * \param retoffset Returns the offset within the cap that is mapped
 * \param retflags Returns the flags for this mapping
 *
 * All of the ret parameters are optional.
 */
static errval_t lookup(struct pmap *pmap, genvaddr_t vaddr,
                       genvaddr_t *retvaddr, size_t *retsize,
                       struct capref *retcap, genvaddr_t *retoffset,
                       vregion_flags_t *retflags)
{
    USER_PANIC("NYI");
    return 0;
}


static errval_t serialise(struct pmap *pmap, void *buf, size_t buflen)
{
    // Unimplemented: ignored
    return SYS_ERR_OK;
}

static errval_t deserialise(struct pmap *pmap, void *buf, size_t buflen)
{
    // Unimplemented: noop for beehive
    return SYS_ERR_OK;
}

static struct pmap_funcs pmap_funcs = {
    .determine_addr = determine_addr,
    .map = map,
    .unmap = unmap,
    .modify_flags = modify_flags,
    .lookup = lookup,
    .serialise = serialise,
    .deserialise = deserialise,
};

/**
 * \brief Initialize the pmap object
 */
errval_t pmap_init(struct pmap *pmap, struct vspace *vspace,
                   struct capref vnode, struct slot_allocator *opt_slot_alloc)
{
    /* Generic portion */
    pmap->f = pmap_funcs;
    pmap->vspace = vspace;

    /* Will need some architecture specific code here */

    return SYS_ERR_OK;
}

errval_t pmap_current_init(bool init_domain)
{
    return SYS_ERR_OK;
}
