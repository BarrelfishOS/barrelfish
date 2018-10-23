#ifndef LIBBF_PMAP_LL_H
#define LIBBF_PMAP_LL_H

#define INIT_SLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, sizeof(struct vnode))

errval_t refill_vnode_slabs(struct pmap *pmap, size_t count);

struct pmap_vnode_mgmt {
    errval_t (*refill_slabs)(struct pmap *, size_t count); ///< Function to refill slabs
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
};

#endif // LIBBF_PMAP_LL_H
