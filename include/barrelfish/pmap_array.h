#ifndef LIBBF_PMAP_ARRAY_H
#define LIBBF_PMAP_ARRAY_H

#define INIT_SLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, sizeof(struct vnode))
#define PTSLAB_SLABSIZE (sizeof(struct vnode *)*PTABLE_ENTRIES)
#define INIT_PTSLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, PTSLAB_SLABSIZE)

errval_t refill_vnode_slabs(struct pmap *pmap, size_t count);
errval_t refill_pt_slabs(struct pmap *pmap, size_t count);

struct pmap_vnode_mgmt {
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct slab_allocator ptslab;   ///< Slab allocator for the page table children arrays
    errval_t (*refill_slabs)(struct pmap *, size_t count); ///< Function to refill slabs
    errval_t (*refill_ptslab)(struct pmap *, size_t count); ///< Function to refill slabs
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
};

#endif // LIBBF_PMAP_ARRAY_H
