#ifndef LIBBF_PMAP_LL_H
#define LIBBF_PMAP_LL_H

#define INIT_SLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, sizeof(struct vnode))

struct pmap_vnode_mgmt {
    errval_t (*refill_slabs)(struct pmap *, size_t count); ///< Function to refill slabs
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
};

#endif // LIBBF_PMAP_LL_H
