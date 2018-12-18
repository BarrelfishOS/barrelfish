
errval_t cow_init(size_t bufsize, size_t granularity,
        void *vaddr, struct vregion *vregion);
errval_t vspace_map_one_frame_cow(void **buf, size_t size,
        struct capref frame, vregion_flags_t flags,
        struct memobj **memobj, struct vregion **vregion,
        size_t granularity);
