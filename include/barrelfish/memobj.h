/**
 * \file
 * \brief Memobj definitions
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_MEMOBJ_H
#define LIBBARRELFISH_MEMOBJ_H

#include <barrelfish/slab.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

// FIXME: these enum names need to be scoped (e.g. MEMOBJ_X rather than X) -AB
enum memobj_type {
    ANONYMOUS,
    ONE_FRAME,
    ONE_FRAME_LAZY,
    PINNED,
    ONE_FRAME_ONE_MAP,
    MEMOBJ_VFS, // see lib/vfs/mmap.c
    MEMOBJ_FIXED,
    MEMOBJ_NUMA
};

typedef uint32_t memobj_flags_t;
typedef uint32_t vs_prot_flags_t;

struct memobj;
struct vregion;
struct memobj_funcs {
    errval_t (*map_region)(struct memobj *memobj, struct vregion *vregion);
    errval_t (*unmap_region)(struct memobj* memobj, struct vregion* region);
    errval_t (*protect)(struct memobj* memobj, struct vregion* region,
                        genvaddr_t offset, size_t range, vs_prot_flags_t flags);
    errval_t (*pin)(struct memobj* memobj, struct vregion* region,
                    genvaddr_t offset, size_t range);
    errval_t (*unpin)(struct memobj* memobj, struct vregion* region,
                      genvaddr_t offset, size_t range);
    errval_t (*fill)(struct memobj *memobj, genvaddr_t offset, struct capref frame,
                     size_t size);
    errval_t (*fill_foff)(struct memobj *memobj, genvaddr_t offset, struct capref frame,
                     size_t size, genpaddr_t foffset);
    errval_t (*unfill)(struct memobj *memobj, genvaddr_t offset,
                       struct capref *ret_frame, genvaddr_t *ret_offset);
    errval_t (*pagefault)(struct memobj* memobj, struct vregion* region,
                          genvaddr_t offset, vm_fault_type_t type);
    errval_t (*pager_free)(struct memobj* memobj, size_t size,
                           struct capref *frames, size_t num_frames);
};

struct vregion_list {
    struct vregion *region;
    struct vregion_list *next;
};

/// Public interface for memobj
struct memobj {
    size_t size;              ///< Size of the object
    memobj_flags_t flags;     ///< Flags for the object. NYI.
    enum memobj_type type;    ///< Type of the memory object
    struct memobj_funcs f;    ///< Function pointers
};

struct memobj_pinned {
    struct memobj m;          ///< Public interface
    struct vregion *vregion;  ///< Pointer to the single vregion
};

struct memobj_one_frame_one_map {
    struct memobj m;          ///< Public interface
    struct vregion *vregion;  ///< Pointer to the single vregion
    struct capref frame;      ///< Frame tracked by the obj
    genpaddr_t offset;        ///< Offset into the frame
};

struct memobj_one_frame {
    struct memobj m;
    struct vregion_list *vregion_list;    ///< List of vregions mapped into the obj
    struct capref frame;                  ///< Frame tracked by the obj
    genpaddr_t offset;                    ///< Offset into the frame
};

struct memobj_one_frame_lazy {
    struct memobj m;
    struct vregion_list *vregion_list;    ///< List of vregions mapped into the obj
    struct capref frame;                  ///< Frame tracked by the obj
    size_t chunk_size;                    ///< Amount to map in per pagefault
};

struct memobj_frame_list {
    genpaddr_t offset;              ///< Offset into the frame
    struct capref frame;            ///< Capability of the frame
    size_t size;                    ///< Size of the frame
    genpaddr_t pa;                  ///< XXX: physical address of frame
    genpaddr_t foffset;             ///< Offset into frame
    struct memobj_frame_list *next;
};

struct memobj_anon {
    struct memobj m;
    struct vregion_list *vregion_list;    ///< List of vregions mapped into the obj
    struct slab_allocator vregion_slab;       ///< Slab to back the vregion list
    struct memobj_frame_list *frame_list; ///< List of frames tracked by the obj
    struct slab_allocator frame_slab;         ///< Slab to back the frame list
    bool frame_slab_refilling;      ///< True, iff we're currently refilling `frame_slab`
    bool vregion_slab_refilling;    ///< True, iff we're currently refilling `vregion_slab`
};

/**
 * this memobj can be mapped into a single vregion and backed by a fixed number
 * of equal sized frames
 */
struct memobj_fixed {
    struct memobj    m;          ///< public memobj interface
    size_t           count;      ///< the number of frames
    size_t           chunk_size; ///< the size of the frames
    struct vregion  *vregion;    ///< the associated vregion
    struct capref   *frames;     ///< the tracked frames
    size_t        *offsets;    ///< the offset into the tracked frames
};

/**
 * this memobj can be mapped into a single vregion and is backed by a s
 * this memobj can be mapped into a single vregion and backed by a fixed number
 * of equal sized frames
 */
struct memobj_numa {
    struct memobj    m;          ///< public memobj interface
    uint32_t         node_count; ///< number of nodes in the machine
    size_t           stride;     ///< size of the regions to map
    struct vregion  *vregion;    ///< the associated vregion
    struct capref   *frames;     ///< the tracked frames
};

errval_t memobj_create_pinned(struct memobj_pinned *memobj, size_t size,
                              memobj_flags_t flags);

errval_t memobj_create_anon(struct memobj_anon *memobj, size_t size,
                            memobj_flags_t flags);
errval_t memobj_destroy_anon(struct memobj *memobj, bool delete_caps);

errval_t memobj_create_one_frame(struct memobj_one_frame *memobj, size_t size,
                                 memobj_flags_t flags);
errval_t memobj_destroy_one_frame(struct memobj *memobj);

errval_t memobj_create_one_frame_lazy(struct memobj_one_frame_lazy *memobj,
                                      size_t size, memobj_flags_t flags,
                                      struct capref frame, size_t chunk_size);
errval_t memobj_create_one_frame_one_map(struct memobj_one_frame_one_map *memobj,
                                         size_t size, memobj_flags_t flags);


errval_t memobj_create_fixed(struct memobj_fixed *memobj, size_t size,
                             memobj_flags_t flags, size_t count,
                             size_t chunk_size);

errval_t memobj_destroy_fixed(struct memobj *memobj);

errval_t memobj_create_numa(struct memobj_numa *numa, size_t size,
                            memobj_flags_t flags, size_t node_count, size_t stride);

errval_t memobj_destroy_numa(struct memobj *memobj);

__END_DECLS

#endif // LIBBARRELFISH_MEMOBJ_H
