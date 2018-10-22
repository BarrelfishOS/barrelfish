/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/mman.h>
#include <barrelfish/barrelfish.h>
#include <sys/errno.h>

// Copied from usr/monitor/capops/internal.h
#define GOTO_IF_ERR(err, label) do { \
    if (err_is_fail(err)) { \
        DEBUG_ERR(err, "%s:%u -> goto err", __FUNCTION__, __LINE__); \
        goto label; \
    } \
} while (0)

static vregion_flags_t prot_to_vregion_flags(int flags)
{
    vregion_flags_t f = VREGION_FLAGS_NONE;
    if (flags & PROT_READ) {
        f |= VREGION_FLAGS_READ;
    }
    if (flags & PROT_WRITE) {
        f |= VREGION_FLAGS_WRITE;
    }
    if (flags & PROT_EXEC) {
        f |= VREGION_FLAGS_EXECUTE;
    }
    return f;
}

static const char *flag_to_str(int flag)
{
    switch(flag) {
#ifdef __x86_64__
        case MAP_32BIT        : return "MAP_32BIT";
#endif
        case MAP_ALIGNED_SUPER: return "MAP_ALIGNED_SUPER";
        case MAP_ANON         : return "MAP_ANON|MAP_ANONYMOUS";
        case MAP_EXCL         : return "MAP_EXCL";
        case MAP_FIXED        : return "MAP_FIXED";
        case MAP_HASSEMAPHORE : return "MAP_HASSEMAPHORE";
        case MAP_NOCORE       : return "MAP_NOCORE";
        case MAP_NOSYNC       : return "MAP_NOSYNC";
        case MAP_PREFAULT_READ: return "MAP_PREFAULT_READ";
        case MAP_PRIVATE      : return "MAP_PRIVATE";
        case MAP_SHARED       : return "MAP_SHARED";
        case MAP_STACK        : return "MAP_STACK";
        default: return "Unknown Flag";
    }
}

// If you implement one of the following flags, update this list to avoid
// unnecessary warnings! -SG,2017-07-28
static int NYI_FLAGS = MAP_HASSEMAPHORE | MAP_NOCORE | MAP_NOSYNC |
                       MAP_PREFAULT_READ | MAP_PRIVATE | MAP_SHARED | MAP_STACK;

static inline bool flag_is_set(int flags, int flag)
{
    return flags & flag;
}

static inline bool is_anon_mapping(int flags)
{
    return flag_is_set(flags, MAP_ANONYMOUS) || flag_is_set(flags, MAP_ANON);
}

static inline bool is_fixed_mapping(int flags)
{
    return flag_is_set(flags, MAP_FIXED);
}

void * mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    // Hard-fail on non anonymous mappings for now
    if (!is_anon_mapping(flags)) {
        debug_printf("mmap() without MAP_ANON NYI!\n");
        *__error() = EBADF;
        return MAP_FAILED;
    }

#ifdef CONFIG_PRINT_MMAP_NYI_FLAGS
    // Warn user about NYI flags
    if (flags & NYI_FLAGS) {
        debug_printf("mmap(): user provided NYI flags, ignoring the following:\n");
        for (int i = 0; i < 32; i++) {
            if (NYI_FLAGS & (1 << i)) {
                debug_printf("     * %s\n", flag_to_str(1<<i));
            }
        }
    }
#endif // CONFIG_PRINT_MMAP_NYI_FLAGS

    // According to manpage, for anonymous mappings mmap() MUST be called with
    // fd = -1 and offset = 0.
    if (is_anon_mapping(flags) && (fd != -1 || offset != 0)) {
        *__error() = EINVAL;
        return MAP_FAILED;
    }

    genvaddr_t vaddr = (genvaddr_t)(lvaddr_t) addr;

    if (flag_is_set(flags, MAP_FIXED) && !flag_is_set(flags, MAP_EXCL)) {
        // We currently do not support MAP_FIXED without MAP_EXCL, as we do
        // not have a clean way of replacing existing mapped pages.
        debug_printf("mmap(): MAP_FIXED without MAP_EXCL NYI!\n");
        *__error() = EINVAL;
        return MAP_FAILED;
    }

    if (len == 0) {
        *__error() = EINVAL;
        return MAP_FAILED;
    }

    errval_t err;
    // Translate prot to vregion flags
    vregion_flags_t vflags = prot_to_vregion_flags(prot);

    // Set large page flag, if MAP_ALIGNED_SUPER is set
    if (flag_is_set(flags, MAP_ALIGNED_SUPER)) {
        vflags |= VREGION_FLAGS_LARGE;
    }

    // Returned from vspace_map* call
    struct vregion *vregion;
    struct memobj *memobj;

    if (is_fixed_mapping(flags)) {
        // According to manpage, calls with MAP_FIXED must have aligned base
        // address.
        if (vaddr & BASE_PAGE_MASK) {
            *__error() = EINVAL;
            return MAP_FAILED;
        }

        err = vspace_map_anon_fixed(vaddr, len, vflags, &vregion, &memobj);
#ifdef __x86_64__
    } else if (is_anon_mapping(flags) && !flag_is_set(flags, MAP_32BIT)) {
        // For anonymous mappings without constraints we let libbarrelfish
        // find a suitable region of virtual address space.
        err = vspace_map_anon_attr(&addr, &memobj, &vregion, len, NULL, vflags);
#endif
    } else {
        // Fail on other combinations of flags for now
        debug_printf("mmap(): mapping with given set of flags NYI!\n");
        *__error() = EBADF;
        return MAP_FAILED;
    }
    if (err_is_fail(err)) {
        *__error() = ENOMEM;
        return MAP_FAILED;
    }

    // Contrary to mmap() we do not support lazy page allocation on Barrelfish
    // at the moment, and rather than requiring all mmap() calls to explicitly
    // specify MAP_PREFAULT_READ we allocate RAM here and prefault the new
    // mapping like all other libbarrelfish high-level functions.
    // -SG,2017-07-28

    size_t retbytes;
    struct capref frame;
    err = frame_alloc(&frame, len, &retbytes);
    GOTO_IF_ERR(err, cleanup);
    assert(len <= retbytes);

    // Attach Frame to memobj
    err = memobj->f.fill(memobj, 0, frame, len);
    GOTO_IF_ERR(err, cleanup);

    // Prefault
    err = memobj->f.pagefault(memobj, vregion, 0, 0);
    GOTO_IF_ERR(err, cleanup);

    return (void *)vregion_get_base_addr(vregion);

cleanup:
    *__error() = ENOMEM;
    vregion_destroy(vregion);
    memobj_destroy_anon(memobj, true);
    return MAP_FAILED;
}

int munmap(void *addr, size_t len)
{
    genvaddr_t vaddr = (genvaddr_t)addr;
    // Catch unaligned base addr or invalid length
    if ((vaddr & BASE_PAGE_MASK) || len <= 0) {
        *__error() = EINVAL;
        return -1;
    }

    // Get vregion containing the region to unmap
    struct vregion *vr = vspace_get_region(get_current_vspace(), addr);

    if (vregion_get_base_addr(vr) != vaddr || vregion_get_size(vr) != len) {
        // As Barrelfish does not cleanly support unmapping parts of an existing
        // vregion, we fail on such requests. -SG,2017-07-28.
        debug_printf("munmap(): user requested unmapping of part of existing vregion, NYI on Barrelfish\n");
        *__error() = EINVAL;
        return -1;
    }

    errval_t err = vregion_destroy(vr);
    if (err_is_fail(err)) {
        *__error() = EINVAL;
        return -1;
    }

    return 0;
}
