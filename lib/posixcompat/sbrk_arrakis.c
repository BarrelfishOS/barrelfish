/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <posixcompat.h>

//#ifndef ARRAKIS
//#error should not be included in non-arrakis posixcompat!
//#endif

#include "nestedpaging.h"

// 512GB sbrk heap
#define SBRK_REGION_BYTES (PTABLE_SIZE * HUGE_PAGE_SIZE)

static void *base = NULL;
static size_t offset = 0; ///< How much is currently used
static size_t goffset = 0; ///< Maximum ever allocated

struct pml4_entry *pdpt = NULL;

union x86_64_pdir_entry *sbrk_get_pdpt(void)
{
    assert(pdpt != NULL);
    return pdpt->e.addr;
}

void* sbrk_get_base(void) 
{
    assert(base != NULL);
    return base;
}

size_t sbrk_get_offset(void) 
{
    assert(offset != 0);
    return offset;
}

static void initialize_sbrk(void)
{
    errval_t err;
    pdpt = malloc(sizeof(*pdpt));
    if (!pdpt) {
        USER_PANIC("could not alloc struct pml4_entry");
    }
    err = install_user_managed_pdpt(pdpt);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "installing pdpt");
    }
    // XXX: replace << 39 with proper macro
    base = (void *)(pdpt->e.entry << 39);
}

void *sbrk(intptr_t increment)
{
    errval_t err;
    size_t orig_offset;

    if (!pdpt) { // Initialize
        initialize_sbrk();
    }

    if (increment < 0) {
      if (-increment > offset) {
        USER_PANIC("sbrk() called with negative increment beyond offset");
      } else {
        orig_offset = offset;
        offset += increment;

        void *ret = base + orig_offset;
        return ret;
      }
    } else if (increment == 0) {
        return base + offset;
    } else if (offset + increment > SBRK_REGION_BYTES) {
        debug_printf("sbrk() exceeded static region limit of %zu bytes, offset: %zu\n",
                     (size_t)SBRK_REGION_BYTES, offset);
        return (void *)-1;
    } else if (offset + increment <= goffset) {
        orig_offset = offset;
        offset += increment;

        void *ret = base + orig_offset;
        return ret;
    }

    size_t inc_bytes = offset + increment - goffset;
    orig_offset = offset;

    struct capref frame;
    err = frame_alloc(&frame, inc_bytes, &inc_bytes);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "frame_alloc failed");
        return (void *)-1;
    }

    genvaddr_t next_map = ((genvaddr_t)base) + goffset;
    err = user_managed_map_frame(pdpt, next_map,
            frame, VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err,
                  "user_managed_map_frame failed");
        return (void *)-1;
    }
    goffset += inc_bytes;

    void *ret = base + orig_offset;
    return ret;
}
