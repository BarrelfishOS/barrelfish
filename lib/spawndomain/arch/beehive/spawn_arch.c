/**
 * \file
 * \brief functionality to spawn domains
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <barrelfish/dispatcher_arch.h>
#include <bexec.h>
#include "spawn.h"
#include "../../arch.h"

#if 0
static unsigned int ImageSum(const unsigned int *ptr, size_t count)
{
    unsigned int val = 0;
    do
        val += *(ptr++);
    while(--count > 0);
    return val ^ ~((val << 1) | (val >> 31));
}
#endif

errval_t spawn_arch_load(struct spawninfo *si,
                         lvaddr_t binary, size_t binary_size,
                         genvaddr_t *entry, void **arch_load_info)
{
    // XXX: is this a real bexec image, or a reference to one elsewhere?
    if (binary_size == sizeof(struct bimgref_file)) {
        // XXX: cheat the cap system and grab the binary straight out of memory
        struct bimgref_file *bimgref = (void *)binary;
        assert(bimgref->magic == BIMGREF_MAGIC);
        struct mem_region *region = bimgref->region;
        binary = region->mr_base;
        debug_printf("spawn_arch_load: fixing up fake binary to %"PRIxLVADDR"\n",
                     binary);
    }

    // Look at the bexec header and pull out the entry point.
    // Don't need to relocate the binary - but we may want to record
    // that the module is in use to prevent two domains sharing the
    // same data segment!
    bexec_t *bexec = (bexec_t *)binary;
    assert(bexec->bmagic == BEXEC_BMAGIC);
    assert(bexec->btorg != 0);

    *entry = bexec->btorg;

    // XXX All executables currently checksum themselves
    //    unsigned int checksum = ImageSum((void*)bexec, bexec->btsize + bexec->bdsize);
    //assert(checksum == 0);

    // TLS is NYI
    si->tls_init_base = 0;
    si->tls_init_len = si->tls_total_len = 0;

    return SYS_ERR_OK;
}

void spawn_arch_set_registers(void *arch_load_info,
                              dispatcher_handle_t handle,
                              arch_registers_state_t *enabled_area,
                              arch_registers_state_t *disabled_area)
{
}
