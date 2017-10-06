/**
 * \file
 * \brief Local memory allocator for init till mem_serv is ready to use
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "init.h"
#include <mm/mm.h>

/* parameters for local memory allocator used until we spawn mem_serv */
#define OBJBITS_DISPATCHER (10)
// XXX: 16MB is not enough to create tracing buffers on babybel for all cores
#define MM_REQUIREDBITS    27          ///< Required size of memory to boot (128MB)
#define MM_REQUIREDBYTES   (1UL << MM_REQUIREDBITS)
#define MM_MAXSIZEBITS     (MM_REQUIREDBITS + 3) ///< Max size of memory in allocator
#define MM_MINSIZEBITS     BASE_PAGE_BITS ///< Min size of allocation
#define MM_MAXCHILDBITS    1           ///< Max branching factor of BTree nodes
#define MM_MAXDEPTH (MM_MAXSIZEBITS - MM_MINSIZEBITS + 1)   ///< BTree depth
#define MM_NNODES   ((1UL << MM_MAXDEPTH) + MM_MINSIZEBITS - OBJBITS_DISPATCHER) ///< Max BTree nodes
#define MM_NCNODES  DIVIDE_ROUND_UP(MM_NNODES, 1UL << DEFAULT_CNODE_BITS) //CNodes

// Number of slots placed in smallcn of mem_serv
#define MEM_SERV_SMALLCN_SLOTS 10

/// MM allocator instance data
static struct mm mymm;

static errval_t mymm_alloc(struct capref *ret, uint8_t bits, uint64_t minbase,
                           uint64_t maxlimit)
{
    /* XXX: although we have calculated the space requirements for
     * MM_MINSIZEBITS, we only ever allocate a single dispatcher from this
     * allocator, so this should be safe */
    assert(bits >= OBJBITS_DISPATCHER);
    errval_t err = mm_alloc(&mymm, bits, ret, NULL);
    return err;
}

/**
 * \brief Setups a local memory allocator for init to use till the memory server
 * is ready to be used.
 */
errval_t initialize_ram_alloc(void)
{
    errval_t err;

    /* init slot allocator */
    static struct slot_alloc_basecn init_slot_alloc;
    err = slot_alloc_basecn_init(&init_slot_alloc);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_SLOT_ALLOC_INIT);
    }

    /* walk bootinfo looking for suitable RAM cap to use
     * we pick the first cap equal to MM_REQUIREDBITS,
     * or else the next closest less than MM_MAXSIZEBITS */
    int mem_slot = 0;
    struct capref mem_cap = {
        .cnode = cnode_super,
        .slot = 0,
    };

    /* get destination slot for retype */
    genpaddr_t region_base = 0;
    struct capref region_for_init;
    err = slot_alloc_basecn(&init_slot_alloc, 1, &region_for_init);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc_basecn in initialize_ram_alloc");
        return err_push(err, MM_ERR_SLOT_NOSLOTS);
    }

    assert(bi != NULL);
    for (int i = 0; i < bi->regions_length; i++) {
        assert(!bi->regions[i].mr_consumed);
        if (bi->regions[i].mr_type == RegionType_Empty) {
            if (bi->regions[i].mr_bytes >= MM_REQUIREDBYTES) {
                mem_cap.slot = mem_slot;
                if (bi->regions[i].mr_bytes == MM_REQUIREDBYTES) {
                    bi->regions[i].mr_consumed = true;
                    break;
                }

                /* found cap bigger than required; cut off end */
                bi->regions[i].mr_bytes -= MM_REQUIREDBYTES;
                // can use mr_bytes as offset here
                err = cap_retype(region_for_init, mem_cap,
                                 bi->regions[i].mr_bytes, ObjType_RAM,
                                 MM_REQUIREDBYTES, 1);
                if (err_is_fail(err)) {
                    return err_push(err, MM_ERR_CHUNK_NODE);
                }
                mem_cap = region_for_init;
                region_base = bi->regions[i].mr_base + bi->regions[i].mr_bytes;
                break;
            }
            mem_slot++;
        }
    }

    if (region_base == 0) {
        printf("Error: no RAM capability >= %zu MB found", MM_REQUIREDBYTES / 1024 / 1024);
    }

    /*  init MM allocator */
    err = mm_init(&mymm, ObjType_RAM, region_base,
                  MM_REQUIREDBITS, MM_MAXCHILDBITS, NULL,
                  slot_alloc_basecn, NULL, &init_slot_alloc, true);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    /* give MM allocator enough static storage for its node allocator */
    assert(1UL << OBJBITS_DISPATCHER == OBJSIZE_DISPATCHER);
    static char nodebuf[SLAB_STATIC_SIZE(MM_NNODES, MM_NODE_SIZE(MM_MAXCHILDBITS))];
    slab_grow(&mymm.slabs, nodebuf, sizeof(nodebuf));

    /* add single RAM cap to allocator */
    /* XXX: can't use mm_add_multi here, as the allocator tends to choke when
     * we add smaller regions before larger */
    debug_printf("using %#"PRIxGENPADDR", %zu MB for init's allocator\n",
            region_base, MM_REQUIREDBYTES / 1024 / 1024);
    err = mm_add(&mymm, mem_cap, MM_REQUIREDBITS, region_base);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_ADD);
    }

    // initialise generic RAM allocator to use local allocator
    err = ram_alloc_set(mymm_alloc);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    return SYS_ERR_OK;
}
