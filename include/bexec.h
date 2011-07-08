/**
 * \file
 * \brief Beehive executable image format
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BEXEC_H
#define BEXEC_H

#define BEXEC_IMAGE_SIZE(b) (((b)->bborg + (b)->bbsize - (b)->btorg)<<2)
#define BEXEC_IMAGE_BASE(b) ((b)->btorg)

typedef struct bexec {
    unsigned bmagic, btorg, btsize, bdorg, bdsize, bborg, bbsize, bcksum;
} bexec_t, *pbexec_t;

#define BEXEC_BMAGIC (0xF800220CU)
#define BEXEC_ARGSMAGIC (0x53475241U)

// XXX: contents of "image pointer" file that appears in the VFS
struct mem_region;
struct bimgref_file {
    uint32_t magic;
    struct mem_region *region;
};
#define BIMGREF_MAGIC 0x42494d47u

#endif
