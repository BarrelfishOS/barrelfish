/**
 * \file
 * \brief Print pmap helper
 */

/*
 * Copyright (c) 2012, 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <barrelfish/vregion.h>
#include "vspace_dump.h"
#include <stdio.h>
#include <string.h>

#ifdef TARGET_X86_64_BARRELFISH_PMAP_H
static int cmp_dump_info(const void *arg1, const void *arg2)
{
    struct pmap_dump_info *info1, *info2;
    info1 = (struct pmap_dump_info *)arg1;
    info2 = (struct pmap_dump_info *)arg2;

    if (info1->pml4_index < info2->pml4_index)
        return -1;
    if (info1->pml4_index > info2->pml4_index)
        return 1;

    // pml indices equal

    if (info1->pdpt_index < info2->pdpt_index)
        return -1;
    if (info1->pdpt_index > info2->pdpt_index)
        return 1;

    // pdpt indices equal

    if (info1->pdir_index < info2->pdir_index)
        return -1;
    if (info1->pdir_index > info2->pdir_index)
        return 1;

    // pdir indices equal

    if (info1->pt_index < info2->pt_index)
        return -1;
    if (info1->pt_index > info2->pt_index)
        return 1;

    // pt indices equal
    return 0;
}
#elif defined(TARGET_X86_32_BARRELFISH_PMAP_H)
static int cmp_dump_info(const void *arg1, const void *arg2)
{
    struct pmap_dump_info *info1, *info2;
    info1 = (struct pmap_dump_info *)arg1;
    info2 = (struct pmap_dump_info *)arg2;

#if CONFIG_PAE
    if (info1->pdpt_index < info2->pdpt_index)
        return -1;
    if (info1->pdpt_index > info2->pdpt_index)
        return 1;

    // pdpt indices equal
#endif

    if (info1->pdir_index < info2->pdir_index)
        return -1;
    if (info1->pdir_index > info2->pdir_index)
        return 1;

    // pdir indices equal

    if (info1->pt_index < info2->pt_index)
        return -1;
    if (info1->pt_index > info2->pt_index)
        return 1;

    // pt indices equal
    return 0;
}
#else
static int cmp_dump_info(const void *arg1, const void *arg2)
{
	return 0;
}
#endif

#define BUFSIZE 8192
void dump_pmap(struct pmap *pmap)
{
    struct pmap_dump_info *buf = calloc(BUFSIZE, sizeof(struct pmap_dump_info));
    size_t items_written;

    pmap->f.dump(pmap, buf, BUFSIZE, &items_written);

    printf("items_written=%zd\n", items_written);

    qsort(buf, items_written, sizeof(struct pmap_dump_info), cmp_dump_info);

    for (size_t i = 0; i < items_written; i++) {
        struct pmap_dump_info *info = buf+i;
        struct frame_identity fi;
        invoke_frame_identify(info->cap, &fi);
        printf(PRIfmtPTIDX": 0x%"PRIxGENPADDR", 0x%"PRIxGENVADDR", 0x%zx\n",
                    GET_PTIDX(info),
                    fi.base, info->offset, fi.bytes);
    }
    printf("\n");

}
