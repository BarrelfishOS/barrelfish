/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitätestrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>
#include <collections/list.h>

#include "e10k_vf_resources.h"

static collections_listnode* list;
static bool init_done = false;

struct vf_resources {
    struct capref regs;
    struct capref irq;
    struct capref devid;
};

static void release_function(void *data) 
{
    // TODO destroy caps?
    free(data);  
}

/* Dump bytes of memory region to stdout */
void add_vf_resources(struct capref devid, struct capref regs, struct capref irq)
{
    if (!init_done) {
        collections_list_create(&list, release_function);
        init_done = true;
    }

    struct vf_resources* vf = calloc(1, sizeof(struct vf_resources));
    vf->regs = regs;
    vf->devid = devid;
    vf->irq = irq;
    
    collections_list_insert(list, vf); ; 
}

bool get_vf_resources(uint8_t vf, struct capref* devid, struct capref* regs, 
                      struct capref* irq)
{
    if (!init_done) {
        return false;
    }

    struct vf_resources* vfr = (struct vf_resources*) 
                               collections_list_get_ith_item(list, vf);
    if (vfr) {
        debug_printf("Found VF resources \n");
        *regs = vfr->regs;   
        *devid = vfr->devid;   
        *irq = vfr->irq;      

        assert(!capcmp(*regs, NULL_CAP));
        assert(!capcmp(*irq, NULL_CAP));
        assert(!capcmp(*devid, NULL_CAP));   

        return true;
    } 

    return false;
}

uint32_t num_vfs(void)
{
    if (!init_done) {
        return 0;
    }

    return collections_list_size(list);
}

