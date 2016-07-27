/**
 * \file
 * \brief Root cnode resizing test
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>

#define STEP 256

//{{{1 main
int main(void)
{
    errval_t err;
    struct capref cap;
    for (cslot_t i = 0; i < 4*L2_CNODE_SLOTS*L2_CNODE_SLOTS; i++) {
        err = slot_alloc(&cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "slot_alloc");
            printf("test_rootcn_resize: slot alloc %"PRIuCSLOT" failed\n", i);
            return 1;
        }
        if (i % STEP == 0) {
            err = cap_copy(cap, cap_vroot);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "cap_copy");
                printf("test_rootcn_resize: copying cpa to slot %"PRIuCSLOT" failed\n", i);
            }
        }
    }
    printf("test_rootcn_resize: passed\n");
    return 0;
}
