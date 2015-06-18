/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <exceptions.h>
#include <stdio.h>

void handle_irq(void)
{
    printf("IRQ handler\n");
    return;
}

void page_fault(void *exn_frame)
{
    printf("PF handler: %p\n", exn_frame);
    return;
}

void handle_sync_abort(uint64_t esr)
{
    printf("Sync Abort: %"PRIx64"\n", esr);
    return;
}
