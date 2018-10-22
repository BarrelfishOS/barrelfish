/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/paging_target.h>
#include <barrelfish/sys_debug.h>

#include "nestedpaging.h"

int main(int argc, char *argv[])
{
    //const char *sysprint = "Hello world (debug_printf)\n";
    //sys_print(sysprint, strlen(sysprint));
    //debug_printf("Hello world (debug_printf)\n");
    printf("Hello world (normal printf)\n");
    for (int i = 0;i < argc; i ++) {
        printf("arg[%d] = %s\n", i, argv[i]);
    }

    // Check that we're in privileged mode
    uint16_t cs;
    __asm volatile("mov %%cs, %[reg]"
            : [reg] "=r" (cs));

    if((cs & 3) == 0) {
        printf("We're in privileged mode!\n");

        printf("Trying privileged operation...\n");
        uintptr_t cr0;
        __asm volatile("mov %%cr0, %[reg]"
                : [reg] "=r" (cr0));

        printf("Succeeded! CR0 is %" PRIxPTR "\n", cr0);
    } else {
        printf("NO privileged mode enabled\n");
        return EXIT_FAILURE;
    }

    genvaddr_t base;
    void *ptable;
    errval_t err;
    err = install_user_managed_pdpt(&base, &ptable);
    assert(err_is_ok(err));

    union x86_64_ptable_entry *pdpt = ptable;
    struct capref page;
    size_t size;
    err = frame_alloc(&page, 4ull * HUGE_PAGE_SIZE, &size);
    assert(err_is_ok(err));
    assert(size == 4ull*HUGE_PAGE_SIZE);
    struct frame_identity fi;
    err = invoke_frame_identify(page, &fi);
    assert(err_is_ok(err));

    for (uint64_t i = 0; i < 4; i++) {
        paging_x86_64_map_huge(&pdpt[i], fi.base + i*HUGE_PAGE_SIZE,
                vregion_to_pmap(VREGION_FLAGS_READ_WRITE));
    }

    printf("first entry: 0x%"PRIxGENVADDR"\n", pdpt[0].raw);

    uint8_t *buf = (uint8_t *)base;
    for (uint64_t i = 0; i < 4ull*HUGE_PAGE_SIZE / BASE_PAGE_SIZE; i++) {
        buf[i*BASE_PAGE_SIZE] = i % 256;
    }
    printf("filled 4G, flushing cache & tlb\n");
    wbinvd();
    do_full_tlb_flush();
    for (uint64_t i = 0; i < 4ull*HUGE_PAGE_SIZE / BASE_PAGE_SIZE; i++) {
        if (buf[i*BASE_PAGE_SIZE] != i % 256) {
            debug_printf("value mismatch at page %lu: expected %ld, got %d\n",
                    i, i % 256, buf[i*BASE_PAGE_SIZE]);
        }
    }
    printf("validated 4G, dumping ptables\n");

    debug_dump_hw_ptables();
    printf("done!\n\n");

    // make stuff not crash
    while(true);
    return EXIT_SUCCESS;
}
