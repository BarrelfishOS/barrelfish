/**
 * \file
 * \brief Run all new kernel memory tests
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "tests.h"

int main(void)
{
    int r;
    if((r = map_unmap())) {
        printf("map_unmap returned %d\n", r); 
        return 1;
    }

    if((r = modify_flags())) {
        printf("modify_flags returned %d\n", r); 
        return 2;
    }

#ifdef __x86_64__
    if((r = invalid_mappings())) {
        printf("invalid_mappings returned %d\n", r); 
        return 3;
    }
#endif

    printf("nkmtest_all: all tests passed\n");

    return 0;
}
