/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

static void test_malloc(size_t bytes)
{
    printf("malloctest: testing allocation of %zu bytes\n", bytes);
    uint8_t *buf = malloc(bytes);
    if (!buf) {
        printf("malloctest: malloc returned null\n");
        exit(1);
    }
    for (size_t i = 0; i < bytes; i++) {
        buf[i] = i % 256;
    }

    free(buf);

    return;
}

int main(void)
{

    test_malloc(5);

    test_malloc(1000);

    test_malloc(2*1024*1024ULL);

#if (UINTPTR_MAX == UINT64_MAX)
    test_malloc(512*1024*1024ULL);
#else
    test_malloc(256*1024*1024ULL); 
#endif

    printf("malloctest done.\n");
    return 0;
}
