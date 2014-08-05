/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include "xomptest.h"

void do_process(uint32_t *src,
                uint32_t *dst)
{

    if (src == NULL || dst == 0) {
        return;
    }
#pragma omp parallel for
    for (int j = 0; j < IT; j++) {
        for (int i = 0; i < MAX; i += IT) {
            dst[i + j] = src[i + j];
        }
    }
}

void do_process_single(uint32_t *src,
                       uint32_t *dst)
{
    for (int j = 0; j < IT; j++) {
        for (int i = 0; i < MAX; i += IT) {
            dst[i + j] = src[i + j];
        }
    }
}
