/**
 * \file
 * \brief contains functions to signal a linux based host
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


static inline void notify_host(void) {
    volatile uint32_t *signal;
    /* XXX this has to be done without MMU enabled or with 1:1 mapping */
    signal = (uint32_t *)(0x08007D0000ULL + 0x0000AB28);
   *signal |= 0x1;
}
