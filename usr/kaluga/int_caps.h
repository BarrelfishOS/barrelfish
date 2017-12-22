/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INT_CAPS_H
#define INT_CAPS_H

errval_t store_int_cap(int start, int end, struct driver_argument *arg); 
errval_t init_int_caps_manager(struct capref all_irq);

#endif
