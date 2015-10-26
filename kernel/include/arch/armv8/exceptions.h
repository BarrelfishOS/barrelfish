/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARMV8_EXCEPTIONS_H
#define KERNEL_ARMV8_EXCEPTIONS_H

#include <barrelfish_kpi/types.h>

#ifndef __ASSEMBLER__

void handle_irq(void);
void page_fault(void *exn_frame);
void handle_sync_abort(uint64_t esr);

#else

.extern handle_irq, page_fault, handle_sync_abort
.type handle_irq @function
.type page_fault @function
.type handle_sync_abort @function

#endif // __ASSEMBLER__

#endif // KERNEL_ARMV8_EXCEPTIONS_H
