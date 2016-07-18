/**
 * \file
 * \brief Low-level interrupt controller interaction
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTERRUPTS_H
#define INTERRUPTS_H

errval_t set_device_irq(char* device, uint32_t irq);

struct ioapic;
struct ioapic *find_ioapic(uint32_t gsi);


#endif /* INTERRUPTS_H */
