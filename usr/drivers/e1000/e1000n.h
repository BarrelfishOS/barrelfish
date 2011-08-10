/**
 * \file
 * \brief Intel e1000 driver: Prototypes
 *
 * 
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E1000N_H_
#define E1000N_H_
#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include "e1000n_desc.h"
#include "e1000_dev.h"
#include "e1000n_debug.h"

uint16_t read_eeprom(struct e1000_t *dev, uint64_t offset);
void *alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap);
void e1000_hwinit(e1000_t *d, struct device_mem *bar_info,
                  int nr_allocated_bars,
                  volatile struct tx_desc **transmit_ring,
                  volatile union rx_desc **receive_ring,
                  int receive_buffers, int transmit_buffers,
                  uint8_t *macaddr, bool user_macaddr, bool use_interrupt);

#endif // E1000N_H_
