/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_
#define _AHCI_

#include <barrelfish/barrelfish.h>
#include <pci/mem.h>

struct ahci_disk;

errval_t blk_ahci_init(struct device_mem* bar5, struct ahci_disk** out);
errval_t blk_ahci_stop(struct ahci_disk* ad);

#endif // _AHCI_
