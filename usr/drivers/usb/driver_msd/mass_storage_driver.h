/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_CLIENT_H
#define USB_CLIENT_H

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include "scsi.h"

/*
 * Function to handle new device
 */

void handle_device(uint8_t dev);
scsi_device_t *export_scsi_dev(void);
void read10(uint8_t dev, uint32_t block,
            uint16_t num, void *buff, uint8_t cache);
void write10(uint8_t dev, uint32_t block, uint16_t num, void *pbuff,
             uint8_t cache);

void sync_cache_on_dev(uint8_t dev, uint32_t block, uint16_t num);
#endif
