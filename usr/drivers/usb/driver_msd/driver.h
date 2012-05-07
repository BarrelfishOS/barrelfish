/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains services exported by mass storage 
 * driver server
 */

#ifndef USB_DRIVER_H
#define USB_DRIVER_H

#include <barrelfish/barrelfish.h>
#include "scsi.h"

#define ACCEPT 0x0
#define REJECT 0x1

int probe(uint8_t dev, uint8_t clss, uint8_t subclass, uint8_t protocol);
int disconnect(uint8_t dev);
void get_scsi_dev(scsi_device_t dev);
void read_scsi(scsi_device_t dev, uint32_t start, uint32_t num, uint64_t buff,
               uint8_t cache);
void write_scsi(scsi_device_t dev, uint32_t start, uint32_t num, uint64_t buff,
                uint8_t cache);
void sync_cache(scsi_device_t dev);

#endif                          //USB_DRIVER_H
