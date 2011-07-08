/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_MASS_STORAGE_CLASS_H
#define USB_MASS_STORAGE_CLASS_H

#include <barrelfish/barrelfish.h>

// Only MSB is used in flag field
#define CBW_FLAG_IN   0x80
#define CBW_FLAG_OUT  0x00

/* USB mass storage, bulk only transportation command block
 * wrapper. As defined in USB Mass Storage Class:
 * Bulk-only transportation
 * page number : 13/22
 */

#pragma pack(push,1)
typedef struct cbw_t {
    uint32_t dCBWSignature;
    uint32_t dCBWTag;
    uint32_t dCBWDataTransferLength;
    uint8_t bmCBWFlags;
    uint8_t bCBWLUN;
    uint8_t bCBWCBLength;
    uint8_t CBWCB[16];
} cbw_t;


/* USB mass storage, bulk only
 * transportation command status
 * wrapper. As defined in
 * USB Mass Storage Class: Bulk-only transportation
 * page number : 14/22
 */

typedef struct csw_t {
    uint32_t dCSWSignature;
    uint32_t dCSWTag;
    uint32_t dCSWDataResidue;
    uint8_t bCSWStatus;
} csw_t;

#pragma pack()

/*
 * Helper functions
 */

uint32_t get_new_tag(void);

void print_cbw(cbw_t cbw);
void print_csw(csw_t csw);

// Assuming that, LUNs are zero
void init_new_cbw(uint32_t tag, uint32_t len, uint8_t flags, uint8_t cmd_len,
                  uint8_t * cmd, cbw_t * cmd_wrapper);
// Similar with Status
void init_new_csw(uint32_t tag, csw_t * status_wrapper);

#endif                          // USB_MASS_STORAGE_H
