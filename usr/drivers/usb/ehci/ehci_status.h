/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains macros used to check status of a transaction
 */

#ifndef __EHCI_STATUS_H
#define __EHCI_STATUS_H

#define USB_TRAN_STATUS_FINISHED    0x00
#define USB_TRAN_STATUS_ACTIVE      0x80
#define USB_TRAN_STATUS_HALTED      0x40
#define USB_TRAN_STATUS_DBUFF_ERR   0x20
#define USB_TRAN_STATUS_BABBLE      0x10
#define USB_TRAN_STATUS_TX_ERR      0x08
#define USB_TRAN_STATUS_MISS        0x04
#define USB_TRAN_STATUS_SPLITX      0x02
#define USB_TRAN_STATUS_PERR        0x01

#endif                          // __EHCI_STATUS_H
