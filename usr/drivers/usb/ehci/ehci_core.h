/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains sub-set (basically related to I/O requests) of basic services 
 * exported by EHCI server 
 */

#ifndef __EHCI_CORE_H
#define __EHCI_CORE_H

#include <barrelfish/barrelfish.h>
#include <stdio.h>

#include <usb/ch9.h>
#include <usb/usb_pipe.h>

#define EHCI_NO_DEBUG 0
#define EHCI_DEBUG 1


int usb_dctrl_exe(usb_device_request usb_req, void *buff, uint64_t sz,
                  uint8_t device, int debug);

int usb_ctrl_exe(usb_device_request usb_req, uint8_t device, int debug);

int terminate_req(usb_device_request req);

int usb_bulk_exe(usb_pipe_t pipe, void *p_buff, uint32_t len, int debug);

#endif                          // __EHCI_CORE_H
