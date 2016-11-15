/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCID_
#define _AHCID_

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>
#include <barrelfish/syscalls.h>
#include <barrelfish/nameservice_client.h>
#include <pci/pci.h>
#include <skb/skb.h>
#include <blk/ahci.h>

#define DISABLE_INTERRUPTS 1

extern void* dq;
extern struct waitset disk_ws;

#endif // _AHCID_
