/*
 * \file
 * \brief Solarflare sfn5122f driver: Constants
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

#ifndef SFN5122F_H_
#define SFN5122F_H_

#include <dev/sfn5122f_dev.h>
#include "mcdi_rpc.h"
#include "helper.h"

#define NUM_QUEUES 1024

#define BUF_SIZE 4096
#define DEVICE_ID 0x803
// TX Queue
#define TX_DESC_CACHE_SIZE 16
#define TX_ENTRIES 4096
#define TX_DC_BASE 0x11000
// Event Queue
#define EV_CODE_RX 0
#define EV_CODE_TX 2
#define EV_CODE_DRV 5
#define EV_CODE_DRV_GEN 7
#define EV_CODE_USER 8
#define EV_CODE_MCDI 12
#define EV_CODE_GLOBAL 6

/* for each TX/RX entry one entry plus an additonal 2 for mcdi completion
and link state events */
#define EV_ENTRIES 32768

// RX Queue
#define RX_DESC_CACHE_SIZE 64
#define RX_ENTRIES 4096
#define RX_DC_BASE 0xD000
// calculcat max frame length
#define MTU 1500
#define MTU_MAX 2048
/* PHY and MAC stats       */
#define NUM_MAC_STATS 0x61
// Numer of buffer table entries for each type of queue
#define NUM_ENT_EVQ ((EV_ENTRIES*8)/BUF_SIZE)
#define NUM_ENT_RX ((RX_ENTRIES*8)/BUF_SIZE)
#define NUM_ENT_TX ((TX_ENTRIES*8) / BUF_SIZE)
#define NUM_ENT_RX_USR  ((RX_ENTRIES*4) / BUF_SIZE)
#define MAX_BUF_TBL_ENTRIES 147456

// Filters
#define NUM_FILTERS_IP 8192
#define NUM_FILTERS_MAC 512


#endif /* SFN5122F_H_ */
