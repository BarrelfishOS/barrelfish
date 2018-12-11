/**
 * \file
 * \brief High-Precision Event Timer driver.
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HPET_H
#define HPET_H

#include <barrelfish/barrelfish.h>
#include <hw_records.h>
#include <skb/skb.h>

#define HPET_MEM_CAP 0
#define HPET_COMP_INT_CAP 0

#define DEFAULT_COMPARATOR 9000000

struct hpet_t;

/* Per HPET state */
struct hpet_driver_state {
    struct hpet_t *d;
    uint64_t int_start_range;
    uint64_t int_end_range;
    uint64_t msix_port; // int out range
    uint8_t nTimers; 
};


int hpet_init(void);
errval_t map_fsb_int(uint64_t from_port, uint32_t msg_addr, uint32_t msg_data,
                     void *driv_state);

/* Slightly ugly global variable, used by the hpet_comp to find the mmaped region */
extern lvaddr_t hpet_vbase;

#endif
