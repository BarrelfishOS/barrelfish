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
#include <dev/hpet_dev.h>

#define HPET_MEM_CAP 0
#define HPET_INT_CAP 1

#define DEFAULT_COMPARATOR 9000000   

struct hpet_driver_state {
   hpet_t d;
 
   uint64_t int_start_range; 
   uint64_t int_end_range; 
   uint64_t msix_port; // int out range
  
 
};


int hpet_init(void);
errval_t map_fsb_int(uint64_t from_port, uint32_t msg_addr , uint32_t msg_data , void * driv_state);

#endif
