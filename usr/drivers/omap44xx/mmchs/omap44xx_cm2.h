/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef __ARM_CM2_H__
#define __ARM_CM2_H__

#include <barrelfish/barrelfish.h>
#include <dev/omap/omap44xx_mmchs_dev.h>
#include <dev/omap/omap44xx_sysctrl_padconf_core_dev.h>

// We need this because paging_arm_device returns the address 
// of the beginning of the section in virtual memory ..
// We map sections because we don't use the second level page table 
// at this point yet. XXXX Is this actually true?
#define omap_dev_map(p) \
    (mackerel_addr_t) \
    (paging_map_device(p, ARM_L1_SECTION_BYTES) \
     + (p & ARM_L1_SECTION_MASK))

void cm2_enable_i2c(size_t i2c_index);
int  cm2_get_hsmmc1_base_clock(void);
void cm2_debug_print(void);
void cm2_print_standby_state(void);

void cm2_enable_hsmmc1(void);
void cm2_init(void);

#endif /* __ARM_CM2_H__ */
