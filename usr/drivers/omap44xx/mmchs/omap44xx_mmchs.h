/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef __ARM_MMCHS_H__
#define __ARM_MMCHS_H__

#include <barrelfish/types.h>

// Use 3.3 Volt for bus voltage
// #define MMCHS_VS33 1

// Base address of the MMC host controller
// TRM Table 24-56, page 5140
#define MMCHS_BASE 0x4809C000U

/*
 * List of commands
 *
 * see TRM MMCHS_CMD documentation, table 24-83, page 5160
 * and SD card specification
 */
#define CMD0 0x0   // GO_IDLE_STATE / maybe also just a card reset ..
#define CMD5 0x5   // SEND_OP_COND
#define CMD8 0x8   // Detect v2 card, handshake voltage

#define MMCHS_RESP_SUCCESS 0UL
#define MMCHS_RESP_TIMEOUT 1UL

#define CYCLES_PER_MSEC 1200000UL
#define CYCLES_PER_SEC (1000*CYCLES_PER_MSEC)

void        mmchs_send_cmd(int cmd_idx, uint32_t arg);
void        mmchs_identify_card(void);
int         mmchs_init_and_ident_card(void);

static void mmchs_change_clock_frequency(int clkdiv);
static int  mmchs_finalize_cmd(void);
void mmchs_handle_irq(void *args);
void mmchs_init(void);

// MMC1 interrupt, two lines
//    (MM_IRQ_50 for Cortex-M3 INTC)
//     MA_IRQ_83 for Cortex-A9 INTC
// see Free BSD sys/arm/ti/omap4/omap4_reg.h
#define MMC1_IRQ (32+83) 

#endif /* __ARM_MMCHS_H__ */
