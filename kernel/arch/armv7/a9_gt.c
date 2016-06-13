/**
 * \file
 * \brief ARM Cortex A9 Global Timer driver.
 */

/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <a9_gt.h>
#include <a9mpcore_map.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <dev/cortex_a9_gt_dev.h>

static cortex_a9_gt_t a9_gt;
static bool initialized = false;

#define MSG(format, ...) \
    printk( LOG_NOTE, "CortexA9 GT: "format, ## __VA_ARGS__ )

/*
 * Initialize the timer.  The MMU is on.
 */
void a9_gt_init(lpaddr_t addr)
{
    assert(!initialized);
    lvaddr_t gt_base = paging_map_device(addr, A9MPCORE_TIMER_GBL_SIZE);
    cortex_a9_gt_initialize(&a9_gt, (mackerel_addr_t)gt_base);
    initialized = true;
    MSG("initialized at 0x%"PRIxLVADDR"\n", gt_base);

    // enable auto increment
    cortex_a9_gt_TimerControl_auto_increment_wrf(&a9_gt, 0x1);

    // reset timer (TRM 4.4.4)
    cortex_a9_gt_TimerControl_timer_enable_wrf(&a9_gt, 0x0);
    cortex_a9_gt_TimerCounterLow_wr(&a9_gt, 0x0);
    cortex_a9_gt_TimerCounterHigh_wr(&a9_gt, 0x0);
    cortex_a9_gt_TimerControl_timer_enable_wrf(&a9_gt, 0x1);
}

uint64_t a9_gt_read(void)
{
    // need to re-read high value according to ARM TRM 4.4.1
    uint32_t low, high;
    do {
        high = cortex_a9_gt_TimerCounterHigh_rd(&a9_gt);
        low  = cortex_a9_gt_TimerCounterLow_rd(&a9_gt);
    } while(high != cortex_a9_gt_TimerCounterHigh_rd(&a9_gt));

    return (((uint64_t) high) << 32) | ((uint32_t) low);
}

uint32_t a9_gt_read_low(void)
{
    return cortex_a9_gt_TimerCounterLow_rd(&a9_gt);
}

uint32_t a9_gt_read_high(void)
{
    return cortex_a9_gt_TimerCounterHigh_rd(&a9_gt);
}
