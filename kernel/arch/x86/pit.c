/**
 * \file
 * \brief x86 legacy timer driver (PIT).
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <kernel.h>
#include <arch/x86/pit.h>
#include <dev/lpc_timer_dev.h>

#define TIMER_IOBASE    0x40

static struct lpc_timer_t timer;        ///< Mackerel state for timer registers

/**
 * \brief Set hardware timer mode and count value
 *
 * Sets timer 0 as either a rate generator (mode 2) or one-shot timer (mode 0)
 * and sets its value. This function currently does not deal with any other
 * timers.
 *
 * \param count  Count for oneshot timer, rate for ticker
 * \param periodic True for a periodic timer, false for oneshot
 */
void pit_timer0_set(uint16_t count, bool periodic, bool only_lsb)
{
    struct lpc_timer_tcw_t tcw = {
        .bcd = 0,                       // Binary mode (no BCD)
        .mode = periodic ? lpc_timer_rtgen : lpc_timer_oseoc, // Operating mode
        .rwsel = only_lsb ? lpc_timer_lsb: lpc_timer_lmsb,
        .select = lpc_timer_c0          // Select counter 0
    };

    // Prepare timer 0 to set its count
    lpc_timer_tcw_wr(&timer, tcw);

    if (count > 0) {
        // Set the count/rate (LSB, then MSB)
        if (!only_lsb)
            lpc_timer_cntacc0_wr(&timer, count & 0xff);
        lpc_timer_cntacc0_wr(&timer, count >> 8);
    }
}

/**
 * \brief Read current value of timer
 *
 * \returns the current value of timer 0
 */
uint16_t pit_timer0_read(void)
{
    uint16_t val;
    lpc_timer_sbyte_fmt_t status;

    do {
        // 1. Issue read back command to read the status and count of the counter
        struct lpc_timer_rdbk_cmd_t cmd = {
            .c0 = 1, .c1 = 0, .c2 = 0,  // select counter 0 only
            .stat = 0, .count = 0       // latch both status and count
        };
        lpc_timer_rdbk_cmd_wr(&timer, cmd);

        // 2. Read status
        status = lpc_timer_sbyte_fmt0_rd(&timer);

        // 3. Read value latched value (LSB, then MSB)
        // (we must do this even if the status shows an invalid count)
        val = lpc_timer_cntacc0_rd(&timer) << 8;
        val |= lpc_timer_cntacc0_rd(&timer);

    // if we got unlucky, and read the counter before it had finished loading,
    // the count may be invalid ("null count"), so we repeat the whole rigmarole
    } while (status.cnt_stat);

    return val;
}

uint8_t pit_timer0_read_lsb(void)
{
    return lpc_timer_cntacc0_rd(&timer);
}

void pit_init(void)
{
    lpc_timer_initialize(&timer, TIMER_IOBASE);
    pit_timer0_set(0, false, false);
}
