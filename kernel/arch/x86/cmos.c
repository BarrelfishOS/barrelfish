/**
 * \file
 * \brief CMOS RAM interface functions
 *
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <arch/x86/cmos.h>
#include "cmos_dev.h"

/** \brief This function reads the hardware clock.
    This function reads the hardware real time clock and fills the
    passed struct with the read values.
    \param t pointer to a rtc_time struct
*/

void cmos_write(int addr, uint8_t b)
{
    cmos_t rtc;
    cmos_initialize(&rtc,0x00);
    cmos_ndx_wr(&rtc,addr);
    cmos_target_wr(&rtc,b);
}

void cmos_write_extended(int addr, uint8_t b)
{
    cmos_t rtc;
    cmos_initialize(&rtc,0x00);
    cmos_endx_wr(&rtc,addr);
    cmos_etarget_wr(&rtc,b);
}

uint8_t cmos_read(int addr)
{
    cmos_t rtc;
    cmos_initialize(&rtc,0x00);
    cmos_ndx_wr(&rtc,addr);
    return cmos_target_rd(&rtc);
}

uint8_t cmos_read_extended(int addr, uint8_t b)
{
    cmos_t rtc;
    cmos_initialize(&rtc,0x00);
    cmos_endx_wr(&rtc,addr);
    return cmos_etarget_rd(&rtc);
}

