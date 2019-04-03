/**
 * \file
 * \brief Struct definition for the boot param struct supplied by the
 *        K1OM boot loader
 */

/*
 * Copyright (c) 2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 *
 * This is adapted from the Linux kernel (kernel.org)
 *
 */

/*
 * Include file for the interface to an APM BIOS
 * Copyright 1994-2001 Stephen Rothwell (sfr@canb.auug.org.au)
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */
#ifndef KERNEL_APM_BIOS_H
#define KERNEL_APM_BIOS_H

typedef unsigned short  apm_event_t;
typedef unsigned short  apm_eventinfo_t;

struct apm_bios_info {
    uint16_t   version;
    uint16_t   cseg;
    uint32_t   offset;
    uint16_t   cseg_16;
    uint16_t   dseg;
    uint16_t   flags;
    uint16_t   cseg_len;
    uint16_t   cseg_16_len;
    uint16_t   dseg_len;
};


/*
 * Power states
 */
#define APM_STATE_READY     0x0000
#define APM_STATE_STANDBY   0x0001
#define APM_STATE_SUSPEND   0x0002
#define APM_STATE_OFF       0x0003
#define APM_STATE_BUSY      0x0004
#define APM_STATE_REJECT    0x0005
#define APM_STATE_OEM_SYS   0x0020
#define APM_STATE_OEM_DEV   0x0040

#define APM_STATE_DISABLE   0x0000
#define APM_STATE_ENABLE    0x0001

#define APM_STATE_DISENGAGE 0x0000
#define APM_STATE_ENGAGE    0x0001

/*
 * Events (results of Get PM Event)
 */
#define APM_SYS_STANDBY     0x0001
#define APM_SYS_SUSPEND     0x0002
#define APM_NORMAL_RESUME   0x0003
#define APM_CRITICAL_RESUME 0x0004
#define APM_LOW_BATTERY     0x0005
#define APM_POWER_STATUS_CHANGE 0x0006
#define APM_UPDATE_TIME     0x0007
#define APM_CRITICAL_SUSPEND    0x0008
#define APM_USER_STANDBY    0x0009
#define APM_USER_SUSPEND    0x000a
#define APM_STANDBY_RESUME  0x000b
#define APM_CAPABILITY_CHANGE   0x000c

/*
 * Error codes
 */
#define APM_SUCCESS     0x00
#define APM_DISABLED        0x01
#define APM_CONNECTED       0x02
#define APM_NOT_CONNECTED   0x03
#define APM_16_CONNECTED    0x05
#define APM_16_UNSUPPORTED  0x06
#define APM_32_CONNECTED    0x07
#define APM_32_UNSUPPORTED  0x08
#define APM_BAD_DEVICE      0x09
#define APM_BAD_PARAM       0x0a
#define APM_NOT_ENGAGED     0x0b
#define APM_BAD_FUNCTION    0x0c
#define APM_RESUME_DISABLED 0x0d
#define APM_NO_ERROR        0x53
#define APM_BAD_STATE       0x60
#define APM_NO_EVENTS       0x80
#define APM_NOT_PRESENT     0x86

/*
 * APM Device IDs
 */
#define APM_DEVICE_BIOS     0x0000
#define APM_DEVICE_ALL      0x0001
#define APM_DEVICE_DISPLAY  0x0100
#define APM_DEVICE_STORAGE  0x0200
#define APM_DEVICE_PARALLEL 0x0300
#define APM_DEVICE_SERIAL   0x0400
#define APM_DEVICE_NETWORK  0x0500
#define APM_DEVICE_PCMCIA   0x0600
#define APM_DEVICE_BATTERY  0x8000
#define APM_DEVICE_OEM      0xe000
#define APM_DEVICE_OLD_ALL  0xffff
#define APM_DEVICE_CLASS    0x00ff
#define APM_DEVICE_MASK     0xff00


/*
 * Battery status
 */
#define APM_MAX_BATTERIES   2

/*
 * APM defined capability bit flags
 */
#define APM_CAP_GLOBAL_STANDBY          0x0001
#define APM_CAP_GLOBAL_SUSPEND          0x0002
#define APM_CAP_RESUME_STANDBY_TIMER    0x0004 /* Timer resume from standby */
#define APM_CAP_RESUME_SUSPEND_TIMER    0x0008 /* Timer resume from suspend */
#define APM_CAP_RESUME_STANDBY_RING     0x0010 /* Resume on Ring fr standby */
#define APM_CAP_RESUME_SUSPEND_RING     0x0020 /* Resume on Ring fr suspend */
#define APM_CAP_RESUME_STANDBY_PCMCIA   0x0040 /* Resume on PCMCIA Ring */
#define APM_CAP_RESUME_SUSPEND_PCMCIA   0x0080 /* Resume on PCMCIA Ring */

#endif /* KERNEL_APM_BIOS_H */
