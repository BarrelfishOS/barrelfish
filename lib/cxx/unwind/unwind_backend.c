/** \file
 *  \brief A simple work stealing library based upon both cilk and wool.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unwind.h>
#include <barrelfish/barrelfish.h>
#include <runetype.h>

#define _CTYPE_A    0x00000100L     /* Alpha */
#define _CTYPE_C    0x00000200L     /* Control */
#define _CTYPE_D    0x00000400L     /* Digit */
#define _CTYPE_G    0x00000800L     /* Graph */
#define _CTYPE_L    0x00001000L     /* Lower */
#define _CTYPE_P    0x00002000L     /* Punct */
#define _CTYPE_S    0x00004000L     /* Space */
#define _CTYPE_U    0x00008000L     /* Upper */
#define _CTYPE_X    0x00010000L     /* X digit */
#define _CTYPE_B    0x00020000L     /* Blank */
#define _CTYPE_R    0x00040000L     /* Print */
#define _CTYPE_I    0x00080000L     /* Ideogram */
#define _CTYPE_T    0x00100000L     /* Special */
#define _CTYPE_Q    0x00200000L     /* Phonogram */
#define _CTYPE_SW0  0x20000000L     /* 0 width character */
#define _CTYPE_SW1  0x40000000L     /* 1 width character */
#define _CTYPE_SW2  0x80000000L     /* 2 width character */
#define _CTYPE_SW3  0xc0000000L     /* 3 width character */
#define _CTYPE_SWM  0xe0000000L     /* Mask for screen width data */
#define _CTYPE_SWS  30          /* Bits to shift to get width */

#include <barrelfish/barrelfish.h>

void bf_unwind_get_eh(uint64_t *eh_frame, uint64_t *eh_frame_size)
{
    lvaddr_t eh;
    size_t eh_size;

    disp_get_eh_frame(&eh, &eh_size);

    if (eh_frame) {
        *eh_frame = eh;
    }
    if (eh_frame_size) {
        *eh_frame_size = eh_size;
    }
}

void bf_unwind_get_eh_hdr(uint64_t *eh_frame_hdr, uint64_t *eh_frame_hdr_size)
{
    lvaddr_t eh;
    size_t eh_size;

    disp_get_eh_frame_hdr(&eh, &eh_size);

    if (eh_frame_hdr) {
        *eh_frame_hdr = eh;
    }
    if (eh_frame_hdr_size) {
        *eh_frame_hdr_size = eh_size;
    }
}
