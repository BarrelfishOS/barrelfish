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

_RuneLocale _DefaultRuneLocale = {
    _RUNE_MAGIC_1,
    "NONE",
    NULL,
    NULL,
    0xFFFD,

    {   /*00*/  _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
    /*08*/  _CTYPE_C,
        _CTYPE_C|_CTYPE_S|_CTYPE_B,
        _CTYPE_C|_CTYPE_S,
        _CTYPE_C|_CTYPE_S,
        _CTYPE_C|_CTYPE_S,
        _CTYPE_C|_CTYPE_S,
        _CTYPE_C,
        _CTYPE_C,
    /*10*/  _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
    /*18*/  _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
        _CTYPE_C,
    /*20*/  _CTYPE_S|_CTYPE_B|_CTYPE_R,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
    /*28*/  _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
    /*30*/  _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|0,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|1,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|2,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|3,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|4,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|5,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|6,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|7,
    /*38*/  _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|8,
        _CTYPE_D|_CTYPE_R|_CTYPE_G|_CTYPE_X|9,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
    /*40*/  _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_U|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|10,
        _CTYPE_U|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|11,
        _CTYPE_U|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|12,
        _CTYPE_U|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|13,
        _CTYPE_U|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|14,
        _CTYPE_U|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|15,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
    /*48*/  _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
    /*50*/  _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
    /*58*/  _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_U|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
    /*60*/  _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_L|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|10,
        _CTYPE_L|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|11,
        _CTYPE_L|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|12,
        _CTYPE_L|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|13,
        _CTYPE_L|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|14,
        _CTYPE_L|_CTYPE_X|_CTYPE_R|_CTYPE_G|_CTYPE_A|15,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
    /*68*/  _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
    /*70*/  _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
    /*78*/  _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_L|_CTYPE_R|_CTYPE_G|_CTYPE_A,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_P|_CTYPE_R|_CTYPE_G,
        _CTYPE_C,
    },
    {   0x00,   0x01,   0x02,   0x03,   0x04,   0x05,   0x06,   0x07,
        0x08,   0x09,   0x0a,   0x0b,   0x0c,   0x0d,   0x0e,   0x0f,
    0x10,   0x11,   0x12,   0x13,   0x14,   0x15,   0x16,   0x17,
        0x18,   0x19,   0x1a,   0x1b,   0x1c,   0x1d,   0x1e,   0x1f,
    0x20,   0x21,   0x22,   0x23,   0x24,   0x25,   0x26,   0x27,
        0x28,   0x29,   0x2a,   0x2b,   0x2c,   0x2d,   0x2e,   0x2f,
    0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,
        0x38,   0x39,   0x3a,   0x3b,   0x3c,   0x3d,   0x3e,   0x3f,
    0x40,   'a',    'b',    'c',    'd',    'e',    'f',    'g',
        'h',    'i',    'j',    'k',    'l',    'm',    'n',    'o',
    'p',    'q',    'r',    's',    't',    'u',    'v',    'w',
        'x',    'y',    'z',    0x5b,   0x5c,   0x5d,   0x5e,   0x5f,
    0x60,   'a',    'b',    'c',    'd',    'e',    'f',    'g',
        'h',    'i',    'j',    'k',    'l',    'm',    'n',    'o',
    'p',    'q',    'r',    's',    't',    'u',    'v',    'w',
        'x',    'y',    'z',    0x7b,   0x7c,   0x7d,   0x7e,   0x7f,
    0x80,   0x81,   0x82,   0x83,   0x84,   0x85,   0x86,   0x87,
        0x88,   0x89,   0x8a,   0x8b,   0x8c,   0x8d,   0x8e,   0x8f,
    0x90,   0x91,   0x92,   0x93,   0x94,   0x95,   0x96,   0x97,
        0x98,   0x99,   0x9a,   0x9b,   0x9c,   0x9d,   0x9e,   0x9f,
    0xa0,   0xa1,   0xa2,   0xa3,   0xa4,   0xa5,   0xa6,   0xa7,
        0xa8,   0xa9,   0xaa,   0xab,   0xac,   0xad,   0xae,   0xaf,
    0xb0,   0xb1,   0xb2,   0xb3,   0xb4,   0xb5,   0xb6,   0xb7,
        0xb8,   0xb9,   0xba,   0xbb,   0xbc,   0xbd,   0xbe,   0xbf,
    0xc0,   0xc1,   0xc2,   0xc3,   0xc4,   0xc5,   0xc6,   0xc7,
        0xc8,   0xc9,   0xca,   0xcb,   0xcc,   0xcd,   0xce,   0xcf,
    0xd0,   0xd1,   0xd2,   0xd3,   0xd4,   0xd5,   0xd6,   0xd7,
        0xd8,   0xd9,   0xda,   0xdb,   0xdc,   0xdd,   0xde,   0xdf,
    0xe0,   0xe1,   0xe2,   0xe3,   0xe4,   0xe5,   0xe6,   0xe7,
        0xe8,   0xe9,   0xea,   0xeb,   0xec,   0xed,   0xee,   0xef,
    0xf0,   0xf1,   0xf2,   0xf3,   0xf4,   0xf5,   0xf6,   0xf7,
        0xf8,   0xf9,   0xfa,   0xfb,   0xfc,   0xfd,   0xfe,   0xff,
    },
    {   0x00,   0x01,   0x02,   0x03,   0x04,   0x05,   0x06,   0x07,
        0x08,   0x09,   0x0a,   0x0b,   0x0c,   0x0d,   0x0e,   0x0f,
    0x10,   0x11,   0x12,   0x13,   0x14,   0x15,   0x16,   0x17,
        0x18,   0x19,   0x1a,   0x1b,   0x1c,   0x1d,   0x1e,   0x1f,
    0x20,   0x21,   0x22,   0x23,   0x24,   0x25,   0x26,   0x27,
        0x28,   0x29,   0x2a,   0x2b,   0x2c,   0x2d,   0x2e,   0x2f,
    0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,
        0x38,   0x39,   0x3a,   0x3b,   0x3c,   0x3d,   0x3e,   0x3f,
    0x40,   'A',    'B',    'C',    'D',    'E',    'F',    'G',
        'H',    'I',    'J',    'K',    'L',    'M',    'N',    'O',
    'P',    'Q',    'R',    'S',    'T',    'U',    'V',    'W',
        'X',    'Y',    'Z',    0x5b,   0x5c,   0x5d,   0x5e,   0x5f,
    0x60,   'A',    'B',    'C',    'D',    'E',    'F',    'G',
        'H',    'I',    'J',    'K',    'L',    'M',    'N',    'O',
    'P',    'Q',    'R',    'S',    'T',    'U',    'V',    'W',
        'X',    'Y',    'Z',    0x7b,   0x7c,   0x7d,   0x7e,   0x7f,
    0x80,   0x81,   0x82,   0x83,   0x84,   0x85,   0x86,   0x87,
        0x88,   0x89,   0x8a,   0x8b,   0x8c,   0x8d,   0x8e,   0x8f,
    0x90,   0x91,   0x92,   0x93,   0x94,   0x95,   0x96,   0x97,
        0x98,   0x99,   0x9a,   0x9b,   0x9c,   0x9d,   0x9e,   0x9f,
    0xa0,   0xa1,   0xa2,   0xa3,   0xa4,   0xa5,   0xa6,   0xa7,
        0xa8,   0xa9,   0xaa,   0xab,   0xac,   0xad,   0xae,   0xaf,
    0xb0,   0xb1,   0xb2,   0xb3,   0xb4,   0xb5,   0xb6,   0xb7,
        0xb8,   0xb9,   0xba,   0xbb,   0xbc,   0xbd,   0xbe,   0xbf,
    0xc0,   0xc1,   0xc2,   0xc3,   0xc4,   0xc5,   0xc6,   0xc7,
        0xc8,   0xc9,   0xca,   0xcb,   0xcc,   0xcd,   0xce,   0xcf,
    0xd0,   0xd1,   0xd2,   0xd3,   0xd4,   0xd5,   0xd6,   0xd7,
        0xd8,   0xd9,   0xda,   0xdb,   0xdc,   0xdd,   0xde,   0xdf,
    0xe0,   0xe1,   0xe2,   0xe3,   0xe4,   0xe5,   0xe6,   0xe7,
        0xe8,   0xe9,   0xea,   0xeb,   0xec,   0xed,   0xee,   0xef,
    0xf0,   0xf1,   0xf2,   0xf3,   0xf4,   0xf5,   0xf6,   0xf7,
        0xf8,   0xf9,   0xfa,   0xfb,   0xfc,   0xfd,   0xfe,   0xff,
    },
    {0, NULL}, {0, NULL}, {0, NULL}, NULL, 0
};

_RuneLocale *_CurrentRuneLocale = &_DefaultRuneLocale;

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

