/**
 * \file
 * \brief libc startup code.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <asmoffsets.h>
    .syntax unified
    .text
    .globl  _start, _start_init

_start:
        // Entry for processes other than init
        // Prepare arg1 init_dom_arg (false)
        mrc p15, 0, r0, c13, c0, 3 /* udisp */
        mov     r1, #0

_start_generic:
        ldr     sp, =crt0_temp_stack
        // Call barrelfish_init_disabled(struct dispatcher* d, bool init_dom_arg)
        b    barrelfish_init_disabled

_start_init:
        // Entry for the init process
        // Prepare arg1 init_dom_arg (true)
        mrc p15, 0, r0, c13, c0, 3 /* udisp */
        mov     r1, #1
        b       _start_generic


.section ".bss"
crt0_temp_stack_base:
        .space  8192
crt0_temp_stack:
