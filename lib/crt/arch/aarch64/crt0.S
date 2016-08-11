/**
 * \file
 * \brief libc startup code.
 */

/*
 * Copyright (c) 2007,2008, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <asmoffsets.h>
.text
.globl  _start, _start_init

_start:
    // Entry for processes other than init
    // Prepare arg1 init_dom_arg (false)
    mov x1, #0

_start_generic:
    // Get the dispatcher handler from the thread ID register.
    mrs x0, tpidrro_el0
    ldr x9, =crt0_temp_stack
    mov sp, x9
    // Call barrelfish_init_disabled(struct dispatcher* d, bool init_dom_arg)
    b barrelfish_init_disabled

_start_init:
    // Entry for the init process
    // Prepare arg1 init_dom_arg (true)
    mov     x1, #1
    b       _start_generic

.section ".bss"
crt0_temp_stack_base:
        .space  8192
crt0_temp_stack:
