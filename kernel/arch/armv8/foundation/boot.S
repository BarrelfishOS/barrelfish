/*
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

.text

.globl start, halt, got_base
.extern kernel_stack

#include <offsets.h>

.extern test_string

start:
/*
    mov x0, #0x1c090000
    mov w1, #0x10
    str w1, [x0, #0x24]
    str wzr, [x0, #0x28]
    mov w1, #0x70
    str w1, [x0, #0x2c]
    mov w1, #0x301
    str w1, [x0, #0x30]

    mov w1, #0x70
    str w1, [x0]

loop:
    b loop
*/

    ldr     x0, =kernel_stack
    add     sp, x0, #KERNEL_STACK_SIZE
    b       shim_entry
