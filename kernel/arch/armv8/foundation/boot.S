/*
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

.section .text.shim

.globl start
.extern kernel_stack

#include <offsets.h>

start:
    ldr     x0, =kernel_stack
    add     sp, x0, #KERNEL_STACK_SIZE
    b       shim_entry
