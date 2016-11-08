/*
 * Copyright (c) 2016, ETH Zurich.
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* Utility routines for booting. */

.section .efiboot, "ax"

.global eret, efiboot_start

/*
   void vminit_entry(uint32_t magic, void *pointer)
    __attribute__((noreturn,section(".vminit")));

   This is the entry point into the CPU driver.
   at this stage the registers contain the following contents:
   x0: multiboot magic value
   x1: pointer to multiboot image
   sp: points to top of stack

   this code jumps to vminit which has the following signature
   vminit(uint32_t magic, void *pointer, void *stack)

   x0, x1 are already the correct values, so we have to move copy the
   stack pointer to x3.
 */
efiboot_start:
    mov x2, sp
    b efiboot_init

/* Arguments x0-x3 are passed through */
eret:
    eret
