
/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#define ASM_FILE 1

#ifndef ASM_FILE
#define ASM_FILE 1
#endif
#include <multiboot2.h>


.section .boot, "ax"

.global eret, boot_entry, boot_entry_psci, boot_entry_parking


/*
    This is the kernel entry point. We distinguish between how and who booted
    us. In the following, an enumeration of possible boot modes. In all cases,
    we use register X0 as a distinction.

    EFI Boot:
    Started by: UEFI (hagfish)
    X0 Value: MULTIBOOT2_BOOTLOADER_MAGIC


    PSCI Boot:
    Started by: Running kernel
    X0 Value: Pointer to ARMv8 core data

    Parking Protocol Boot:
    Started by: Running kernel
    X0 Value: Pointer to ARMv8 core data
 */


boot_entry:
    /* load the MULTIBOOT2_BOOTLOADER_MAGIC value into register x4. */
    movz x4, #(MULTIBOOT2_BOOTLOADER_MAGIC >> 16), LSL #16
    movk x4, #(MULTIBOOT2_BOOTLOADER_MAGIC & 0xffff)

    /* compare with register x0 */
    cmp x0, x4

    /* branch off to psci or parking boot */
    b.ne psci_or_parking_boot

    /* Calling the boot initialization function for the BSP core.

      void boot_bsp_init(uint32_t magic,
                         lpaddr_t pointer,
                         lpaddr_t kernel_stack_top)

      The function arguments are as follows:
      magic: register x0
      pointer: register x1
      kernel_stack_top: register x2
    */

    /* store the stack pointer at x2 */
    mov x2, sp

    /* branch to the C function */
    b boot_bsp_init



/* Calling the initialization function for the APP cores.

   void boot_app_init(lpaddr_t context);

   Register x0 holds the pointer to the context (ARMv8 core data).
   We need to extract the stack pointer from the context and set it
   before we branch off to the C function
 */

psci_or_parking_boot:
    ldr x1, [x0]
    mov sp, x1
    b boot_app_init


/* Error return required to drop to lower execution levels */
eret:
    eret


