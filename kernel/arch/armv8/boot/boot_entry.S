
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
#include <asmoffsets.h>


.text

.global eret, boot_entry_bsp, boot_entry_psci, boot_entry_parking

.type boot_entry_bsp, @function
.type boot_entry_psci, @function
.type boot_entry_parking, @function

/*
    This is the kernel entry point. We distinguish between how and who booted
    us. In the following, an enumeration of possible boot modes. In all cases,
    we use register X0 as a distinction.

    EFI Boot:
    Started by: UEFI (hagfish)
    X0 Value: MULTIBOOT2_BOOTLOADER_MAGIC
    X1 Value: Pointer to ARMV8 core data

    PSCI Boot:
    Started by: Running kernel
    X0 Value: Pointer to ARMv8 core data

    Parking Protocol Boot:
    Started by: Running kernel
    X0 Value: Pointer to ARMv8 core data
 */


boot_entry_bsp:
     /* Calling the boot initialization function for the BSP core.

      void boot_bsp_init(uint32_t magic,
                         lpaddr_t pointer)

      The function arguments are as follows:
      magic: register x0
      pointer: register x1
    */

    /* branch to the C function */
    b boot_bsp_init



/* Calling the initialization function for the APP cores.

   void boot_app_init(lpaddr_t context);

   Register x0 holds the pointer to the context (ARMv8 core data).
   We need to extract the stack pointer from the context and set it
   before we branch off to the C function
 */

boot_entry_psci:
    ldr x1, [x0, #OFFSETOF_COREDATA_KERNEL_STACK]
    mov sp, x1
    b boot_app_init
    wfi


boot_entry_parking:
    /*
        we are using the remainder of the OS use part of the parking frame for
        the stack
     */
    add sp, x0, #2032
    b boot_app_init
    wfi

/* Error return required to drop to lower execution levels */
eret:
    eret


