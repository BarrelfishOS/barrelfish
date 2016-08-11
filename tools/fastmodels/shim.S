/* System initialisation shim

   This code is executed on boot, by the simulator, to initialise the hardware
   as the CPU driver expects it i.e. as initialised by UEFI/Hagfish.
*/

#define ASM_FILE
#include <multiboot2.h>

#define SHIM_STACK 256

.global invalidate_caches, start

/* We should begin in EL3, caches disabled. */
start:
    /* A small stack to call into C. */
    adr x0, shim_stack
    add sp, x0, #(SHIM_STACK - 8)

    /* Invalidate the TLB */
    tlbi alle3

    /* Load the table base register, table must map this code 1-1. */
    ldr x0, p_kernel_table
    msr ttbr0_el3, x0

    /* Set memory attribute 0 to Normal, inner and outer WB, non-transient.
     * Set memory attribute 1 to Device, nGnRnE (strongly ordered). */
    mov x0, #0x00ff
    msr mair_el3, x0

    /* Set the translation paramaters. */
    mov x1, xzr
    mov x2, #5
    bfi x1, x2, #16, 3   /* 48b PA */
    /* orr x1, x1, #(0<<14) */ /* 4kB granule */
    orr x1, x1, #(3<<12) /* Table IS */
    orr x1, x1, #(1<<10) /* Table Outer WB, WA */
    orr x1, x1, #(1<< 8) /* Table Inner WB, WA */
    orr x1, x1, #16      /* 48b VA */
    msr tcr_el3, x1
    dsb sy

    /* Enable the MMU. */
    mrs x0, sctlr_el3
    orr x0, x0, #(1<<0) /* MMU control bit. */
    msr sctlr_el3, x0
    isb

    /* Enable Caches. */
    bl invalidate_caches
    dmb sy
    isb
    mrs x0, sctlr_el3
    orr x0, x0, #(1<<2) /* Cache control bit. */
    msr sctlr_el3, x0

    /* Switch to the kernel stack. */
    ldr x2, p_kernel_stack_top
    mov sp, x2

    /* Jump to the kernel entry point, passing the multiboot magic (boot
     * core), the address of the multiboot header, and the top of the kernel
     * stack. */
    ldr x0, magic
    ldr x1, p_multiboot
    ldr x5, p_entry
    br x5

/* These pointers are filled in at load time. */
.align 8
p_entry:            .dword 0
p_multiboot:        .dword 0
p_kernel_table:     .dword 0
p_kernel_stack_top: .dword 0

magic:
    .word MULTIBOOT2_BOOTLOADER_MAGIC

.align 8
shim_stack:
    .space SHIM_STACK
