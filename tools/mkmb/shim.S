/* System initialisation shim

   This code is executed on boot, by the simulator, to initialise the hardware
   as the CPU driver expects it i.e. as initialised by UEFI/Hagfish.
*/

#define ASM_FILE
#include <multiboot2.h>

#define SHIM_STACK 256

.global invalidate_caches, start

/* We should begin in EL2, caches disabled. */
start:
    /* A small stack to call into C. */
    adr x0, shim_stack
    add sp, x0, #(SHIM_STACK - 8)

    /* Enable coherence (SMPEN) */
    mrs x0, S3_1_c15_c2_1
    orr x0, x0, #(1<<6)
    msr S3_1_c15_c2_1, x0

    /* Enable Caches. */
    bl invalidate_caches
    dmb sy
    isb
    mrs x0, sctlr_el2
    orr x0, x0, #(1<<2) /* Cache control bit. */
    msr sctlr_el2, x0

    /* Enable the MMU, table must map this region 1-1. */
    ldr x0, p_kernel_table
    msr ttbr0_el2, x0
    mov x1, xzr
    mov x2, #5
    bfi x1, x2, #16, 3   /* 48b PA */
    /* orr x1, x1, #(0<<14) */ /* 4kB granule */
    orr x1, x1, #(3<<12) /* Table IS */
    orr x1, x1, #(1<<10) /* Table Outer WB, WA */
    orr x1, x1, #(1<< 8) /* Table Inner WB, WA */
    orr x1, x1, #16      /* 48b VA */
    msr tcr_el2, x1
    dsb sy
    mrs x0, sctlr_el2
    orr x0, x0, #(1<<0) /* MMU control bit. */
    msr sctlr_el2, x0
    isb

    /* Switch to the kernel stack. */
    ldr x0, p_kernel_stack_top
    mov sp, x0

    /* Jump to the kernel entry point, passing the multiboot magic (boot
     * core), and the address of the multiboot header. */
    ldr x0, magic
    ldr x1, p_multiboot
    ldr x5, p_plat_init
    br x5

/* These pointers are filled in at load time. */
p_plat_init:        .word 0
p_multiboot:        .word 0
p_kernel_table:     .word 0
p_kernel_stack_top: .word 0

magic:
    .word MULTIBOOT2_BOOTLOADER_MAGIC

shim_stack:
    .space SHIM_STACK
