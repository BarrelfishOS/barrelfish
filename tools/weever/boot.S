/**
 * \file
 * \brief X-Loader for the Intel Xeon Phi
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

 /*
  * Parts of this code comes from the Linux distribution shipped with the
  * MPSS 3.2 for the Xeon Phi.
  */

#include <../kernel/include/target/k1om/offsets_target.h>

/* Boot Magic */
#define K1OM_BOOT_MAGIC               0xB001B001

/* The long mode code segment */
#define LONG_MODE_CS                  0x0010

/* the boot stack size (16 kb)*/
#define BOOT_STACK_SIZE               0x4000

/* X86 Registers */
#define MSR_EFER                      0xc0000080
#define X86_EFER_LONG_MODE_ENABLE     8
#define X86_CR0_PROTECTION_ENABLE     0x00000001
#define X86_CR0_PAGING_ENABLE         0x80000000
#define X86_CR4_PADDREXT_ENABLE       0x00000020

/*
 * scratch field in struct boot_params supplied by the bootloader
 * this is used for a mini stack to determine the load address
 */
#define BP_SCRATCH                     484


#define ENTRY(name) \
  .globl name; \
  .align 4,0x90; \
  name:

#ifndef ENDPROC
#define ENDPROC(name) \
  .type name, @function; \
  .size name, .-name
#endif


/*
 * ===========================================================================
 * Begin of 32 bit entry code
 * ===========================================================================
 */

          .section  ".head.text","ax"
          .code32
ENTRY(startup_32)

          /* clear direction and interrupts */
          cld
          cli

          leal      (BP_SCRATCH+4)(%esi), %esp
          call      1f
1:        popl      %ebp
          subl      $1b, %ebp
          movl      %ebp, %ebx

		  leal      prepare_long_mode(%ebx), %eax
		  jmp		*%eax
ENDPROC(startup_32)

          /* Prepare for the long mode */
.section  ".head32.text","ax"

ENTRY(prepare_long_mode)
          /* Load new GDT with the 64bit segments using 32bit descriptor */
          leal      gdt(%ebx), %eax
          movl      %eax, gdt+2(%ebx)
          lgdt      gdt(%ebx)

          /* Enable physical address extension */
          mov       %cr4,%eax
          orl       $X86_CR4_PADDREXT_ENABLE,%eax
          mov       %eax,%cr4

         /* Building the boot page tables for long mode */

          /* Initialize Page tables to 0 */
          leal      pgtable(%ebx), %edi
          xorl      %eax, %eax
          movl      $((4096*6)/4), %ecx
          rep       stosl

          /* Build Level 4 */
          leal      pgtable + 0(%ebx), %edi
          leal      0x1007 (%edi), %eax
          movl      %eax, 0(%edi)

          /* Build Level 3 */
          leal      pgtable + 0x1000(%ebx), %edi
          leal      0x1007(%edi), %eax
          movl      $4, %ecx
1:        movl      %eax, 0x00(%edi)
          addl      $0x00001000, %eax
          addl      $8, %edi
          decl      %ecx
          jnz       1b

          /* Build Level 2 */
          leal      pgtable + 0x2000(%ebx), %edi
          movl      $0x00000183, %eax
          movl      $2048, %ecx
1:        movl      %eax, 0(%edi)
          addl      $0x00200000, %eax
          addl      $8, %edi
          decl      %ecx
          jnz       1b

          /* Adding mappings for the "serial out" */

          /* Build Level 3 */
          leal      pgtable + 0x1000(%ebx), %edi
          leal      0x5007(%edi), %eax
          addl      $256, %edi                    //31st entry
          movl      %eax, 0x00(%edi)

          /* Build Level 2 */
          leal      pgtable + 0x6000(%ebx), %edi
          movl      $0x00000183, %eax
          movl      $512, %ecx
1:        movl      %eax, 0(%edi)
          addl      $0x00200000, %eax
          addl      $8, %edi
          decl      %ecx
          jnz       1b

          /* Build Level 2 - high part*/
          leal    pgtable + 0x6000(%ebx), %edi
          addl    $4, %edi
          movl    $512, %ecx
1:        movl    $0x8, 0(%edi)
          addl    $8, %edi
          decl    %ecx
          jnz     1b

          /* Enable the boot page tables */
          leal      pgtable(%ebx), %eax
          movl      %eax, %cr3

          /* Enable the long mode */
          movl      $MSR_EFER, %ecx
          rdmsr
          btsl      $X86_EFER_LONG_MODE_ENABLE, %eax
          wrmsr

            /* prepare the long mode jump */
          pushl     $LONG_MODE_CS
          leal      startup_64(%ebx), %eax
          pushl     %eax

          /* Activate protected mode and paging*/
          movl        %cr0, %eax
          orl      $(X86_CR0_PAGING_ENABLE | X86_CR0_PROTECTION_ENABLE), %eax
          movl      %eax, %cr0

          /* Jump from 32bit compatibility mode into 64bit mode. */
          lret

ENDPROC(prepare_long_mode)

/*
 * ===========================================================================
 * Begin of 64 bit code
 * ===========================================================================
 */
 	.section  ".head64.text","ax"
          .code64
ENTRY(startup_64)

          /* Setup data segments. */
          xorl      %eax, %eax
          movl      %eax, %ds
          movl      %eax, %es
          movl      %eax, %ss
          movl      %eax, %fs
          movl      %eax, %gs
          lldt      %ax
          movl      $0x20, %eax
          ltr       %ax

          /* a little "printf" to show that we are running */
          movabsq $0x08007DAB5C, %r14
          movl    $0x0A58553E, (%r14)
          movabsq $0x08007DAB40, %r14
          movl    $2054847098, (%r14)


          /* Set up the stack */
          leaq      boot_stack_end(%rbx), %rsp


          /* Reset EFLAGS */
          pushq     $0
          popf

          /*
           * store the boot param struct pointer and the magic value
           * Note: %rsi holds already a pointer to the boot params
           */
          movq      $K1OM_BOOT_MAGIC, %rdi
          call      loader

           /* jump to the kernel entry point */
          movq      $K1OM_BOOT_MAGIC, %rax
          movq      multiboot_info, %rbx
          movq      kernel_entry, %rcx
          jmp       *%rcx

          .data
          .align 16
gdt:
          .word     gdt_end - gdt
          .long     gdt
          .word     0
          .quad     0x0000000000000000  /* NULL descriptor */
          .quad     0x00af9a000000ffff  /* __KERNEL_CS */
          .quad     0x00cf92000000ffff  /* __KERNEL_DS */
          .quad     0x0080890000000000  /* TS descriptor */
          .quad     0x0000000000000000    /* TS continued */
gdt_end:

          .align 16
boot_stack:
          .fill BOOT_STACK_SIZE, 1, 0
boot_stack_end:

          .section ".pgtable","a",@nobits
          .align 4096
pgtable:
          .fill 7*4096, 1, 0


