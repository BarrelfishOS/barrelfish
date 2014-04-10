/* * Copyright (c) Intel Corporation (2011).
*
* Disclaimer: The codes contained in these modules may be specific to the
* Intel Software Development Platform codenamed: Knights Ferry, and the
* Intel product codenamed: Knights Corner, and are not backward compatible
* with other Intel products. Additionally, Intel will NOT support the codes
* or instruction set in future products.
*
* Intel offers no warranty of any kind regarding the code.  This code is
* licensed on an "AS IS" basis and Intel is not obligated to provide any support,
* assistance, installation, training, or other services of any kind.  Intel is
* also not obligated to provide any updates, enhancements or extensions.  Intel
* specifically disclaims any warranty of merchantability, non-infringement,
* fitness for any particular purpose, and any other warranty.
*
* Further, Intel disclaims all liability of any kind, including but not
* limited to liability for infringement of any proprietary rights, relating
* to the use of the code, even if Intel is notified of the possibility of
* such liability.  Except as expressly stated in an Intel license agreement
* provided with this code and agreed upon with Intel, no license, express
* or implied, by estoppel or otherwise, to any intellectual property rights
* is granted herein.
*/

/*
 *  linux/boot/head.S
 *
 *  Copyright (C) 1991, 1992, 1993  Linus Torvalds
 */

/*
 *  head.S contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00001000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory. [According to comments etc elsewhere on a compressed
 * kernel it will end up at 0x1000 + 1Mb I hope so as I assume this. - AC]
 *
 * Page 0 is deliberately kept safe, since System Management Mode code in
 * laptops may need to access the BIOS data stored there.  This is also
 * useful for future device drivers that either access the BIOS via VM86
 * mode.
 */

/*
 * High loaded stuff by Hans Lermen & Werner Almesberger, Feb. 1996
 */
#include <../kernel/include/target/k1om/offsets_target.h>

#define K1OM_BOOT_MAGIC         0xB001B001


#define CONFIG_RELOCATABLE 0


#define GDT_ENTRY_KERNEL_DS 3
#define GDT_ENTRY_KERNEL_CS 2
#define __KERNEL_DS (GDT_ENTRY_KERNEL_DS*8)
#define __KERNEL_CS (GDT_ENTRY_KERNEL_CS*8)
#define LONG_MODE_CS 0x0008
#define BOOT_STACK_SIZE       0x4000
#define X86_CR4_PAE 0x00000020 /* enable physical address extensions */
#define MSR_EFER              0xc0000080 /* extended feature register */
#define _EFER_LME             8  /* Long mode enable */
#define X86_CR0_PE  0x00000001 /* Protection Enable */
#define X86_CR0_PG  0x80000000 /* Paging */



#define BP_scratch 484 /* offsetof(struct boot_params, scratch) # */
#define BP_loadflags 529 /* offsetof(struct boot_params, hdr.loadflags) # */
#define BP_hardware_subarch 572 /* offsetof(struct boot_params, hdr.hardware_subarch)   # */
#define BP_version 518 /* offsetof(struct boot_params, hdr.version)     # */
#define BP_kernel_alignment 560 /* offsetof(struct boot_params, hdr.kernel_alignment)   # */
#define z_extract_offset 0
#define LOAD_PHYSICAL_ADDR K1OM_START_KERNEL_PHYS

#ifndef LINKER_SCRIPT
#define ALIGN __ALIGN
#define ALIGN_STR __ALIGN_STR

#ifndef ENTRY
#define ENTRY(name) \
  .globl name; \
  .align 4,0x90; \
  name:
#endif
#endif /* LINKER_SCRIPT */

#ifndef WEAK
#define WEAK(name)     \
          .weak name;            \
          name:
#endif

#ifndef END
#define END(name) \
  .size name, .-name
#endif

/* If symbol 'name' is treated as a subroutine (gets called, and returns)
 * then please use ENDPROC to mark 'name' as STT_FUNC for the benefit of
 * static analysis tools such as stack depth analyzer.
 */
#ifndef ENDPROC
#define ENDPROC(name) \
  .type name, @function; \
  END(name)
#endif


          .code32
          .text

          .section  ".head.text","ax"
          .code32
ENTRY(startup_32)
          cld
          /*
           * Test KEEP_SEGMENTS flag to see if the bootloader is asking
           * us to not reload segments
           */
          testb $(1<<6), BP_loadflags(%esi)
          jnz 1f

          cli
          movl      $(__KERNEL_DS), %eax
          movl      %eax, %ds
          movl      %eax, %es
          movl      %eax, %ss
1:

/*
 * Calculate the delta between where we were compiled to run
 * at and where we were actually loaded at.  This can only be done
 * with a short local call on x86.  Nothing  else will tell us what
 * address we are running at.  The reserved chunk of the real-mode
 * data at 0x1e4 (defined as a scratch field) are used as the stack
 * for this calculation. Only 4 bytes are needed.
 */
          leal      (BP_scratch+4)(%esi), %esp
          call      1f
1:        popl      %ebp
          subl      $1b, %ebp

/* setup a stack and make sure cpu supports long mode. */
          movl      $boot_stack_end, %eax
          addl      %ebp, %eax
          movl      %eax, %esp


/*
 * Compute the delta between where we were compiled to run at
 * and where the code will actually run at.
 *
 * %ebp contains the address we are loaded at by the boot loader and %ebx
 * contains the address where we should move the kernel image temporarily
 * for safe in-place decompression.
 */

          movl      $LOAD_PHYSICAL_ADDR, %ebx


                    /*
                    * Prepare for entering 64 bit mode
                    */

          /* Load new GDT with the 64bit segments using 32bit descriptor */
          leal      gdt(%ebp), %eax
          movl      %eax, gdt+2(%ebp)
          lgdt      gdt(%ebp)



                mov %cr4,%eax
                      orl $(X86_CR4_PAE),%eax
                      mov %eax,%cr4

 /*
  * Build early 4G boot pagetable
  */
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

    /* TO SUPPORT "Serial" out */

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
          addl      $4, %edi
          movl    $512, %ecx
1:        movl    $0x8, 0(%edi)
          addl    $8, %edi
          decl    %ecx
          jnz     1b


          /* Enable the boot page tables */
          leal      pgtable(%ebx), %eax
          movl      %eax, %cr3

          /* Enable Long mode in EFER (Extended Feature Enable Register) */
          movl      $MSR_EFER, %ecx
          rdmsr
          btsl      $_EFER_LME, %eax
          wrmsr

          /*
           * Setup for the jump to 64bit mode
           *
           * When the jump is performend we will be in long mode but
           * in 32bit compatibility mode with EFER.LME = 1, CS.L = 0, CS.D = 1
           * (and in turn EFER.LMA = 1).          To jump into 64bit mode we use
           * the new gdt/idt that has __KERNEL_CS with CS.L = 1.
           * We place all of the values on our mini stack so lret can
           * used to perform that far jump.
           */
          pushl     $__KERNEL_CS
          leal      startup_64(%ebp), %eax
          pushl     %eax

          /* Enter paged protected Mode, activating Long Mode */
          movl      $(X86_CR0_PG | X86_CR0_PE), %eax
          /* Enable Paging and Protected mode */
          movl      %eax, %cr0

          /* Jump from 32bit compatibility mode into 64bit mode. */
          lret

ENDPROC(startup_32)

no_longmode:
          /* This isn't an x86-64 CPU so hang */
1:
          hlt
          jmp     1b

          /*
           * Be careful here startup_64 needs to be at a predictable
           * address so I can export it in an ELF header.  Bootloaders
           * should look at the ELF header to find this address, as
           * it may change in the future.
           */
          .code64
          .org 0x200
ENTRY(startup_64)
          /*
           * We come here either from startup_32 or directly from a
           * 64bit bootloader.  If we come here from a bootloader we depend on
           * an identity mapped page table being provied that maps our
           * entire text+data+bss and hopefully all of memory.
           */

          /* Setup data segments. */
          xorl      %eax, %eax
          movl      %eax, %ds
          movl      %eax, %es
          movl      %eax, %ss
          movl      %eax, %fs
          movl      %eax, %gs
          lldt      %ax
          movl    $0x20, %eax
          ltr       %ax

                      /* a little "printf" to show that we are running */
                      movabsq $0x08007DAB5C, %r14
          movl    $0x0A58553E, (%r14)
          movabsq $0x08007DAB40, %r14
          movl    $2054847098, (%r14)

          /*
           * Compute the decompressed kernel start address.  It is where
           * we were loaded at aligned to a 2M boundary. %rbp contains the
           * decompressed kernel start address.
           *
           * If it is a relocatable kernel then decompress and run the kernel
           * from load address aligned to 2MB addr, otherwise decompress and
           * run the kernel from LOAD_PHYSICAL_ADDR
           *
           * We cannot rely on the calculation done in 32-bit mode, since we
           * may have been invoked via the 64-bit entry point.
           */

          /* Start with the delta to where the kernel will run at. */

          movq      $LOAD_PHYSICAL_ADDR, %rbp

          /* Target address to relocate to for decompression */
          //leaq      z_extract_offset(%rbp), %rbx
          movq                %rbp, %rbx

          /* Set up the stack */
       //   leaq      boot_stack_end(%rbx), %rsp

          /* Zero EFLAGS */
          pushq     $0
          popfq


#if 0
/*
 * Copy the compressed kernel to the end of our buffer
 * where decompression in place becomes safe.
 */
          pushq     %rsi
          leaq      (_bss-8)(%rip), %rsi    /* FROM */
          leaq      (_bss-8)(%rbx), %rdi          /* TO */
          movq      $_bss /* - $startup_32 */, %rcx /* UNTIL ZERO */
          shrq      $3, %rcx
          std
          rep       movsq
          cld
          popq      %rsi

#endif


/*
 * Jump to the relocated address.
 */
          leaq      relocated(%rbx), %rax
          jmp       *%rax

          .text
          .section  ".head64.text","ax"
relocated:


/*
 * Clear BSS (stack is currently empty)
 */
          xorl      %eax, %eax
          leaq    _bss(%rip), %rdi
          leaq    _ebss(%rip), %rcx
          subq      %rdi, %rcx
          shrq      $3, %rcx
          rep       stosq

          /* Reset EFLAGS */
          pushq     $0
          popf

		  /* store the boot info struct pointer and the magic value */
          movl      $K1OM_BOOT_MAGIC, %eax
          movl      %esi, %ebx
          call      loader

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
          .quad   0x0000000000000000    /* TS continued */
          .byte 0xff,0xff,0x00,0x60,0x00,0x9a,0xcf,0x00 // segment at linear address 0x6000
          .byte 0xff,0xff,0x00,0x00,0x00,0x92,0xaf,0x00 // stack segment in 64bit mode
gdt_end:


/*
 * Stack and heap for uncompression
 */
          .align 16
boot_stack:
          .fill BOOT_STACK_SIZE, 1, 0
boot_stack_end:

/*
 * Space for page tables (not in .bss so not zeroed)
 */
          .section ".pgtable","a",@nobits
          .balign 4096
pgtable:
          .fill 7*4096, 1, 0

