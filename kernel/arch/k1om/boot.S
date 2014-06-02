/**
 * \file
 * \brief Bootstrap the kernel.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <multiboot.h>
#include <target/k1om/offsets_target.h>

/* The flags for the Multiboot header */
#define MB_FLAGS (MULTIBOOT_HEADER_FLAG_MODS_PGALIGNED | MULTIBOOT_HEADER_FLAG_NEED_MEMINFO)

          .text
          .globl    kernel_start, halt
          /* Multiboot header, 4-byte aligned */
          .align    4
          .long     MULTIBOOT_HEADER_MAGIC               /* magic */
          .long     MB_FLAGS                             /* flags */
          .long     -(MULTIBOOT_HEADER_MAGIC + MB_FLAGS) /* checksum */

kernel_start:
          /* Initialize the stack pointer */
          lea       (k1om_kernel_stack + K1OM_KERNEL_STACK_SIZE)(%rip), %rsp

          /* Reset EFLAGS */
          pushq     $0
          popf

          /* Enter architecture-specific init -- this should never return */
          movq      %rax, %rdi          /* Multiboot magic value */
          movq      %rbx, %rsi          /* Pointer to multiboot info struct */
          call      arch_init

          /* Halt -- this should never be reached */
halt:     hlt
          jmp       halt
