/**
 * \file
 * \brief Kernel debugging functions
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_DEBUG_H
#define KERNEL_DEBUG_H

#include <elf/elf.h>

void debug_vaddr_identify(lvaddr_t pml4, lvaddr_t vaddr);

void debug_sort_dynsyms (struct Elf64_Sym *dynsyms, int n);
void debug_setup_stackwalker (uint64_t stack_top, uint64_t stack_bottom, uint64_t text_start, uint64_t text_end, struct Elf64_Sym *dynsyms, char *dynstr, int nsyms);
void debug_relocate_dynsyms (struct Elf64_Sym *dynsyms, int n, uint64_t offset);
void dump_stack (void);

/* Citing the QEMU manpage, circa 2016:
 * -debugcon dev
 *
 *   Redirect the debug console to host device dev (same devices as the serial
 *   port).  The debug console is an I/O port which is typically port 0xe9;
 *   writing to that I/O port sends output to this device.  The default device is
 *   "vc" in graphical mode and "stdio" in non graphical mode.
 */
static inline void __attribute__ ((always_inline)) qemu_debug_outb (char c)
{
    int qemu_debug_port = 0xe9;
    __asm__ __volatile__("outb %0,%%dx" : : "a" (c), "d" (qemu_debug_port));
}

static inline void __attribute__ ((always_inline)) qemu_debug_puts (char *s)
{
    char c; int i;
    for (i = 0; (c = s[i]); i++)
	qemu_debug_outb (c);
}

#endif //KERNEL_DEBUG_H
