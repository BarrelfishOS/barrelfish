/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdbool.h>
#include <init.h>

extern void dbg_break(void);
void dbg_break(void)
{
#ifndef __thumb__ 
    __asm("bkpt #0xffff");
#else
    //heteropanda: smaller breakpoint immediate in pure thumb2
    __asm("bkpt #0xff");
#endif
}

extern void arch_benchmarks(void);
void arch_benchmarks(void) { dbg_break(); }

extern void arch_benchmarks_size(void);
void arch_benchmarks_size(void) { dbg_break(); }

extern void conio_putchar(void);
void conio_putchar(void) { /* Don't break here yet! */ }

extern void gdb_arch_continue(void);
void gdb_arch_continue(void) { dbg_break(); }

extern void gdb_arch_get_register(void);
void gdb_arch_get_register(void) { dbg_break(); }

extern void gdb_arch_read_byte(void);
void gdb_arch_read_byte(void) { dbg_break(); }

extern void gdb_arch_registers(void);
void gdb_arch_registers(void) { dbg_break(); }

extern void gdb_arch_set_register(void);
void gdb_arch_set_register(void) { dbg_break(); }

extern void gdb_arch_single_step(void);
void gdb_arch_single_step(void) { dbg_break(); }

extern void gdb_arch_write_byte(void);
void gdb_arch_write_byte(void) { dbg_break(); }

extern void reboot(void);
void reboot(void) { dbg_break(); }

struct dcb;
extern void __attribute__ ((noreturn)) vmkit_vmenter (struct dcb *dcb);
void vmkit_vmenter(struct dcb *dcb) { dbg_break(); for(;;); }

extern void __aeabi_unwind_cpp_pr0(void);
void __aeabi_unwind_cpp_pr0(void) { dbg_break(); }

extern void raise(void);
void raise(void) { dbg_break(); }

void breakpoint(void) { dbg_break(); }

bool arch_core_is_bsp(void) { return cpu_is_bsp(); }
