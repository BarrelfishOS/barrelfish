/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdbool.h>
#include <init.h>

extern void dbg_break(void);

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
