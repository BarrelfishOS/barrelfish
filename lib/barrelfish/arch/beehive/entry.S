/**
 * \file
 * \brief Dispatcher entry points.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__
#endif /* __ASSEMBLER__ */

#include <barrelfish/dispatch.h>
#include <regconvention.h>
#include <asmoffsets.h>

#ifndef OFFSETOF_DISP_PRIV_STACK_LIMIT
#error "No OFFSETOF_DISP_PRIV_STACK_LIMIT"
#endif
#ifndef OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT
#error "No OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT"
#endif

//
// void run_entry(struct disp_priv* p)
//
	.globl _run_entry
_run_entry:
	x_lli	OFFSETOF_DISP_PRIV_STACK_LIMIT
	add	sp, link, p1
	ld	a1, p1
	long_call _disp_run
	j	.

//
// void pagefault_entry(disp ptr, vaddr_t fault_addr, uintptr_t error, vaddr_t pc)
//
	.globl _pagefault_entry
_pagefault_entry:
	x_lli	OFFSETOF_DISP_PRIV_STACK_LIMIT
	add	sp, link, p1
	long_call _disp_pagefault
	j	.

//
// void disabled_pagefault_entry(disp ptr, vaddr_t fault_addr, uintptr_t error, vaddr_t pc)
//
	.globl _disabled_pagefault_entry
_disabled_pagefault_entry:
	x_lli	OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT
	add	sp, link, p1
	long_call _disp_pagefault_disabled
	j	.

//
// void trap_entry(disp ptr, uintptr_t irq, uintptr_t error, vaddr_t pc)
//
	.globl _trap_entry
_trap_entry:
	x_lli	OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT
	add	sp, link, p1
	long_call _disp_trap
	j	.
