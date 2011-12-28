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

#include <regconvention.h>
#include <beej7.h>
#include <barrelfish_kpi/syscalls.h>
#include <asmoffsets.h>

/*
 * extern errval_t sys_yield(capaddr_t target);
 */
	.globl _sys_yield
_sys_yield:
	ld	t2, link
	j7	BEE_J7_COREAREA
	aqr_add	vb, link, OFFSETOF_COREAREA_SYSCALL
	ld	link, t2	
	ld	t1, SYSCALL_YIELD
	j	rq

/*
 * extern errval_t sys_nop(void);
 */
	.globl _sys_nop
_sys_nop:
	ld	t2, link
	j7	BEE_J7_COREAREA
	aqr_add	vb, link, OFFSETOF_COREAREA_SYSCALL
	ld	link, t2	
	ld	t1, SYSCALL_NOP
	j	rq

/*
 * extern errval_t sys_print(const char *string, size_t length);
 */
	.globl _sys_print
_sys_print:
	ld	t2, link
	j7	BEE_J7_COREAREA
	aqr_add	vb, link, OFFSETOF_COREAREA_SYSCALL
	ld	link, t2	
	ld	t1, SYSCALL_PRINT
	j	rq

/*
 * extern errval_t sys_invoke(uintptr_t *const pvalue,
 *	uint8_t invokebits,
 *	capaddr_t invokecptr,
 *	struct idc_send_msg *const msg);
 */
	.globl _sys_invoke
_sys_invoke:
	ld	t2, link
	j7	BEE_J7_COREAREA
	aqr_add	vb, link, OFFSETOF_COREAREA_SYSCALL
	ld	link, t2	
	ld	t1, SYSCALL_INVOKE
	j	rq

/*
 * extern errval_r sys_rundown(void);
 */
	.globl _sys_rundown
_sys_rundown:
	ld	t2, link
	j7	BEE_J7_COREAREA
	aqr_add	vb, link, OFFSETOF_COREAREA_SYSCALL
	ld	link, t2	
	ld	t1, SYSCALL_BEEHIVE_TRACE_RUNDOWN
	j	rq

/* End */
