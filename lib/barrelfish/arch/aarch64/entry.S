/**
 * \file
 * \brief Dispatcher entry points.
 */

/*
 * Copyright (c) 2007-2009, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__ // GCC Bug c/25993
#endif /* __ASSEMBLER__ */
        
#include <barrelfish/dispatch.h>
#include <asmoffsets.h>

.text
.extern disp_run, disp_pagefault, disp_pagefault_disabled, disp_trap
.globl run_entry, pagefault_entry, disabled_pagefault_entry, trap_entry

.macro init_sp offset
    /* Offset to the top of the required stack. */
    ldr x10, =(OFFSETOF_DISP_GENERIC + \offset)
    add sp, x0, x10
.endm

/* void run_entry(struct disp_priv* p) */
run_entry:
    init_sp OFFSETOF_DISP_PRIV_STACK_LIMIT
    b disp_run

/* 
 * void pagefault_entry(struct disp_priv *p, vaddr_t fault_addr,
 *                      uintptr_t error, vaddr_t pc)
 */
pagefault_entry:
    init_sp OFFSETOF_DISP_PRIV_STACK_LIMIT
    b disp_pagefault

/*
 * void disabled_pagefault_entry(struct disp_priv *p, vaddr_t fault_addr,
 *                               uintptr_t error, vaddr_t pc)
 */
disabled_pagefault_entry:
    init_sp OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT
    b disp_pagefault_disabled

/*
 * void trap_entry(struct disp_priv *p, uintptr_t irq,
 *                 uintptr_t error, vaddr_t pc)
 */
trap_entry:
    init_sp OFFSETOF_DISP_PRIV_TRAP_STACK_LIMIT
    b disp_trap
