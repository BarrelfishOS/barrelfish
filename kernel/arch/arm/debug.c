/**
 * \file
 * \brief ARM-specific kernel debugging functions
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

uintptr_t kernel_virt_to_elf_addr(void *addr)
{
    return (uintptr_t)addr - (uintptr_t)&kernel_first_byte + 0x100000;
}
