/**
 * \file
 * \brief Kernel debugging helpers
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DEBUG_H
#define DEBUG_H

uintptr_t kernel_virt_to_elf_addr(void *addr);

#endif // DEBUG_H
