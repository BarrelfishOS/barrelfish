/**
 * \file acpi_allocators.h
 * \brief 
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USR_ACPI_ACPI_ALLOCATORS_H_
#define USR_ACPI_ACPI_ALLOCATORS_H_ 1

errval_t acpi_allocators_init(void);
errval_t acpi_allocators_init_arch(struct bootinfo *bootinfo);

#endif /* USR_ACPI_ACPI_ALLOCATORS_H_ */
