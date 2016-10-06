/*
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef KERNEL_ARMV8_MULTIBOOT_UTILITIES_H_
#define KERNEL_ARMV8_MULTIBOOT_UTILITIES_H_

#include <multiboot2.h>

struct multiboot_header_tag *
multiboot2_find_header(struct multiboot_header_tag *mb, const size_t size,
                       const multiboot_uint16_t type);

struct multiboot_tag_string *
multiboot2_find_cmdline(struct multiboot_header_tag *mb, const size_t size);

struct multiboot_tag_module_64 *
multiboot2_find_module_64(struct multiboot_header_tag *multiboot,
                          const size_t size, const char* name);


#endif /* KERNEL_ARMV8_MULTIBOOT_UTILITIES_H_ */
