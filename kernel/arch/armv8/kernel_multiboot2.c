/*
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <multiboot2.h>

#include <arch/armv8/kernel_multiboot2.h>


struct multiboot_header_tag *
multiboot2_find_header(struct multiboot_header_tag *mb, const size_t size, const multiboot_uint16_t type) {
    size_t processed = 0;

    while(processed < size) {
        /* encountered the end tag */
        if (mb->type == MULTIBOOT_TAG_TYPE_END) {
            return NULL;
        } else if (mb->type == type) {
            return mb;
        }

        processed += mb->size;
        mb = ((void*)mb) + mb->size;
    }

    return NULL;
}

struct multiboot_tag_string *
multiboot2_find_cmdline(struct multiboot_header_tag *mb, const size_t size)
{
    return (struct multiboot_tag_string*)multiboot2_find_header(mb, size, MULTIBOOT_TAG_TYPE_CMDLINE);
}

struct multiboot_tag_module_64 *multiboot2_find_module_64(
        struct multiboot_header_tag *multiboot, const size_t size, const char* pathname) {
    size_t len = strlen(pathname);
    size_t position = 0;
    multiboot = multiboot2_find_header(multiboot, size, MULTIBOOT_TAG_TYPE_MODULE_64);
    while (multiboot) {
        struct multiboot_tag_module_64 *module_64 = (struct multiboot_tag_module_64 *) multiboot;
        // Strip off trailing whitespace
        char *modname = module_64->cmdline;
        char *endstr;
        if(strchr(modname, ' ')) {
            endstr = strchr(modname, ' ');
        } else {
            endstr = modname + strlen(modname);
        }

        if(!strncmp(endstr - len, pathname, len)) {
            return module_64;
        }
        multiboot = ((void *) multiboot) + multiboot->size;
        position += multiboot->size;
        multiboot = multiboot2_find_header(multiboot, size - position, MULTIBOOT_TAG_TYPE_MODULE_64);
    }
    return NULL;
}
