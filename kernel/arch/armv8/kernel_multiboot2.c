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


/*
 * Need to be initialized during kernel loading.
 */
struct arm_core_data *glbl_core_data = NULL;


// TODO: size is not really usable.
struct multiboot_header_tag *
multiboot2_find_header(struct multiboot_header_tag *mb, const size_t size, const multiboot_uint16_t type) {
    size_t count = 0;
    while (mb->type != MULTIBOOT_TAG_TYPE_END && mb->type != type && count < size) {
//        printf("find_header: count=%d size=%d type=%d size=%d\n", count, size, mb->type, mb->size);
        count += mb->size;
        mb = ((void*)mb) + mb->size;
    }
//    printf("find_header: found? count=%d size=%d type=%d size=%d\n", count, size, mb->type, mb->size);
    if (count < size && mb->type == type) {
        return mb;
    } else {
        return NULL;
    }
}

struct multiboot_tag_module_64 *multiboot2_find_module_64(
        struct multiboot_header_tag *multiboot, const size_t size, const char* pathname) {
    printf("%s: searching for module %s\n", __FUNCTION__, pathname);
    size_t len = strlen(pathname);
    multiboot = multiboot2_find_header(multiboot, size, MULTIBOOT_TAG_TYPE_MODULE_64);
    while (multiboot) {
        struct multiboot_tag_module_64 *module_64 = (struct multiboot_tag_module_64 *) multiboot;
        printf("  testing cmdline=%s\n", module_64->cmdline);
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
        multiboot = multiboot2_find_header(multiboot, size, MULTIBOOT_TAG_TYPE_MODULE_64);
    }
    return NULL;
}
