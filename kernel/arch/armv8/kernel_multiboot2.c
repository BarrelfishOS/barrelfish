/*
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <multiboot2.h>

#include <arch/armv8/kernel_multiboot2.h>


// Dump the Multiboot2 structure
void multiboot2_dump(struct multiboot_tag *first_tag,
                               const size_t size)
{
    size_t processed = 0;
    char description[256];

    printf("%s: %p:%ld\n", __func__, first_tag, size);
    while (processed < size) {
        switch (first_tag->type) {
            case MULTIBOOT_TAG_TYPE_CMDLINE: {
                snprintf(description, sizeof(description), "CMDLINE: [%s]",
                    ((struct multiboot_tag_string *)first_tag)->string);
            } break;
            case MULTIBOOT_TAG_TYPE_EFI64: {
                snprintf(description, sizeof(description), "EFI64: pointer=%lx",
                    ((struct multiboot_tag_efi64 *)first_tag)->pointer);
            } break;
            case MULTIBOOT_TAG_TYPE_ACPI_NEW: {
                snprintf(description, sizeof(description), "ACPI new:");
            } break;
            case MULTIBOOT_TAG_TYPE_EFI_MMAP: {
                snprintf(description, sizeof(description), "EFI MMAP: size=%d version=%d",
                    ((struct multiboot_tag_efi_mmap *)first_tag)->descr_size,
                    ((struct multiboot_tag_efi_mmap *)first_tag)->descr_vers);
            } break;
            case MULTIBOOT_TAG_TYPE_MODULE_64: {
                snprintf(description, sizeof(description), "MODULE 64: mod_start=%lx  mod_end=%lx cmdline=[%s]",
                    ((struct multiboot_tag_module_64 *)first_tag)->mod_start,
                    ((struct multiboot_tag_module_64 *)first_tag)->mod_end,
                    ((struct multiboot_tag_module_64 *)first_tag)->cmdline);
            } break;
            case MULTIBOOT_TAG_TYPE_END: {
                snprintf(description, sizeof(description), "END");
            } break;
            default: {
                snprintf(description, sizeof(description), "[%d]", first_tag->type);
            } break;
        }
        printf("%p:%4ld:  tag %d:%d  %s\n", first_tag, processed, first_tag->type, first_tag->size, description);
        if (first_tag->type == MULTIBOOT_TAG_TYPE_END)
            break;
        processed += first_tag->size;
        first_tag = (void *)first_tag + first_tag->size;
    }
}

struct multiboot_tag * multiboot2_find_tag(struct multiboot_tag *first_tag,
                               const size_t size, const multiboot_uint16_t type)
{
    size_t processed = 0;

    while (processed < size) {
        /* encountered the end tag */
        if (first_tag->type == MULTIBOOT_TAG_TYPE_END) {
            return NULL;
        } else if (first_tag->type == type) {
            return first_tag;
        }
        processed += first_tag->size;
        first_tag = (void *)first_tag + first_tag->size;
    }

    return NULL;
}

struct multiboot_tag_string * multiboot2_find_cmdline(
                             struct multiboot_tag *first_tag, const size_t size)
{
    return (struct multiboot_tag_string *)multiboot2_find_tag(first_tag, size,
        MULTIBOOT_TAG_TYPE_CMDLINE);
}

struct multiboot_tag_module_64 * multiboot2_find_module_64(
       struct multiboot_tag *first_tag, const size_t size, const char *pathname)
{
    size_t len = strlen(pathname);
    size_t position = 0;
    struct multiboot_tag *tag;

    tag = multiboot2_find_tag(first_tag, size, MULTIBOOT_TAG_TYPE_MODULE_64);
    while (tag) {
        struct multiboot_tag_module_64 *module_64 = (struct multiboot_tag_module_64 *)tag;
        // Strip off trailing whitespace
        char *modname = module_64->cmdline;
        char *endstr;
        endstr = strchr(modname, ' ');
        if (!endstr) {
            endstr = modname + strlen(modname);
        }

        if(!strncmp(endstr - len, pathname, len)) {
            return module_64;
        }
        tag = (void *)tag + tag->size;
        position += tag->size;
        tag = multiboot2_find_tag(tag, size - position, MULTIBOOT_TAG_TYPE_MODULE_64);
    }
    return NULL;
}
