/**
 * \file
 * \brief Multiboot information creator
 *
 * This program creates a multiboot informaton structure based on the pre-
 * processed menu.lst file
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include "elf.h"

#define MAX_LINE_SIZE 2048
#define MAX_CMD_SIZE 64
#define MAX_ARGS_SIZE 1024
#define MAX_IMAGE_SIZE 256

#define MAX_MODULES 256

static char *kernel_cmd_line;
static unsigned int kernel_offset;
static unsigned int kernel_length;
static unsigned int module_offset[MAX_MODULES];
static unsigned int module_length[MAX_MODULES];
static char *module_cmd_line[MAX_MODULES];


int
main(int argc,
     char *argv[])
{
    int got_kernel = 0;
    int n_modules = 0;
    int n_mmaps = 0;

    if (argc < 3) {
        printf("Usage: %s <menu.lst> <output.c>\n", argv[0]);
        return 0;
    }

    FILE *f = fopen(argv[1], "r");
    assert((f != NULL) && "Could not open input file");

    FILE *o = fopen(argv[2], "w");
    assert((o != NULL) && "Could not open output file");

    // Output initial part of file: include basic types etc.
    fprintf(o, "#include <assert.h>\n");
    fprintf(o, "#include <stdio.h>\n");
    fprintf(o, "#include <stdint.h>\n");
    fprintf(o, "#include <stddef.h>\n");
    fprintf(o, "#include <string.h>\n");
    fprintf(o, "#include <barrelfish_kpi/types.h>\n");
    fprintf(o, "#include <errors/errno.h>\n");
    fprintf(o, "#include <elf/elf.h>\n");
    fprintf(o, "#include \"mbi.h\"\n");
    fprintf(o, "#include \"../kernel/include/multiboot.h\"\n");

    // Process menu.lst, generating definitions
    uint64_t offset;
    uint64_t length;

    char cmd[MAX_CMD_SIZE], args[MAX_ARGS_SIZE], image[MAX_IMAGE_SIZE];
    while (!feof(f)) {
        char line[MAX_LINE_SIZE];

        cmd[0] = args[0] = image[0] = line[0] = '\0';

        /*
         * input format
         *
         * CMD    IMAGE          OFFSET SIZE ARGS
         * kernel /k1om/sbin/cpu 0 1006840 loglevel=4
         *
         */
        fgets(line, MAX_LINE_SIZE, f);
        sscanf(line, "%s %s %lu %lu %[^\n]", cmd, image, &offset, &length, args);

        if (!strcmp(cmd, "kernel")) {
            printf("   +Kernel: %s, offset=0x%lx, length=0x%lx, args=%s\n",
                   image, offset, length, args);
            kernel_cmd_line = malloc(strlen(line) + 1);
            kernel_length = length;
            kernel_offset = offset;
            sprintf(kernel_cmd_line, "%s %s", image, args);
            got_kernel = 1;
        } else if (!strcmp(cmd, "module")) {
            assert(n_modules < MAX_MODULES);
            module_cmd_line[n_modules] = malloc(strlen(line) + 1);
            module_length[n_modules] = length;
            module_offset[n_modules] = offset;
            sprintf(module_cmd_line[n_modules], "%s %s", image, args);
            printf("   +Module [%d]: %s, offset=0x%lx, length=0x%lx, args=%s\n", n_modules,
                               image, offset, length, args);
            n_modules++;
        } else if (!strcmp(cmd, "mmap")) {
            //uint64_t base, len;
            int type;
            sscanf(line, "%s %s %lx %lx %i", cmd, image, &offset, &length, &type);
            sscanf(args, "%i", &type);
            printf("   +MMAP %d: [0x%" PRIx64 ", 0x%" PRIx64 "], type %d\n",
                   n_mmaps, offset, length, type);
            fprintf(o, "static uint64_t mbi_mmap%d[] = {0x%lx, 0x%lx, %d};\n",
                    n_mmaps, offset, length, type);
            n_mmaps++;
        } else {
            bool iscmd = false;
            for (int i = 0; i < strlen(cmd); i++) {
                if (cmd[i] == '#') {
                    break;
                }
                if (!isspace(cmd[i])) {
                    iscmd = true;
                    break;
                }
            }
            if (iscmd) {
                printf("   -Ignoring command '%s'\n", cmd);
            }
        }
    }

    // Generate multiboot-structure initialization code
    fprintf(o, "static struct multiboot_modinfo mbi_mods[%d];\n", n_modules + 1);
    fprintf(o, "static struct multiboot_mmap mbi_mmaps[%d];\n", n_mmaps);
    fprintf(o, "static struct multiboot_info mbi;\n\n");
    fprintf(o, "struct multiboot_info *get_multiboot(void) {\n");

    // Flags:
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_CMDLINE;\n");
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MODS;\n");
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;\n");
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MMAP;\n");

    // Kernel command line:
    fprintf(o, "  mbi.cmdline = (uint32_t)(uint64_t) \"%s\";\n", kernel_cmd_line);

    // Modules:
    fprintf(o, "  mbi.mods_count = %d;\n", n_modules + 1);
    fprintf(o, "  mbi.mods_addr = (uint32_t)(uint64_t) mbi_mods;\n");
    fprintf(o, "  mbi_mods[0].mod_start = (uint32_t) 0x%x;\n", kernel_offset);
    fprintf(o, "  mbi_mods[0].mod_end = (uint32_t) 0x%x;\n",
            kernel_offset + kernel_length);
    fprintf(o, "  mbi_mods[0].string = (uint32_t)(uint64_t) \"%s\";\n",
            kernel_cmd_line);

    for (int i = 0; i < n_modules; i++) {
        fprintf(o, "  mbi_mods[%d].mod_start = (uint32_t) 0x%x;\n", i + 1,
                module_offset[i]);
        fprintf(o, "  mbi_mods[%d].mod_end = (uint32_t) 0x%x;\n", i + 1,
                module_offset[i] + module_length[i]);
        fprintf(o, "  mbi_mods[%d].string = (uint32_t)(uint64_t) \"%s\";\n", i + 1,
                module_cmd_line[i]);
    }

    // MMAPS:
    fprintf(o, "  mbi.mmap_length = sizeof(mbi_mmaps);\n");
    fprintf(o, "  mbi.mmap_addr = (uint32_t)(uint64_t) mbi_mmaps;\n");
    for (int i = 0; i < n_mmaps; i++) {
        fprintf(o, "  mbi_mmaps[%d].size = sizeof(struct multiboot_mmap);\n", i);
        fprintf(o, "  mbi_mmaps[%d].base_addr = mbi_mmap%d[0];\n", i, i);
        fprintf(o, "  mbi_mmaps[%d].length = mbi_mmap%d[1];\n", i, i);
        fprintf(o, "  mbi_mmaps[%d].type = (int)mbi_mmap%d[2];\n", i, i);
    }
    fprintf(o, "  return &mbi;\n");
    fprintf(o, "}\n\n");

    fclose(f);
    fclose(o);

    return 0;
}
