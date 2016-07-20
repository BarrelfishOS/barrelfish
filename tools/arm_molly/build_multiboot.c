/*
 * M5 boot image tool.
 *
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


//XXX: hardcoded start address of ramdisk
//     has to match the address specified in tools/arm_gem5/gem5script.py
#define INITRD_BASE			0x10000000

#define MAX_MODULES 256

static char *kernel_symbol_prefix;
static char *kernel_cmd_line;
static char *module_symbol_prefix[MAX_MODULES];
static char *module_cmd_line[MAX_MODULES];


static char *get_symbol_name_prefix(char *original) {
    char *prefix = "_binary__";
    char *r = malloc(strlen(prefix) + strlen(original) + 1);
    sprintf(r, "%s%s", prefix, original);
    for (int i = 0; i < strlen(r); i ++) {
        if (r[i] == '/') {
            r[i] = '_';
        }
        if (r[i] == '-') {//needed for armv7-m
            r[i] = '_';
        }
    }
    return r;
}

int main(int argc, char *argv[])
{
    int n_modules = 0;
    int n_mmaps = 0;
    bool aarch64 = false;

    if(argc < 3 || (argc == 4 && strcmp("-64", argv[3])
                              && strcmp("-32", argv[3]))) {
        printf("Usage: %s <menu.lst> <output.c> [-64|-32]\n", argv[0]);
        return 0;
    }

    if(argc >= 4 && !strcmp("-64", argv[3])) aarch64 = true;

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
    fprintf(o, "#include <multiboot.h>\n");

    // Process menu.lst, generating definitions
    char cmd[1024], args[1024], image[1024];
    while(!feof(f)) {
        char line[1024], *l;

        cmd[0] = args[0] = image[0] = line[0] = '\0';

        l = fgets(line, 1024, f);
        if (!l) {
            /* error or EOF, check for EOF with next iteration */
            continue;
        }
        sscanf(line, "%s %s %[^\n]", cmd, image, args);

        if(!strcmp(cmd, "kernel")) {
            kernel_symbol_prefix = get_symbol_name_prefix(image);
            kernel_cmd_line = malloc(strlen(line) + 1);
            sprintf(kernel_cmd_line, "%s %s", image, args);
            fprintf(o, "extern char %s_start;\n", kernel_symbol_prefix);
            fprintf(o, "extern char %s_end;\n", kernel_symbol_prefix);
        } else if(!strcmp(cmd, "module") || !strcmp(cmd, "modulenounzip")) {
            assert(n_modules < MAX_MODULES);
            module_symbol_prefix[n_modules] = get_symbol_name_prefix(image);
            module_cmd_line[n_modules] = malloc(strlen(line) + 1);
            sprintf(module_cmd_line[n_modules], "%s %s", image, args);
            fprintf(o, "extern char %s_start;\n", module_symbol_prefix[n_modules]);
            fprintf(o, "extern char %s_end;\n", module_symbol_prefix[n_modules]);
            n_modules ++;
        } else if(!strcmp(cmd, "mmap")) {
            if(aarch64) {
                uint64_t base, len;
                int type;
                sscanf(args, "%" SCNi64 " %" SCNi64 " %i",
                        &base, &len, &type);
                printf("Inserting MMAP %d: [0x%" PRIx64 ", 0x%" PRIx64 "], type %d\n",
                        n_mmaps, base, len, type);
                fprintf(o, "static uint64_t mbi_mmap%d[] = {0x%" PRIx64
                           ", 0x%" PRIx64 ", %d};\n",
                        n_mmaps, base, len, type);
            }
            else {
                uint32_t base, len;
                int type;
                sscanf(args, "%" SCNi32 " %" SCNi32 " %i",
                        &base, &len, &type);
                printf("Inserting MMAP %d: [0x%" PRIx32 ", 0x%" PRIx32 "], type %d\n",
                        n_mmaps, base, len, type);
                fprintf(o, "static uint32_t mbi_mmap%d[] = {0x%" PRIx32
                           ", 0x%" PRIx32 ", %d};\n",
                        n_mmaps, base, len, type);
            }
            n_mmaps ++;
        } else {
            bool iscmd = false;
            for(int i = 0; i < strlen(cmd); i++) {
                if(cmd[i] == '#') {
                    break;
                }
                if(!isspace(cmd[i])) {
                    iscmd = true;
                    break;
                }
            }
            if(iscmd) {
                printf("Ignoring command '%s'\n", cmd);
            }
        }
    }

    // Generate multiboot-structure initialization code
    fprintf(o, "static struct multiboot_modinfo mbi_mods[%d];\n", n_modules + 1);
    fprintf(o, "static struct multiboot_mmap mbi_mmaps[%d];\n", n_mmaps);
    fprintf(o, "static struct multiboot_info mbi;\n\n");
    fprintf(o, "struct multiboot_info *molly_get_mbi(void) {\n");

    // Initialize all static data structures
    fprintf(o, "memset(&mbi, 0, sizeof(struct multiboot_info));\n");
    fprintf(o, "memset(&mbi_mods, 0, sizeof(mbi_mods));\n");
    fprintf(o, "memset(&mbi_mmaps, 0, sizeof(mbi_mmaps));\n");

    // Flags:
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_CMDLINE;\n");
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MODS;\n");
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;\n");
    fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MMAP;\n");

    // Kernel command line:
    if(aarch64)
        fprintf(o, "  mbi.cmdline = (uint64_t) \"%s\";\n", kernel_cmd_line);
    else
        fprintf(o, "  mbi.cmdline = (uint32_t) \"%s\";\n", kernel_cmd_line);

    // Modules:
    fprintf(o, "  mbi.mods_count = %d;\n", n_modules + 1);
    if(aarch64) {
        fprintf(o, "  mbi.mods_addr = (uint64_t) mbi_mods;\n");
        fprintf(o, "  mbi_mods[0].mod_start = (uint64_t) &%s_start;\n",
                kernel_symbol_prefix);
        fprintf(o, "  mbi_mods[0].mod_end = (uint64_t) &%s_end;\n",
                kernel_symbol_prefix);
        fprintf(o, "  mbi_mods[0].string = (uint64_t)\"%s\";\n",
                kernel_cmd_line);

        for (int i = 0; i < n_modules; i ++) {
            fprintf(o, "  mbi_mods[%d].mod_start = (uint64_t) &%s_start;\n",
                    i+1, module_symbol_prefix[i]);
            fprintf(o, "  mbi_mods[%d].mod_end = (uint64_t) &%s_end;\n",
                    i+1, module_symbol_prefix[i]);
            fprintf(o, "  mbi_mods[%d].string = (uint64_t) \"%s\";\n",
                    i+1, module_cmd_line[i]);
        }
    } else {
        fprintf(o, "  mbi.mods_addr = (uint32_t) mbi_mods;\n");
        fprintf(o, "  mbi_mods[0].mod_start = (uint32_t) &%s_start;\n",
                kernel_symbol_prefix);
        fprintf(o, "  mbi_mods[0].mod_end = (uint32_t) &%s_end;\n",
                kernel_symbol_prefix);
        fprintf(o, "  mbi_mods[0].string = (uint32_t)\"%s\";\n",
                kernel_cmd_line);

        for (int i = 0; i < n_modules; i ++) {
            fprintf(o, "  mbi_mods[%d].mod_start = (uint32_t) &%s_start;\n",
                    i+1, module_symbol_prefix[i]);
            fprintf(o, "  mbi_mods[%d].mod_end = (uint32_t) &%s_end;\n",
                    i+1, module_symbol_prefix[i]);
            fprintf(o, "  mbi_mods[%d].string = (uint32_t) \"%s\";\n",
                    i+1, module_cmd_line[i]);
        }
    }

    // MMAPS:
    fprintf(o, "  mbi.mmap_length = sizeof(mbi_mmaps);\n");
    if(aarch64)
        fprintf(o, "  mbi.mmap_addr = (uint64_t) mbi_mmaps;\n");
    else
        fprintf(o, "  mbi.mmap_addr = (uint32_t) mbi_mmaps;\n");
    for (int i = 0; i < n_mmaps; i ++) {
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
