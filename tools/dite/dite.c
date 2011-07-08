/**
 * \file
 * \brief Boot image generation tool
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
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

typedef uint8_t coreid_t;

#include "elf.h"
#include "../../kernel/include/arch/scc/diteinfo.h"

struct diteinfo cd;
uint32_t cdbase;

static genvaddr_t elf_virtual_base32(struct Elf32_Ehdr *ehead)
{
    struct Elf32_Phdr *phead =
        (struct Elf32_Phdr *)((uintptr_t)ehead + (uintptr_t)ehead->e_phoff);

    genvaddr_t retval = 0;
    int i;

    for (i = 0; i < ehead->e_phnum; i++) {
        struct Elf32_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {
            if(retval == 0) {
                retval = p->p_vaddr;
            }
            retval = p->p_vaddr < retval ? p->p_vaddr : retval;
        }
    }

    return retval;
}

static size_t elf_virtual_size32(struct Elf32_Ehdr *ehead)
{
    struct Elf32_Phdr *phead =
        (struct Elf32_Phdr *)((uintptr_t)ehead + (uintptr_t)ehead->e_phoff);

    size_t retval = 0;
    int i;

    for (i = 0; i < ehead->e_phnum; i++) {
        struct Elf32_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {
            retval = p->p_vaddr + p->p_memsz;
        }
    }

    return retval - elf_virtual_base32(ehead);
}

struct monitor_allocate_state {
    void          *vbase;
    genvaddr_t     elfbase;
};

static errval_t elf_allocate(void *state, genvaddr_t base,
                             size_t size, uint32_t flags,
                             void **retbase)
{
    struct monitor_allocate_state *s = state;

    *retbase = s->vbase + base - s->elfbase;
    return 0;
}

static void *preload_kernel(const char *filename, size_t *outsize)
{
    // Preload kernel image
    FILE *f = fopen(filename, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    size_t filesize = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *fbuf = malloc(filesize);
    fread(fbuf, filesize, 1, f);
    fclose(f);

    size_t size = elf_virtual_size32((void *)fbuf);
    char *kernelbuf = malloc(size);
    struct monitor_allocate_state state = {
        .vbase = kernelbuf,
        .elfbase = elf_virtual_base32((void *)fbuf)
    };

    genvaddr_t entry;
    errval_t err = elf32_load(elf_allocate, &state, (lvaddr_t)fbuf, filesize, &entry);
    if(err != 0) {
        printf("error!\n");
        exit(1);
    } else {
        printf("kernel entry point: 0x%" PRIx64 "\n", entry);
        printf("load image at: 0x%" PRIx64 "\n",
               state.elfbase - DITEINFO_SIZE);
    }

    cd.elf.size = sizeof(struct Elf32_Shdr);
    struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)fbuf;
    cd.elf.num  = head32->e_shnum;
    cd.elf.addr = state.elfbase - DITEINFO_SIZE +
        __builtin_offsetof(struct diteinfo, sh);
    cdbase = state.elfbase - DITEINFO_SIZE;

    size_t sh_size = cd.elf.size * cd.elf.num;
    if(sh_size < 2048) {
        memcpy(cd.sh, fbuf + (uintptr_t)head32->e_shoff, sh_size);
    } else {
        printf("section header too big (size %zd)\n", sh_size);
        exit(1);
    }

    free(fbuf);
    *outsize = size;
    return kernelbuf;
}

static void *load_module(const char *filename, size_t *outsize)
{
    // Read file
    FILE *mbf = fopen(filename, "r");
    if(mbf == NULL) {
        fprintf(stderr, "file %s not found\n", filename);
        exit(EXIT_FAILURE);
    }
    fseek(mbf, 0, SEEK_END);
    size_t fsize = ftell(mbf);
    fseek(mbf, 0, SEEK_SET);

    void *dstbuf = malloc(fsize);

    // Load multiboot module
    fread(dstbuf, fsize, 1, mbf);
    fclose(mbf);

    *outsize = fsize;
    return dstbuf;
}

#define MAX_MODULES     20

char *output = NULL;

int main(int argc, char *argv[])
{
    assert(sizeof(struct diteinfo) <= DITEINFO_SIZE);

    void *kernelbuf = NULL;

    if(argc < 2) {
        printf("Usage: %s <menu.lst>\n", argv[0]);
        return 0;
    }

    FILE *f = fopen(argv[1], "r");
    assert(f != NULL);
    char cmd[1024], args[1024], image[1024];
    void *module[MAX_MODULES];
    size_t sizemodule[MAX_MODULES], sizekernel, posmodule[MAX_MODULES];
    int modules = 0, mmaps = 0, fakemodules = 0;
    size_t bufsize = DITEINFO_SIZE;
    size_t fakebufsize = 0;

    while(!feof(f)) {
        char line[1024];

        cmd[0] = args[0] = image[0] = line[0] = '\0';

        fgets(line, 1024, f);
        sscanf(line, "%s %s %[^\n]", cmd, image, args);

        if(!strcmp(cmd, "kernel")) {
            kernelbuf = preload_kernel(image + 1, &sizekernel);
            bufsize += sizekernel;

            // Fill commandline args
            strcat(cd.strings, "\1");
            cd.cmdline = cdbase + __builtin_offsetof(struct diteinfo, strings) + strlen(cd.strings);
            strcat(cd.strings, image);
            strcat(cd.strings, " ");
            strcat(cd.strings, args);
        } else if(!strcmp(cmd, "module")) {
            assert(modules < MAX_MODULES);
            module[modules] = load_module(image + 1, &sizemodule[modules]);
            if(bufsize % 4096 != 0) {
                bufsize += 4096 - (bufsize % 4096);
            }
            posmodule[modules] = bufsize;
            bufsize += sizemodule[modules];

            struct diteinfo_modinfo *mi = &cd.modinfo[modules];
            mi->mod_start = posmodule[modules] + cdbase;
            mi->mod_end = bufsize - 1 + cdbase;

            // Fill commandline args
            strcat(cd.strings, "\1");
            mi->string = cdbase + __builtin_offsetof(struct diteinfo, strings) + strlen(cd.strings);
            strcat(cd.strings, image);
            if(strlen(args) > 0) {
                strcat(cd.strings, " ");
                strcat(cd.strings, args);
            }

            printf("Adding multiboot module '%s', args '%s' at [0x%x:0x%x]\n",
                   image, args, mi->mod_start, mi->mod_end);

            modules++;
        } else if(!strcmp(cmd, "fake")) {
            if(fakemodules == 0) {
                fakemodules = modules;
            }

            assert(fakemodules < MAX_MODULES);
            module[fakemodules] = load_module(image + 1, &sizemodule[fakemodules]);

            if(fakebufsize == 0) {
                fakebufsize = bufsize;
            }

            if(fakebufsize % 4096 != 0) {
                fakebufsize += 4096 - (fakebufsize % 4096);
            }

            posmodule[fakemodules] = fakebufsize;
            fakebufsize += sizemodule[fakemodules];

            struct diteinfo_modinfo *mi = &cd.modinfo[fakemodules];
            mi->mod_start = posmodule[fakemodules] + cdbase;
            mi->mod_end = fakebufsize - 1 + cdbase;

            // Fill commandline args
            strcat(cd.strings, "\1");
            mi->string = cdbase + __builtin_offsetof(struct diteinfo, strings) + strlen(cd.strings);
            strcat(cd.strings, image);
            if(strlen(args) > 0) {
                strcat(cd.strings, " ");
                strcat(cd.strings, args);
            }

            printf("Faking multiboot module '%s', args '%s' at [0x%x:0x%x]\n",
                   image, args, mi->mod_start, mi->mod_end);

            fakemodules++;
        } else if(!strcmp(cmd, "orig")) {
            unsigned int orig;
            sscanf(args, "%x", &orig);
            printf("New origin: 0x%x\n", orig);
            assert(bufsize + cdbase <= orig);
            bufsize = orig - cdbase;
        } else if(!strcmp(cmd, "mmap")) {
            if(mmaps >= 20) {
                printf("too many MMAPs\n");
                exit(1);
            }

            sscanf(args, "%" SCNi64 " %" SCNi64 " %i",
                   &cd.mmap[mmaps].base_addr,
                   &cd.mmap[mmaps].length,
                   &cd.mmap[mmaps].type);
            cd.mmap[mmaps].size = sizeof(struct diteinfo_mmap) - 4;

            printf("Inserting MMAP %d: [0x%" PRIx64 ", 0x%" PRIx64 "], type %d\n",
                   mmaps,
                   cd.mmap[mmaps].base_addr,
                   cd.mmap[mmaps].length,
                   cd.mmap[mmaps].type);

            mmaps++;
        } else if(!strcmp(cmd, "output")) {
            output = calloc(1, strlen(image) + 1);
            strcpy(output, image);
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

    fclose(f);

    // Catenate all images
    char *dstbuf = malloc(bufsize);
    char *pos = dstbuf + DITEINFO_SIZE;

    if(fakebufsize == 0) {
        fakebufsize = bufsize;
        fakemodules = modules;
    }

    // Setup rest of core_data
    struct diteinfo *ccd = (struct diteinfo *)dstbuf;
    /* cd.multiboot_flags = MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS | MULTIBOOT_INFO_FLAG_HAS_MEMINFO */
    /*     | MULTIBOOT_INFO_FLAG_HAS_CMDLINE | MULTIBOOT_INFO_FLAG_HAS_MODS | MULTIBOOT_INFO_FLAG_HAS_MMAP; */
    cd.mmap_addr = cdbase + __builtin_offsetof(struct diteinfo, mmap);
    cd.mmap_length = sizeof(struct diteinfo_mmap) * mmaps;
    cd.mods_count = fakemodules;
    cd.mods_addr = cdbase + __builtin_offsetof(struct diteinfo, modinfo);
    cd.start_free_ram = cdbase + fakebufsize;
    if(cd.start_free_ram & 4095) {
        cd.start_free_ram += 4096 - (cd.start_free_ram & 4095);
    }
    assert(strlen(cd.strings) < 1024);
    for(int i = 0; i < 1024; i++) {
        if(cd.strings[i] == '\1') {
            cd.strings[i] = '\0';
        }
    }
    *ccd = cd;

    // Copy kernel
    memcpy(pos, kernelbuf, sizekernel);

    printf("writing %d modules ...\n", modules);

    // Catenate all multiboot modules
    for(int i = 0; i < modules; i++) {
        pos = dstbuf + posmodule[i];
        memcpy(pos, module[i], sizemodule[i]);
    }

    if(output == NULL) {
        printf("No output image specified!\n");
        exit(1);
    }

    // Write output
    FILE *out = fopen(output, "w");
    assert(out != NULL);
    fwrite(dstbuf, bufsize, 1, f);
    fclose(out);

    free(dstbuf);
    return 0;
}
