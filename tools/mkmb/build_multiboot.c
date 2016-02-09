#include <libelf.h> /* FreeBSD libelf */
#include <multiboot2.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "util.h"

void *
create_multiboot2_info(struct config *cfg, Elf *elf, size_t mmap_size) {
    size_t size;
    struct component_config *cmp;
    void *cursor;

    size_t shnum;
    if(elf_getshdrnum(elf, &shnum)) {
        fprintf(stderr, "elf_getshdrnum: %s\n",
                elf_errmsg(elf_errno()));
        return NULL;
    }

    /* Calculate the boot information size. */
    /* Fixed header - there's no struct for this in multiboot.h */
    size= 8;
    /* Kernel command line */
    size+= sizeof(struct multiboot_tag_string)
         + cfg->kernel->args_len+1;
    /* ELF section headers */
    size+= sizeof(struct multiboot_tag_elf_sections)
         + shnum * sizeof(Elf64_Shdr);
    /* Kernel module tag, including command line. */
    size+= sizeof(struct multiboot_tag_module_64)
         + cfg->kernel->args_len+1;
    /* All other modules */
    for(cmp= cfg->first_module; cmp; cmp= cmp->next) {
        size+= sizeof(struct multiboot_tag_module_64)
             + cmp->args_len+1;
    }
    /* EFI memory map */
    size+= sizeof(struct multiboot_tag_efi_mmap)
         + mmap_size;

    cfg->multiboot_size= size;
    cfg->multiboot_alloc= ROUNDUP(size, PAGE_4k);

    cfg->multiboot= calloc(1, cfg->multiboot_alloc);
    if(!cfg->multiboot) {
        perror("calloc");
        return NULL;
    }

    cursor= cfg->multiboot;
    /* Write the fixed header. */
    *((uint32_t *)cursor)= size; /* total_size */
    cursor+= sizeof(uint32_t);
    *((uint32_t *)cursor)= 0;    /* reserved */
    cursor+= sizeof(uint32_t);

    /* Add the boot command line */
    {
        struct multiboot_tag_string *bootcmd=
            (struct multiboot_tag_string *)cursor;

        bootcmd->type= MULTIBOOT_TAG_TYPE_CMDLINE;
        bootcmd->size= sizeof(struct multiboot_tag_string)
                     + cfg->kernel->args_len+1;
        ntstring(bootcmd->string,
                 cfg->buf + cfg->kernel->args_start,
                 cfg->kernel->args_len);

        cursor+= sizeof(struct multiboot_tag_string)
               + cfg->kernel->args_len+1;
    }
    /* Add the ELF section headers. */
    {
        struct multiboot_tag_elf_sections *sections=
            (struct multiboot_tag_elf_sections *)cursor;

        size_t shndx;
        if(elf_getshdrstrndx(elf, &shndx)) {
            fprintf(stderr, "elf_getshdrstrndx: %s\n",
                    elf_errmsg(elf_errno()));
            return NULL;
        }

        sections->type= MULTIBOOT_TAG_TYPE_ELF_SECTIONS;
        sections->size= sizeof(struct multiboot_tag_elf_sections)
                 + shnum * sizeof(Elf64_Shdr); 
        sections->num= shnum;
        sections->entsize= sizeof(Elf64_Shdr);
        sections->shndx= shndx;

        Elf64_Shdr *shdrs= (Elf64_Shdr *)sections->sections;
        for(size_t i= 0; i < shnum; i++) {
            Elf_Scn *scn= elf_getscn(elf, i);
            if(!scn) {
                fprintf(stderr, "elf_getscn: %s\n",
                        elf_errmsg(elf_errno()));
                return NULL;
            }

            Elf64_Shdr *shdr= elf64_getshdr(scn);
            if(!shdr) {
                fprintf(stderr, "elf64_getshdr: %s\n",
                        elf_errmsg(elf_errno()));
                return NULL;
            }

            memcpy(&shdrs[i], shdr, sizeof(Elf64_Shdr));
        }

        cursor+= sizeof(struct multiboot_tag_elf_sections)
               + shnum * sizeof(Elf64_Shdr);
    }
    /* Add the kernel module. */
    {
        struct multiboot_tag_module_64 *kernel=
            (struct multiboot_tag_module_64 *)cursor;

        kernel->type= MULTIBOOT_TAG_TYPE_MODULE_64;
        kernel->size= sizeof(struct multiboot_tag_module_64)
                    + cfg->kernel->args_len+1;
        /* Leave the addresses uninitialised until we've finished allocation,
         * which needs the multboot image constructed first. */
        ntstring(kernel->cmdline,
                 cfg->buf + cfg->kernel->args_start,
                 cfg->kernel->args_len);

        cfg->kernel->tag= kernel;

        cursor+= sizeof(struct multiboot_tag_module_64)
               + cfg->kernel->args_len+1;
    }
    /* Add the remaining modules */
    for(cmp= cfg->first_module; cmp; cmp= cmp->next) {
        struct multiboot_tag_module_64 *module=
            (struct multiboot_tag_module_64 *)cursor;

        module->type= MULTIBOOT_TAG_TYPE_MODULE_64;
        module->size= sizeof(struct multiboot_tag_module_64)
                    + cmp->args_len+1;
        ntstring(module->cmdline, cfg->buf + cmp->args_start, cmp->args_len);

        cmp->tag= module;

        cursor+= sizeof(struct multiboot_tag_module_64)
               + cmp->args_len+1;
    }
    /* Record the position of the memory map, to be filled in after we've
     * finished doing allocations. */
    cfg->mmap_tag= (struct multiboot_tag_efi_mmap *)cursor;
    cursor+= sizeof(struct multiboot_tag_efi_mmap);
    cfg->mmap_start= cursor;

    return cfg->multiboot;
}
