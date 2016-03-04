#include <sys/types.h>
#include <sys/stat.h>

#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <libelf.h>
#include <multiboot2.h>

#include "efi.h"

void usage(char *name) {
    fprintf(stderr, "usage: %s <multiboot image> <load address> "
                    "<offset of mb header>\n", name);
    exit(EXIT_FAILURE);
}

void fail(char *name) {
    perror(name);
    exit(EXIT_FAILURE);
}

void elf_fail(char *name) {
    fprintf(stderr, "%s: %s\n", name, elf_errmsg(elf_errno()));
    exit(EXIT_FAILURE);
}

/* Read the complete contents of a file. */
void *
load_file(const char *path, size_t *length) {
    FILE *file= fopen(path, "r");
    if(!file) fail("fopen");

    struct stat stat;
    if(fstat(fileno(file), &stat) < 0) fail("stat");

    char *buf= malloc(stat.st_size);
    if(!buf) fail("malloc");

    if(fread(buf, 1, stat.st_size, file) != stat.st_size) fail("fread");

    if(fclose(file) != 0) fail("fclose");

    *length= stat.st_size;
    return buf;
}

int
main(int argc, char *argv[]) {
    if(argc != 4) usage(argv[0]);

    const char *infile= argv[1];

    errno= 0;
    uint64_t load_addr= strtoul(argv[2], NULL, 0);
    if(errno) fail("strtoul");

    errno= 0;
    uint64_t offset= strtoul(argv[3], NULL, 0);
    if(errno) fail("strtoul");

    if(elf_version(EV_CURRENT) == EV_NONE) elf_fail("elf_version");

    printf("Loading multiboot image \"%s\" at 0x%lx\n", infile, load_addr);

    /* Load it. */
    size_t mb_len;
    void *mb_data= load_file(infile, &mb_len);

    /* Start walking the header. */
    void *cursor= mb_data + offset;

    /* Check the fixed 8-byte header. */
    uint32_t total_size= *((uint32_t *)cursor);
    cursor+= sizeof(uint32_t);
    assert(*((uint32_t *)cursor) == 0);
    cursor+= sizeof(uint32_t);

    /* Read the tags. */
    while(cursor < mb_data + offset + total_size) {
        uint64_t paddr= (cursor - mb_data) + load_addr;
        struct multiboot_tag *tag=
            (struct multiboot_tag *)cursor;

        printf("Tag of %uB @ 0x%lx\n", (unsigned int)tag->size, paddr);
        switch(tag->type) {
            case MULTIBOOT_TAG_TYPE_CMDLINE:
            {
                struct multiboot_tag_string *str_tag=
                    (struct multiboot_tag_string *)cursor;
                printf("CPU driver command line: \"%s\"\n",
                       str_tag->string);
                break;
            }
            case MULTIBOOT_TAG_TYPE_ELF_SECTIONS:
            {
                struct multiboot_tag_elf_sections *sect_tag=
                    (struct multiboot_tag_elf_sections *)cursor;
                printf("CPU driver ELF section headers\n");
                printf("%u headers of size %u\n",
                        sect_tag->num, sect_tag->entsize);
                printf("String table in section %u\n",
                        sect_tag->shndx);

                Elf64_Shdr *shdrs= (Elf64_Shdr *)sect_tag->sections;
                for(size_t i= 0; i < sect_tag->num; i++) {
                    printf("section %lu, type %u, address 0x%lx\n",
                           i, shdrs[i].sh_type, shdrs[i].sh_addr);
                }
                break;
            }
            case MULTIBOOT_TAG_TYPE_MODULE_64:
            {
                struct multiboot_tag_module_64 *mod_tag=
                    (struct multiboot_tag_module_64 *)cursor;
                printf("64-bit ELF module @ 0x%lx-0x%lx\n",
                       (uint64_t)mod_tag->mod_start,
                       (uint64_t)mod_tag->mod_end);
                printf("Command line: \"%s\"\n", mod_tag->cmdline);

                void *elf_data= mb_data + (mod_tag->mod_start - load_addr);
                size_t elf_size= mod_tag->mod_end - mod_tag->mod_start + 1;

                Elf *elf= elf_memory(elf_data, elf_size);
                if(!elf) elf_fail("elf_memory");

                size_t e_i_size;
                char *e_ident= elf_getident(elf, &e_i_size);
                if(!e_ident) elf_fail("elf_getident");

                printf("e_ident bytes:");
                for(int i= 0; i < e_i_size; i++)
                    printf(" %02x", e_ident[i]);
                printf("\n");

                Elf64_Ehdr *ehdr= elf64_getehdr(elf);
                if(!ehdr) elf_fail("elf64_getehdr");

                printf("Entry point: %lx\n", ehdr->e_entry);

                if(elf_end(elf)) elf_fail("elf_end");
                break;
            }
            case MULTIBOOT_TAG_TYPE_EFI_MMAP:
            {
                struct multiboot_tag_efi_mmap *mmap_tag=
                    (struct multiboot_tag_efi_mmap *)cursor;
                size_t mmap_len= mmap_tag->size / mmap_tag->descr_size;
                print_mmap((efi_memory_descriptor *)&mmap_tag->efi_mmap,
                           mmap_len);
                break;
            }
        }
        printf("\n");

        cursor+= tag->size;
    }
}
