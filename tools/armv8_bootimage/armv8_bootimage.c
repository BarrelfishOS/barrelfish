/*
 * Create a blob with a Multiboot2 image for the ARMv8 platform
 *
 * This tool reads menu.lst, loads a boot driver, a CPU kernel and modules,
 * and assemble them into a Multiboot2 image. Adds also relocation info for
 * the driver and the kernel.
 *
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/stat.h>
#include <sys/types.h>

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <libelf.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* We need to be able to parse menu.lst files, create multiboot images. */
#include "../../include/grubmenu.h"
#include "../../include/multiboot2.h"
#include "blob.h"

#define DBG(format, ...) printf(format, ## __VA_ARGS__)

/* Keep physical addresses and kernel virtual addresses separated, as far as
 * possible. */
typedef uint64_t kvaddr_t;
typedef uint64_t paddr_t;

/*** A Linear Memory Allocator ***/
static paddr_t phys_alloc_start = 0;

static size_t round_up(size_t x, size_t y)
{
    size_t z = x + (y - 1);
    return z - (z % y);
}

/* Advance the allocator to an address with the given alignment. */
static paddr_t align_alloc(paddr_t align)
{
    phys_alloc_start = round_up(phys_alloc_start, align);
    return phys_alloc_start;
}

/* Allocate an aligned block. */
static paddr_t phys_alloc(size_t size, size_t align)
{
    align_alloc(align);
    paddr_t addr = phys_alloc_start;
    phys_alloc_start += size;
    return addr;
}

/*** Failure Handling ***/

static void fail(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

static void fail_errno(const char *fmt, ...)
{
    char s[1024];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(s, 1024, fmt, ap);
    va_end(ap);

    perror(s);
    exit(EXIT_FAILURE);
}

static void fail_elf(const char *s)
{
    fprintf(stderr, "%s: %s\n", s, elf_errmsg(elf_errno()));
    exit(EXIT_FAILURE);
}

static void join_paths(char *dst, const char *src1, const char *src2)
{
    strcpy(dst, src1);
    dst[strlen(src1)] = '/';
    strcpy(dst + strlen(src1) + 1, src2);
}

struct ram_region {
    uint64_t base;
    uint64_t npages;
    void *buffer;
};

struct loaded_module {
    void *data;
    paddr_t paddr;
    size_t len, size;
    const char *shortname;
};

struct loaded_image {
    struct ram_region segment;

    size_t loaded_size;
    paddr_t loaded_paddr;
    kvaddr_t loaded_vaddr;

    paddr_t entry;
    const char *extrasym_name;
    void *extrasym_ptr;

    void *shdrs, *symtab, *strtab, *shstrtab;
    size_t shdrs_size, symtab_size, strtab_size, shstrtab_size;
    size_t shdrs_entsize, symtab_entsize;

    unsigned no_relocations;
    struct Blob_relocation *relocations;
};


/* Load an ELF file as a raw data blob. */
void raw_load(const char *path, struct loaded_module *m)
{
    struct stat mstat;

    if (stat(path, &mstat))
        fail_errno("stat: %s", path);

    size_t data_len = mstat.st_size;
    m->len = round_up(data_len, BASE_PAGE_SIZE);
    m->size = data_len;
    m->data = calloc(m->len, 1);
    if (!m->data)
        fail_errno("calloc");
    m->paddr = phys_alloc(m->len, BASE_PAGE_SIZE);

    printf("Allocated 0x%zx at PA %016zx for %s (%zd)\n", m->len, m->paddr,
           path, data_len);

    int fd = open(path, O_RDONLY);
    if (fd < 0)
        fail_errno("open: %s", path);
    size_t read_len = read(fd, m->data, data_len);
    if (read_len != data_len)
        fail_errno("fread");
    close(fd);
}

/*** Multiboot ***/

#define ROUND_UP(x, y) (((x) + ((y) - 1)) & ~((y) - 1))
#define ALIGN(x) ROUND_UP((x), sizeof(uintptr_t))

/* Create the multiboot header, using only *physical* addresses. */
void *create_multiboot_info(struct menu_lst *menu,
                            struct loaded_module *modules,
                            size_t * mb_size, paddr_t * mb_base,
                            paddr_t entry)
{
    size_t size;
    unsigned i;
    void *cursor;

    /* Calculate the boot information size. */
    /* Multiboot2 information data structure */
    size = 8;
    /* cpu driver command line */
    size += ALIGN(sizeof(struct multiboot_tag_string)
                  + strlen(menu->kernel.args) + 1);
    // /* Boot driver module tag, including command line and ELF image */
    size += ALIGN(sizeof(struct multiboot_tag_module_64)
                  + strlen(menu->boot_driver.path) + 2);
    // /* CPU driver module tag, including command line and ELF image */
    size += ALIGN(sizeof(struct multiboot_tag_module_64)
                  + strlen(menu->kernel.path) + strlen(menu->kernel.args) +
                  2);
    /* All other modules */
    for (i = 0; i < menu->nmodules; i++) {
        size += ALIGN(sizeof(struct multiboot_tag_module_64)
                      + strlen(menu->modules[i].path) +
                      strlen(menu->modules[i].args) + 2);
    }
#define MEM_MAP_SIZE (1<<13)
    /* EFI memory map */
    size += ALIGN(sizeof(struct multiboot_tag_efi_mmap) + MEM_MAP_SIZE);
    // END tag
    size += ALIGN(sizeof(struct multiboot_tag));

    size_t allocated_size = round_up(size, BASE_PAGE_SIZE);
    /* Allocate target addresses. */
    paddr_t base = phys_alloc(size, BASE_PAGE_SIZE);
    *mb_size = allocated_size;
    *mb_base = base;

    /* Allocate our host buffer. */
    void *mb = calloc(allocated_size, 1);
    if (!mb)
        fail_errno("calloc");

    cursor = mb;
    /* Skip the information structure for now */
    cursor += 8;

    /* Add the boot command line */
    {
        struct multiboot_tag_string *bootcmd =
            (struct multiboot_tag_string *) cursor;
        bootcmd->type = MULTIBOOT_TAG_TYPE_CMDLINE;
        bootcmd->size = ALIGN(sizeof(struct multiboot_tag_string)
                             + strlen(menu->kernel.path) +
                             strlen(menu->kernel.args) + 2);
        sprintf(bootcmd->string, "%s %s", menu->kernel.path,
                menu->kernel.args);
        cursor += bootcmd->size;
    }

    /* Add the boot driver module. */
    {
        struct multiboot_tag_module_64 *boot_driver =
            (struct multiboot_tag_module_64 *) cursor;

        boot_driver->type = MULTIBOOT_TAG_TYPE_MODULE_64;
        boot_driver->size = ALIGN(sizeof(struct multiboot_tag_module_64)
                                  + strlen(menu->boot_driver.path) + 2);
        boot_driver->mod_start = (multiboot_uint64_t) modules[0].paddr;
        boot_driver->mod_end =
            (multiboot_uint64_t) (modules[0].paddr + modules[0].size - 1);
        sprintf(boot_driver->cmdline, "%s", menu->boot_driver.path);
        cursor += boot_driver->size;
    }
    /* Add the kernel module. */
    {
        struct multiboot_tag_module_64 *kernel =
            (struct multiboot_tag_module_64 *) cursor;

        kernel->type = MULTIBOOT_TAG_TYPE_MODULE_64;
        kernel->size = ALIGN(sizeof(struct multiboot_tag_module_64)
                             + strlen(menu->kernel.path) +
                             strlen(menu->kernel.args) + 2);
        kernel->mod_start = (multiboot_uint64_t) modules[1].paddr;
        kernel->mod_end =
            (multiboot_uint64_t) (modules[1].paddr + modules[1].size - 1);
        sprintf(kernel->cmdline, "%s %s", menu->kernel.path,
                menu->kernel.args);
        cursor += kernel->size;
    }
    /* Add the remaining modules */
    for (i = 0; i < menu->nmodules; i++) {
        struct multiboot_tag_module_64 *module =
            (struct multiboot_tag_module_64 *) cursor;

        module->type = MULTIBOOT_TAG_TYPE_MODULE_64;
        module->size = ALIGN(sizeof(struct multiboot_tag_module_64)
                             + strlen(menu->modules[i].path) +
                             strlen(menu->modules[i].args) + 2);
        module->mod_start = (multiboot_uint64_t) modules[i + 2].paddr;
        module->mod_end =
            (multiboot_uint64_t) (modules[i + 2].paddr +
                                  modules[i + 2].size - 1);
        sprintf(module->cmdline, "%s %s", menu->modules[i].path,
                menu->modules[i].args);
        cursor += module->size;
    }
    /* Add the EFI MMAP tag */
    {
        struct multiboot_tag_efi_mmap *mmap_tag =
            (struct multiboot_tag_efi_mmap *) cursor;
        mmap_tag->type = MULTIBOOT_TAG_TYPE_EFI_MMAP;
        cursor += sizeof(struct multiboot_tag_efi_mmap);
    }
    return mb;
}

int relocate_elf(struct ram_region *segment, Elf * elf,
                 Elf64_Phdr * phdr, size_t phnum, size_t shnum,
                 unsigned *no_relocations,
                 struct Blob_relocation **relocations)
{
    size_t i;

    *no_relocations = 0;

    /* Search for relocaton sections. */
    for (i = 0; i < shnum; i++) {
        Elf_Scn *scn = elf_getscn(elf, i);
        if (!scn) {
            printf("elf_getscn: %s\n", elf_errmsg(elf_errno()));
            return -1;
        }

        Elf64_Shdr *shdr = elf64_getshdr(scn);
        if (!shdr) {
            printf("elf64_getshdr: %s\n", elf_errmsg(elf_errno()));
            return -1;
        }
        if (shdr->sh_type == SHT_DYNAMIC) {
            int relocations_size;
            Elf_Data *data = elf_getdata(scn, NULL);
            Elf64_Dyn *dt = (Elf64_Dyn *) data->d_buf;
            for (; dt->d_tag && dt->d_tag != DT_RELACOUNT; dt++) {
            }
            assert(dt->d_tag == DT_RELACOUNT);
            *no_relocations = dt->d_un.d_val;
            relocations_size =
                round_up(*no_relocations * sizeof(struct Blob_relocation),
                         BASE_PAGE_SIZE);
            *relocations = malloc(relocations_size);
        } else if (shdr->sh_type == SHT_RELA) {
            if (shdr->sh_info != 0) {
                printf("I expected global relocations, but got"
                       " section-specific ones.\n");
                return -1;
            }

            /* Hardcoded for one loadable segment.
               XXX: seems to be not always the case for some ARMv8 builids.
             */
            //ASSERT(phnum == 1);

            Elf64_Addr segment_elf_base = phdr[0].p_vaddr;
            Elf64_Addr segment_load_base = segment->base;
            Elf64_Sxword segment_delta =
                segment_load_base - segment_elf_base;

            /* Walk the section data descriptors. */
            Elf_Data *reldata;
            for (reldata = elf_getdata(scn, NULL);
                 reldata; reldata = elf_getdata(scn, reldata)) {
                size_t rsize;
                if (shdr->sh_type == SHT_REL)
                    rsize = sizeof(Elf64_Rel);
                else
                    rsize = sizeof(Elf64_Rela);

                size_t nrel = reldata->d_size / rsize;

                /* Iterate through the relocations. */
                size_t i;
                for (i = 0; i < nrel; i++) {
                    void *reladdr = reldata->d_buf + i * rsize;
                    Elf64_Addr offset;
                    Elf64_Xword sym, type;
                    Elf64_Sxword addend;

                    assert(shdr->sh_type == SHT_RELA);
                    Elf64_Rela *rel = reladdr;

                    offset = rel->r_offset;
                    sym = ELF64_R_SYM(rel->r_info);
                    type = ELF64_R_TYPE(rel->r_info);
                    addend = rel->r_addend;

                    assert(type == R_AARCH64_RELATIVE);
                    if (sym != 0) {
                        printf("Relocation references a"
                               " dynamic symbol, which is"
                               " unsupported.\n");
                        return -1;
                    }

                    /* Delta(S) + A */
                    (*relocations)[i].offset = offset;
                    (*relocations)[i].addend = addend;
                }
            }
        }
    }

    return 0;
}

/* Load and relocate an ELF, with the given offset between the physical
 * address at which it is loaded, and the virtual address at which it
 * executes.  For the boot driver, the offset is zero.  Return a variety of
 * information about the loaded image. */
static void load(struct loaded_module *module, uint32_t vp_offset,
                 struct loaded_image *image, int save_sections)
{
    int i;
    /* Open the ELF. */
    Elf *elf = elf_memory(module->data, module->size);
    if (!elf)
        fail_elf("elf_begin");

    /* Grab the unrelocated entry address from the header. */
    Elf64_Ehdr *ehdr = elf64_getehdr(elf);
    if (!ehdr)
        fail_elf("elf64_getehdr");
    image->entry = ehdr->e_entry;

    /* Grab the program headers i.e. the list of loadable segments. */
    size_t phnum;
    if (elf_getphdrnum(elf, &phnum))
        fail_elf("elf_getphnum");

    Elf64_Phdr *phdr = elf64_getphdr(elf);
    if (!phdr)
        fail_elf("elf_getphdr");

    DBG("%zd program segments.\n", phnum);

    /* Grab the raw ELF data. */
    size_t elfsize;
    void *elfdata = elf_rawfile(elf, &elfsize);
    if (!elfdata)
        fail_elf("elf_rawfile");

    /* Count the loadable segments, to allocate the region list. */
    size_t nloadsegs = 0;
    for (i = 0; i < phnum; i++) {
        if (phdr[i].p_type == PT_LOAD)
            nloadsegs++;
    }

    for (i = 0; i < phnum; i++) {
        printf
            ("Segment %d load address %zx, offset %zx, file size %zx, memory size %zx\n",
             i, phdr[i].p_vaddr, phdr[i].p_offset, phdr[i].p_filesz,
             phdr[i].p_memsz);
        if (phdr[i].p_type != PT_LOAD)
            continue;

        unsigned p_pages =
            round_up(phdr[i].p_memsz, BASE_PAGE_SIZE) / BASE_PAGE_SIZE;
        void *p_buf;

        paddr_t pa = phys_alloc(phdr[i].p_memsz, BASE_PAGE_SIZE);
        p_buf = calloc(p_pages * BASE_PAGE_SIZE, 1);
        assert(p_buf);

        image->segment.buffer = p_buf;
        image->segment.base = pa;
        image->segment.npages = p_pages;

        memcpy(p_buf, module->data + phdr[i].p_offset, phdr[i].p_filesz);
    }

    size_t shnum;
    int status;
    status = elf_getshdrnum(elf, &shnum);
    if (status) {
        printf("elf_getshdrnum: %s\n", elf_errmsg(elf_errno()));
        assert(0);
    }

    status =
        relocate_elf(&image->segment, elf, phdr, phnum, shnum,
                     &image->no_relocations, &image->relocations);
    if (status) {
        printf("Relocation failed.\n");
        assert(0);
    }
    elf_end(elf);
}


int main(int argc, char *argv[])
{
    char pathbuf[PATH_MAX + 1];

    // if(argc != 6) usage(argv[0]);

    const char *menu_lst = argv[1],
        *outfile = argv[2], *buildroot = argv[3];

    errno = 0;

    printf("ARMv8 Static Bootloader\n");

    /* Read the menu.lst file. */
    printf("Reading boot configuration from %s\n", menu_lst);
    struct menu_lst *menu = read_menu_lst(menu_lst);

    struct loaded_module *modules =
        calloc(menu->nmodules + 2, sizeof(struct loaded_module));
    if (!modules)
        fail_errno("calloc");

    // create the Blob
    paddr_t base = phys_alloc(sizeof(struct Blob), BASE_PAGE_SIZE);
    printf("Blob info struct at PA %016lx\n", base);

    // Load the boot driver
    join_paths(pathbuf, buildroot, menu->boot_driver.path);
    raw_load(pathbuf, modules);

    /* Use the filename as a short identifier. */
    const char *lastslash = strrchr(menu->boot_driver.path, '/');
    if (lastslash) {
        modules[0].shortname = lastslash + 1;
    } else {
        modules[0].shortname = "";
    }
    // Load the kernel
    join_paths(pathbuf, buildroot, menu->kernel.path);
    raw_load(pathbuf, modules + 1);

    /* Use the filename as a short identifier. */
    lastslash = strrchr(menu->kernel.path, '/');
    if (lastslash) {
        modules[1].shortname = lastslash + 1;
    } else {
        modules[1].shortname = "";
    }

    /*** Load the modules. ***/

    for (size_t i = 0; i < menu->nmodules; i++) {
        join_paths(pathbuf, buildroot, menu->modules[i].path);
        raw_load(pathbuf, modules + i + 2);

        /* Use the filename as a short identifier. */
        lastslash = strrchr(menu->modules[i].path, '/');
        if (lastslash) {
            modules[i + 2].shortname = lastslash + 1;
        } else {
            modules[i + 2].shortname = "";
        }
    }

    if (elf_version(EV_CURRENT) == EV_NONE)
        fail("ELF library version out of date.\n");
    /*** Load the boot driver. ***/

    /* Load and relocate it. */
    struct loaded_image bd_image[2];
    bd_image[0].extrasym_name = "boot_arguments";
    load(modules, 0, bd_image, 1);
    load(modules + 1, 0, bd_image + 1, 1);

    printf("Boot driver entry point: PA %08zx\n", bd_image[0].entry);
    printf("CPU driver entry point: PA %08zx\n", bd_image[1].entry);

    paddr_t pa, endpa;
    struct Blob blob;

    memset(blob.data, 0, sizeof(blob.data));
    blob.magic = 0x12345678fedcba90;

    pa = phys_alloc(bd_image[0].no_relocations *
                    sizeof(struct Blob_relocation), BASE_PAGE_SIZE);
    printf("Boot relocations PA %016zx,%d\n", pa,
           bd_image[0].no_relocations);
    blob.boot_driver_relocations = pa;
    blob.boot_driver_relocations_count = bd_image[0].no_relocations;
    blob.boot_driver_segment = bd_image[0].segment.base;
    blob.boot_driver_segment_size = bd_image[0].segment.npages * BASE_PAGE_SIZE;
    blob.boot_driver_entry = (uint64_t)bd_image[0].entry;

    pa = phys_alloc(bd_image[1].no_relocations *
                    sizeof(struct Blob_relocation), BASE_PAGE_SIZE);
    printf("Kernel relocations PA %016zx,%d\n", pa,
           bd_image[1].no_relocations);
    blob.cpu_driver_relocations = pa;
    blob.cpu_driver_relocations_count = bd_image[1].no_relocations;
    blob.cpu_driver_segment = bd_image[1].segment.base;
    blob.cpu_driver_segment_size = bd_image[1].segment.npages * BASE_PAGE_SIZE;
    blob.cpu_driver_entry = (uint64_t)bd_image[1].entry;

    /*** Create the multiboot info header. ***/
    size_t mb_size, size;
    paddr_t mb_base;
    void *mb_image =
        create_multiboot_info(menu, modules, &mb_size, &mb_base,
                              bd_image[1].entry);

    endpa = phys_alloc(BASE_PAGE_SIZE, BASE_PAGE_SIZE);
    printf("Final PA %016zx\n", endpa);

    blob.multiboot = mb_base;
    blob.multiboot_size = mb_size;
    
    blob.modules = modules[0].paddr;
    for (size_t i = 0; i < menu->nmodules + 2; i++) {
        blob.modules_size += modules[i].len;
    }

    size_t r;
    FILE *fp = fopen(outfile, "wb");
    assert(fp >= 0);
    // write the blob info
    r = fwrite(&blob, 1, BASE_PAGE_SIZE, fp);
    assert(r == BASE_PAGE_SIZE);
    // write the modules
    for (size_t i = 0; i < menu->nmodules + 2; i++) {
        r = fwrite(modules[i].data, 1, modules[i].len, fp);
        assert(r == modules[i].len);
    }
    // write the boot driver's ELF section
    r = fwrite(bd_image[0].segment.buffer, 1,
              bd_image[0].segment.npages * BASE_PAGE_SIZE, fp);
    assert(r == bd_image[0].segment.npages * BASE_PAGE_SIZE);
    // write the kernel's ELF section
    r = fwrite(bd_image[1].segment.buffer, 1,
              bd_image[1].segment.npages * BASE_PAGE_SIZE, fp);
    assert(r == bd_image[1].segment.npages * BASE_PAGE_SIZE);
    // write the boot driver's relocations
    size =
        round_up(bd_image[0].no_relocations *
                 sizeof(struct Blob_relocation), BASE_PAGE_SIZE);
    r = fwrite(bd_image[0].relocations, 1, size, fp);
    assert(r == size);
    // write the kernel's relocations
    size =
        round_up(bd_image[1].no_relocations *
                 sizeof(struct Blob_relocation), BASE_PAGE_SIZE);
    r = fwrite(bd_image[1].relocations, 1, size, fp);
    assert(r == size);
    // write the multiboot info
    r = fwrite(mb_image, 1, mb_size, fp);
    assert(r == mb_size);
    fclose(fp);

    return 0;
}
