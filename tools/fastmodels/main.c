#include <sys/types.h>
#include <sys/stat.h>

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define KERNEL_OFFSET 0xffff000000000000

#include "build_multiboot.h"
#include "config.h"
#include "efi.h"
#include "util.h"

void usage(char *name) {
    fprintf(stderr, "usage: %s <config> <ram size> <shim image> <fs root>"
                    " <output file> [-d]\n", name);
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
load_file(const char *path, size_t *length, size_t *alloc) {
    FILE *file= fopen(path, "r");
    if(!file) fail("fopen");

    struct stat stat;
    if(fstat(fileno(file), &stat) < 0) fail("stat");

    /* Allocate a page-sized zero-initialised buffer. */
    size_t alloc_size= ROUNDUP(stat.st_size, PAGE_4k);
    char *buf= calloc(alloc_size, 1);
    if(!buf) fail("calloc");

    if(fread(buf, 1, stat.st_size, file) != stat.st_size) fail("fread");

    if(fclose(file) != 0) fail("fclose");

    *length= stat.st_size;
    *alloc= alloc_size;
    return buf;
}

/* Load the ELF image for a component, and fill the relevant fields in the
 * configuration struct. */
int
load_component(char *basepath, size_t bplen, struct component_config *comp,
               char *buf) {
    if(bplen + comp->path_len >= PATH_MAX) {
        errno= ENAMETOOLONG;
        return -1;
    }

    /* Append the component path to the FS base path, and null terminate. */
    memcpy(basepath + bplen, buf + comp->path_start, comp->path_len);
    basepath[bplen + comp->path_len]= '\0';

    /* Canonicalise the path. */
    char path[PATH_MAX];
    if(!realpath(basepath, path)) {
        fprintf(stderr, "Couldn't find %s\n", path);
        fail("relpath");
    }

    /* Load the ELF */
#if 0
    printf("Loading component %s\n", path);
#endif
    comp->image= load_file(path, &comp->image_size, &comp->alloc_size);

    return 0;
}

/* Fill the preallocated EFI memory map in. */
efi_memory_descriptor *
build_efi_mmap(struct config *config, size_t mmap_len,
               size_t first_region, uint64_t ram_size) {
    efi_memory_descriptor *mmap=
        (efi_memory_descriptor *)config->mmap_start;

    /* Write the tag. */
    config->mmap_tag->type= MULTIBOOT_TAG_TYPE_EFI_MMAP;
    config->mmap_tag->size= sizeof(struct multiboot_tag_efi_mmap) +
                            mmap_len * sizeof(efi_memory_descriptor);
    config->mmap_tag->descr_size= sizeof(efi_memory_descriptor);
    config->mmap_tag->descr_vers= 1;

    if((ram_size & (PAGE_4k-1)) != 0) {
        fprintf(stderr, "RAM size %lu isn't a multiple of 4k.\n", ram_size);
        exit(EXIT_FAILURE);
    }

    /* Calculate the sizes of the two windows - fill the lower one first. */
    uint64_t region_one, region_two;
    if(ram_size < 2 * (1UL<<30)) {
        region_one= ram_size;
        region_two= 0;
    }
    else {
        region_one= 2 * (1UL<<30);
        region_two = ram_size - region_one;
    }
    assert(region_two <= 6 * (1UL<<30));

    /* The first 2GiB RAM window starts at 0x80000000, or 2GiB, on sensible
     * ARM platforms, such as this one. */
    mmap[first_region].Type=          EfiConventionalMemory;
    mmap[first_region].PhysicalStart= 0x80000000;
    mmap[first_region].VirtualStart=  0x80000000;
    mmap[first_region].NumberOfPages= region_one / PAGE_4k;
    mmap[first_region].Attribute=     0; /* XXX - this should change. */

    /* Add the second region, only if required.  It must be allocated. */
    if(region_two > 0) {
        assert(first_region + 1 < mmap_len);
        /* On platforms that follow the "Principles of ARM Memory Maps"
         * whitepaper, the second RAM window begins at 0x880000000, or 2GiB +
         * 32GiB, and is 30GiB in length.  The pattern repeats, such that an
         * n-bit physical address space has 2^(n-1)B of RAM windows.  On some
         * platforms (including the fast models), the region 0x800000000 -
         * 0x87fffffff aliases the first RAM window, giving a contiguous
         * window in a 36-bit or greater PA space.  Using the aliased
         * addresses, however, is a bad idea - physical memory aliases are
         * not good for the caches. */
        mmap[first_region+1].Type=          EfiConventionalMemory;
        mmap[first_region+1].PhysicalStart= 0x880000000;
        mmap[first_region+1].VirtualStart=  0x880000000;
        mmap[first_region+1].NumberOfPages= region_two / PAGE_4k;
        mmap[first_region+1].Attribute=     0; /* XXX - this should change. */
    }

    /* We only need two windows to map up to 32GiB of RAM, which is already
     * more than the fast models support. */

    return mmap;
}

void
check_alloc(uint64_t allocbase, efi_memory_descriptor *mmap, size_t region) {
    if(allocbase >= mmap[region].PhysicalStart +
                    mmap[region].NumberOfPages * PAGE_4k) {
        fprintf(stderr, "Ran out of room in the first memory region.\n");
        fprintf(stderr, "Region: %lx-%lx, allocbase=%lx\n",
                mmap[region].PhysicalStart,
                mmap[region].PhysicalStart +
                mmap[region].NumberOfPages * PAGE_4k,
                allocbase);
        exit(EXIT_FAILURE);
    }
}

void *
load_cpudriver(Elf *kernel_elf, void *kernel_raw, size_t kernel_size,
               uint64_t virt_base, uint64_t *loaded_size, uint64_t *alloc,
               uint64_t *entry) {
    size_t phnum;
    if(elf_getphdrnum(kernel_elf, &phnum)) elf_fail("elf_getphdrnum");

    Elf64_Phdr *phdrs= elf64_getphdr(kernel_elf);
    if(!phdrs) elf_fail("elf64_getphdr");

    /* Find the dynamic segment, and calculate the base and limit for the
     * loadable region. */
    uint64_t base= UINT64_MAX, limit= 0;
    size_t dseg;
    int have_dseg= 0;
    for(size_t kseg= 0; kseg < phnum; kseg++) {
        if(phdrs[kseg].p_type == PT_DYNAMIC) {
            if(have_dseg) {
                fprintf(stderr, "Two PT_DYNAMIC segments.\n");
                exit(EXIT_FAILURE);
            }
            dseg= kseg;
            have_dseg= 1;
        }
        else if(phdrs[kseg].p_type == PT_LOAD) {
            if(phdrs[kseg].p_vaddr < base)
                base= phdrs[kseg].p_vaddr;

            assert(phdrs[kseg].p_memsz <= UINT64_MAX - phdrs[kseg].p_vaddr);
            if(phdrs[kseg].p_vaddr + phdrs[kseg].p_memsz > limit)
                limit= phdrs[kseg].p_vaddr + phdrs[kseg].p_memsz;
        }
    }
    if(!have_dseg) {
        fprintf(stderr, "No PT_DYNAMIC segment.\n");
        exit(EXIT_FAILURE);
    }

    /* Relocate the entry point. */
    Elf64_Ehdr *ehdr= elf64_getehdr(kernel_elf);
    if(!ehdr) elf_fail("elf64_getehdr");
    *entry= virt_base + (ehdr->e_entry - base);

    /* Allocate the target region. */
    *loaded_size= limit - base + 1;
    *alloc= ROUNDUP(*loaded_size, PAGE_4k);
    void *cpudriver= malloc(*alloc);
    if(!cpudriver) fail("malloc");
    bzero(cpudriver, *alloc);

    /* Copy all loadable segments. */
    int loaded_something= 0;
    for(size_t kseg= 0; kseg < phnum; kseg++) {
        Elf64_Phdr *ph= &phdrs[kseg];
        if(ph->p_type == PT_LOAD) {
            assert(ph->p_offset < kernel_size);
            assert(ph->p_offset + ph->p_filesz < kernel_size);

            void *seg_vbase= cpudriver + (ph->p_vaddr - base);
            memcpy(seg_vbase, kernel_raw + ph->p_offset, ph->p_filesz);

            loaded_something= 1;
        }
    }

    if(!loaded_something) {
        fprintf(stderr, "No loadable segments in CPU driver ELF.\n");
        exit(EXIT_FAILURE);
    }

    /* Process the dynamic linking section. */
    Elf64_Phdr *dhdr= &phdrs[dseg];
    size_t dtnum= dhdr->p_filesz / sizeof(Elf64_Dyn);
    Elf64_Dyn *dt= (Elf64_Dyn *)(kernel_raw + dhdr->p_offset);
    void *rela_base= NULL;
    size_t rela_entsize= 0, rela_count= 0;
    for(size_t i= 0; i < dtnum && dt[i].d_tag != DT_NULL; i++) {
        switch(dt[i].d_tag) {
            case DT_RELA:
                /* The address of the relocation section is given as an
                 * unrelocated virtual address, inside the loaded segment.  We
                 * need to rebase it. */
                rela_base= cpudriver + (dt[i].d_un.d_ptr - base);
                break;
            case DT_RELAENT:
                rela_entsize= dt[i].d_un.d_val;
                break;
            case DT_RELACOUNT:
                rela_count= dt[i].d_un.d_val;
                break;

            case DT_RELASZ:
            case DT_TEXTREL:
            case DT_DEBUG:
                break; /* Ignored */

            case DT_REL:
            case DT_RELENT:
            case DT_RELCOUNT:
                fprintf(stderr, "Unsupported relocation type: DT_REL\n");
                exit(EXIT_FAILURE);

            default:
                printf("Warning, ignoring dynamic section entry, tag %lx\n",
                       dt[i].d_tag);
        }
    }
    if(!rela_base || !rela_entsize || !rela_count) {
        printf("Warning: no relocation (RELA) section.\n");
        return cpudriver;
    }

    /* Process the relocations. */
    for(size_t i= 0; i < rela_count; i++, rela_base+= rela_entsize) {
        Elf64_Rela *rela= (Elf64_Rela *)rela_base;

        if(ELF64_R_SYM(rela->r_info) != 0) {
            fprintf(stderr, "Unsupported symbol-based relocation at %lx.\n",
                            rela->r_offset);
            exit(EXIT_FAILURE);
        }

        /* Find the target, inside the loaded segment. */
        uint64_t *target= cpudriver + (rela->r_offset - base);

#if 0
        printf("%lx[%lx] (%p): %lx ->", rela->r_offset,
                rela->r_addend, target, *target);
#endif

        switch(ELF64_R_TYPE(rela->r_info)) {
            /* Our one supported relocation type. */
            case R_AARCH64_RELATIVE: {
                /* Relocation: Delta(S) + A */
                *target= (rela->r_addend - base) + virt_base;
                break;
            }

            default:
                fprintf(stderr, "Unsupported relocation type (%lu) at %lx.\n",
                                ELF64_R_TYPE(rela->r_info), rela->r_offset);
                exit(EXIT_FAILURE);
        }
#if 0
        printf(" %lx\n", *target);
#endif
    }

    return cpudriver;
}

void *
load_shim(Elf *shim_elf, void *shim_raw, size_t shim_size,
          uint64_t virt_base, uint64_t *loaded_size, uint64_t *alloc,
          uint64_t kernel_table, uint64_t kernel_stack_top,
          uint64_t multiboot, uint64_t entry, uint64_t *shim_entry,
          int quiet) {
    size_t phnum;
    if(elf_getphdrnum(shim_elf, &phnum)) elf_fail("elf_getphdrnum");

    Elf64_Phdr *phdrs= elf64_getphdr(shim_elf);
    if(!phdrs) elf_fail("elf64_getphdr");

    /* Calculate the base and limit for the loadable region. */
    uint64_t base= UINT64_MAX, limit= 0;
    for(size_t kseg= 0; kseg < phnum; kseg++) {
        if(phdrs[kseg].p_type == PT_LOAD) {
            if(phdrs[kseg].p_vaddr < base)
                base= phdrs[kseg].p_vaddr;

            assert(phdrs[kseg].p_memsz <= UINT64_MAX - phdrs[kseg].p_vaddr);
            if(phdrs[kseg].p_vaddr + phdrs[kseg].p_memsz > limit)
                limit= phdrs[kseg].p_vaddr + phdrs[kseg].p_memsz;
        }
    }

    /* Relocate the entry point. */
    Elf64_Ehdr *ehdr= elf64_getehdr(shim_elf);
    if(!ehdr) elf_fail("elf64_getehdr");
    *shim_entry= virt_base + (ehdr->e_entry - base);

    /* Allocate the target region. */
    *loaded_size= limit - base + 1;
    *alloc= ROUNDUP(*loaded_size, PAGE_4k);
    void *shim= malloc(*alloc);
    if(!shim) fail("malloc");
    bzero(shim, *alloc);

    /* Copy all loadable segments. */
    int loaded_something= 0;
    for(size_t kseg= 0; kseg < phnum; kseg++) {
        Elf64_Phdr *ph= &phdrs[kseg];
        if(ph->p_type == PT_LOAD) {
            assert(ph->p_offset < shim_size);
            assert(ph->p_offset + ph->p_filesz < shim_size);

            void *seg_vbase= shim + (ph->p_vaddr - base);
            memcpy(seg_vbase, shim_raw + ph->p_offset, ph->p_filesz);

            loaded_something= 1;
        }
    }

    if(!loaded_something) {
        fprintf(stderr, "No loadable segments in shim ELF.\n");
        exit(EXIT_FAILURE);
    }

    /* Find the symbol and string tables. */
    Elf_Scn *scn= NULL;
    Elf64_Word sh_type;
    Elf64_Shdr *sh_symtab= NULL, *sh_strtab= NULL;
    while((scn= elf_nextscn(shim_elf, scn)) != NULL) {
        if(!scn) elf_fail("elf_nextscn");

        Elf64_Shdr *shdr= elf64_getshdr(scn);
        if(!shdr) elf_fail("elf64_getshdr");
        sh_type= shdr->sh_type;

        if(sh_type == SHT_REL || sh_type == SHT_RELA) {
            fprintf(stderr, "Shim requires relocation.\n");
            exit(EXIT_FAILURE);
        }

        if(sh_type == SHT_SYMTAB) sh_symtab= shdr;
        if(sh_type == SHT_STRTAB) sh_strtab= shdr;
    }
    if(!sh_symtab) {
        fprintf(stderr, "Missing symbol table.\n");
        exit(EXIT_FAILURE);
    }
    if(!sh_strtab) {
        fprintf(stderr, "Missing symbol table.\n");
        exit(EXIT_FAILURE);
    }

    /* Find the pointer fields to fill in. */
    int have_kernel_table= 0, have_kernel_stack_top= 0,
        have_multiboot= 0, have_entry= 0;
    const char *strings= (const char *)(shim_raw + sh_strtab->sh_offset);
    for(size_t i= 0; i < sh_symtab->sh_size; i+= sizeof(Elf64_Sym)) {
        Elf64_Sym *sym= (Elf64_Sym *)(shim_raw + sh_symtab->sh_offset + i);

        /* Find the symbol name. */
        const char *name= strings + sym->st_name;

        /* Find the symbol in the loaded image. */
        uint64_t *target= shim + (sym->st_value - base);

        /* Check for the target symbols. */
        uint64_t value;
        if(!strcmp("p_kernel_table", name)) {
            have_kernel_table= 1;
            value= kernel_table;
        }
        else if(!strcmp("p_kernel_stack_top", name)) {
            have_kernel_stack_top= 1;
            value= kernel_stack_top;
        }
        else if(!strcmp("p_multiboot", name)) {
            have_multiboot= 1;
            value= multiboot;
        }
        else if(!strcmp("p_entry", name)) {
            have_entry= 1;
            value= entry;
        }
        else continue;

        /* Update the pointer. */
        if(!quiet) {
            printf("Setting %s at %lx to %lx\n",
                   name, virt_base + (sym->st_value - base), value);
        }
        *target= value;
    }
    if(!(have_kernel_table && have_kernel_stack_top &&
         have_multiboot && have_entry)) {
        fprintf(stderr, "Missing shim symbol.\n");
        exit(EXIT_FAILURE);
    }

    return shim;
}

/* A descriptor for the next-level table.
 * These are the same at all levels. */
struct table_descriptor {
    uint64_t        type            :2;     // == 3 -> Table
    uint64_t        ign0            :10;    // Ignored
    uint64_t        base_address    :28;    // Table address
    uint64_t        sbz0            :12;    // sbz
    uint64_t        ign1            :7;     // Ignored

    /* Hierarchical lookup attributes */
    uint64_t        pxn             :1;     // Privileged eXecute Never
    uint64_t        xn              :1;     // eXecute Never
    uint64_t        ap              :2;     // Access Permissions
    uint64_t        ns              :1;     // NonSecure
};

union armv8_l1_entry {
    uint64_t raw;

    /* An L1 entry for a 1GB block (page) */
    struct {
        uint64_t        type            :2;     // == 1 -> Block

        /* Lower block attributes */
        uint64_t        ai              :3;
        uint64_t        ns              :1;
        uint64_t        ap              :2;     // AP
        uint64_t        sh              :2;     // AP
        uint64_t        af              :1;     // AF
        uint64_t        ng              :1;     // NG

        uint64_t        sbz0            :18;
        uint64_t        base_address    :18;    // block base address
        uint64_t        sbz1            :4;

        /* Upper block attributes */
        uint64_t        ch              :1;     // CH
        uint64_t        pxn             :1;     // PXN
        uint64_t        xn              :1;     // XN
        uint64_t        res             :4;     // Reserved
        uint64_t        ign1            :5;     // Ignored
    } block;
};

#define BIT(n) (1ULL << (n))
#define MASK(n) (BIT(n) - 1)
#define PTABLE_ENTRY_BITS 3
#define PTABLE_ENTRY_SIZE BIT(PTABLE_ENTRY_BITS)
#define PTABLE_BITS          9
#define PTABLE_SIZE          BIT(PTABLE_BITS + PTABLE_ENTRY_BITS)
#define PTABLE_NUM_ENTRIES   BIT(PTABLE_BITS)

enum armv8_entry_type {
    ARMv8_Ln_INVALID = 0,
    ARMv8_Ln_BLOCK   = 1,
    ARMv8_Ln_TABLE   = 3,
    ARMv8_L3_PAGE    = 3
};

void *
alloc_kernel_pt(size_t *pt_size, uint64_t table_base,
        efi_memory_descriptor *mmap, size_t mmap_len) {
    /* Allocate one L0 & one L1 table, in a contiguous block. An L0
     * translation unit i.e. an L1 table, maps 512GiB with our translation
     * settings, which is more than enough for a one-to-one kernel physical
     * window. */
    void *block= calloc(2, PTABLE_SIZE);
    if(!block) fail("calloc");

    struct table_descriptor *l0_table=
        (struct table_descriptor *)block;
    union armv8_l1_entry *l1_table=
        (union armv8_l1_entry *)(block + PTABLE_SIZE);

    /* Map the first two 1GiB blocks as device memory, using memory attribute
     * 1, which we'll set to nGnRnE. */
    for(size_t j= 0; j < 2; j++) {
        l1_table[j].block.type= ARMv8_Ln_BLOCK;
        l1_table[j].block.ai=  1; /* Memory type 1 */
        l1_table[j].block.ns=  1; /* Non-secure. */
        l1_table[j].block.ap=  0; /* R/W EL1, no access EL0 */
        l1_table[j].block.sh=  2; /* Outer shareable - this is actually
                                     ignored anyway. */
        l1_table[j].block.af=  1; /* Accessed/dirty - don't fault */
        l1_table[j].block.ng=  0; /* Global mapping */
        l1_table[j].block.base_address= j; /* PA = j << 30 */
        l1_table[j].block.ch=  1; /* Contiguous, combine TLB entries */
        l1_table[j].block.pxn= 1; /* Nonexecutable. */
        l1_table[j].block.xn=  1; /* Nonexecutable. */
    }

    /* Map all RAM regions using contiguous 1GiB blocks.  Use memory attribute
     * 0, which we will set to fully-cacheable Normal memory. */
    for(size_t i= 0; i < mmap_len; i++) {
        efi_memory_descriptor *d= &mmap[i];

        if(d->Type == EfiConventionalMemory) {
            if(d->VirtualStart + d->NumberOfPages * PAGE_4k >= BIT(39)) {
                fprintf(stderr, "RAM region %i lies above 512GB!\n", (int)i);
                exit(EXIT_FAILURE);
            }

            if((d->VirtualStart & MASK(30)) != 0) {
                fprintf(stderr, "RAM region %i not 1GB-aligned!\n", (int)i);
                exit(EXIT_FAILURE);
            }

            if((d->NumberOfPages & MASK(18)) != 0) {
                fprintf(stderr, "RAM region %i not 1GB-aligned!\n", (int)i);
                exit(EXIT_FAILURE);
            }

            size_t ptbase= d->VirtualStart >> 30;
            size_t ptend= ptbase + (d->NumberOfPages >> 18);

            assert(ptbase < PTABLE_NUM_ENTRIES &&
                   ptend < PTABLE_NUM_ENTRIES);

            for(size_t j= ptbase; j < ptend; j++) {
                l1_table[j].block.type= ARMv8_Ln_BLOCK;
                l1_table[j].block.ai=  0; /* Memory type 0 */
                l1_table[j].block.ns=  1; /* Non-secure. */
                l1_table[j].block.ap=  0; /* R/W EL1, no access EL0 */
                l1_table[j].block.sh=  3; /* Inner-shareable, fully coherent */
                l1_table[j].block.af=  1; /* Accessed/dirty - don't fault */
                l1_table[j].block.ng=  0; /* Global mapping */
                l1_table[j].block.base_address= j; /* PA = j << 30 */
                l1_table[j].block.ch=  1; /* Contiguous, combine TLB entries */
                l1_table[j].block.pxn= 0; /* Executable. */
                l1_table[j].block.xn=  0; /* Executable. */
            }
        }
    }

    uint64_t l1_base= table_base + PTABLE_SIZE;

    /* Map the L1 table into the L0. */
    l0_table[0].type= ARMv8_Ln_TABLE;
    l0_table[0].base_address= l1_base >> 12;
    l0_table[0].pxn= 0; /* Executable. */
    l0_table[0].xn=  0; /* Executable. */
    l0_table[0].ap=  0; /* No permission masking. */
    l0_table[0].ns=  1; /* Non-secure. */

    *pt_size= 2 * PTABLE_SIZE;
    return (void *)block;
}

int
main(int argc, char *argv[]) {
    if(argc < 6 || argc > 7) usage(argv[0]);

    const char *config_path= argv[1],
               *shim_path=   argv[3],
               *base_path=   argv[4],
               *out_path=    argv[5];

    errno= 0;
    uint64_t ram_size= strtoul(argv[2], NULL, 0);
    if(errno) fail("strtoul");

    int debug_details= 0;
    if(argc == 7 && !strcmp("-d", argv[6])) debug_details= 1;

    /* Load the configuration. */
    size_t config_size, config_alloc;
    char *config_raw=
        (char *)load_file(config_path, &config_size, &config_alloc);

    /* Parse the configuration. */
    struct config *config= parse_config(config_raw, config_size);
    if(!config) exit(EXIT_FAILURE);

    /* Construct the working buffer for paths. */
    char basepath_buf[PATH_MAX];
    size_t basepath_len;
    strncpy(basepath_buf, base_path, PATH_MAX);
    basepath_len= strlen(base_path);

    /* Load the kernel ELF. */
    assert(config->kernel);
    if(load_component(basepath_buf, basepath_len,
                      config->kernel, config_raw) != 0) {
        fail("load_component");
    }

    if(elf_version(EV_CURRENT) == EV_NONE)
        elf_fail("elf_version");

    /* Start parsing the kernel ELF. */
    Elf *kernel_elf=
        elf_memory((char *)config->kernel->image,
                   config->kernel->image_size);
    if(!kernel_elf) elf_fail("elf_memory");

    /* Load all modules. */
    size_t n_modules= 0;
    for(struct component_config *comp= config->first_module;
                                 comp; comp= comp->next) {
        n_modules++;
        if(load_component(basepath_buf, basepath_len, comp, config_raw) != 0)
            fail("load_component");
    }

    /* How many RAM regions are there?  If there's more than 2GiB, it'll be
     * split into two. */
    if(ram_size > 8 * (1UL<<30)) {
        fprintf(stderr, "The models only support <= 8GiB of RAM.\n");
        exit(EXIT_FAILURE);
    }
    size_t ram_regions;
    if(ram_size > 2 * (1UL<<30)) ram_regions= 2;
    else ram_regions= 1;

    /* The EFI memory map contains one entry for each loaded module, one for
     * each RAM region, and 5 fixed entries: The loaded CPU driver, the CPU
     * driver's stack, the CPU driver's page tables, the Multiboot header, and
     * the CPU driver ELF. */
    size_t mmap_len= n_modules + ram_regions + 5;
    /* The RAM regions are at the end, thus the first is at n_modules + 5. */
    size_t first_region= n_modules + 5;

    /* Create the multiboot header, now that we know how big the memory map
     * needs to be.  */
    void *mb_header=
        create_multiboot2_info(config, kernel_elf,
                               mmap_len * sizeof(efi_memory_descriptor));
    if(!mb_header) {
        fprintf(stderr, "Couldn't build the multiboot header.\n");
        exit(EXIT_FAILURE);
    }

    /* Build the EFI memory map.  We leave uninitialised entries at the
     * beginning for the kernel, all modules, the CPU driver's loadable
     * segment & stack, the MB header, and the boot page table. */
    efi_memory_descriptor *mmap=
        build_efi_mmap(config, mmap_len, first_region, ram_size);

    /* Start allocating at the beginning of the first physical region. */
    uint64_t allocbase= mmap[first_region].PhysicalStart;
    uint64_t loadbase= allocbase;

    /* Load the CPU driver into physical RAM, and relocate for the kernel
     * window. */
    uint64_t kernel_start= allocbase;
    uint64_t cpudriver_size, cpudriver_alloc, cpudriver_entry;
    void *cpudriver=
        load_cpudriver(kernel_elf, config->kernel->image,
                       config->kernel->image_size,
                       KERNEL_OFFSET + kernel_start,
                       &cpudriver_size, &cpudriver_alloc, &cpudriver_entry);

    /* Allocate the CPU driver's loadable segment. */
    allocbase= ROUNDUP(allocbase + cpudriver_size, PAGE_4k);
    check_alloc(allocbase, mmap, first_region);
    mmap[0].Type= EfiBarrelfishCPUDriver;
    mmap[0].PhysicalStart= kernel_start;
    mmap[0].VirtualStart= kernel_start;
    mmap[0].NumberOfPages= roundpage(cpudriver_size);
    mmap[0].Attribute= 0; /* XXX */

    /* Now allocate the CPU driver's stack. */
    config->kernel_stack= allocbase;
    uint64_t kernel_stack_alloc= ROUNDUP(config->stack_size, PAGE_4k);
    allocbase= ROUNDUP(allocbase + kernel_stack_alloc, PAGE_4k);
    check_alloc(allocbase, mmap, first_region);
    mmap[1].Type= EfiBarrelfishCPUDriverStack;
    mmap[1].PhysicalStart= config->kernel_stack;
    mmap[1].VirtualStart= config->kernel_stack;
    mmap[1].NumberOfPages= roundpage(config->stack_size);
    mmap[1].Attribute= 0; /* XXX */

    /* Allocate frames for the CPU driver's root page table. */
    uint64_t kernel_table= allocbase;
    size_t kernel_pt_size;
    void *kernel_pt=
        alloc_kernel_pt(&kernel_pt_size, kernel_table, mmap, mmap_len);
    mmap[2].Type= EfiBarrelfishBootPageTable;
    mmap[2].PhysicalStart= allocbase;
    mmap[2].VirtualStart= allocbase;
    mmap[2].NumberOfPages= 1;
    mmap[2].Attribute= 0; /* XXX */
    allocbase+= kernel_pt_size;
    check_alloc(allocbase, mmap, first_region);

    /* Allocate space for the multiboot header. */
    uint64_t multiboot= allocbase;
    mmap[3].Type= EfiBarrelfishMultibootData;
    mmap[3].PhysicalStart= allocbase;
    mmap[3].VirtualStart= allocbase;
    mmap[3].NumberOfPages= roundpage(config->multiboot_size);
    mmap[3].Attribute= 0; /* XXX */
    allocbase= ROUNDUP(allocbase + config->multiboot_size, PAGE_4k);
    check_alloc(allocbase, mmap, first_region);

    /* Allocate room for the CPU driver ELF. */
    config->kernel->image_address= allocbase;
    allocbase= ROUNDUP(allocbase + config->kernel->image_size, PAGE_4k);
    check_alloc(allocbase, mmap, first_region);
    mmap[4].Type= EfiBarrelfishELFData;
    mmap[4].PhysicalStart= config->kernel->image_address;
    mmap[4].VirtualStart= config->kernel->image_address;
    mmap[4].NumberOfPages= roundpage(config->kernel->image_size);
    mmap[4].Attribute= 0; /* XXX */

    /* Update the multiboot tag. */
    config->kernel->tag->mod_start=
        (multiboot_uint64_t)config->kernel->image_address;
    config->kernel->tag->mod_end=
        (multiboot_uint64_t)(config->kernel->image_address +
                             (config->kernel->image_size - 1));

    if(!debug_details) {
        printf("ELF %.*s %luB @ 0x%lx\n",
               (int)config->kernel->path_len,
               config_raw + config->kernel->path_start,
               config->kernel->image_size,
               config->kernel->image_address);
    }

    /* Allocate all remaining modules.  We've allocated 5 mmap entries so far,
     * so this will bring us up to n_modules+5. */
    struct component_config *m;
    size_t i_mmap;
    for(i_mmap= 5, m= config->first_module; m; i_mmap++, m= m->next) {
        m->image_address= allocbase;
        allocbase= ROUNDUP(allocbase + m->image_size, PAGE_4k);
        check_alloc(allocbase, mmap, first_region);
        mmap[i_mmap].Type= EfiBarrelfishELFData;
        mmap[i_mmap].PhysicalStart= m->image_address;
        mmap[i_mmap].VirtualStart= m->image_address;
        mmap[i_mmap].NumberOfPages= roundpage(m->image_size);
        mmap[i_mmap].Attribute= 0; /* XXX */

        /* Update the multiboot tag. */
        m->tag->mod_start=
            (multiboot_uint64_t)m->image_address;
        m->tag->mod_end=
            (multiboot_uint64_t)(m->image_address +
                                 (m->image_size - 1));

        if(!debug_details) {
            printf("ELF %.*s %luB @ 0x%lx\n",
                   (int)m->path_len,
                   config_raw + m->path_start,
                   m->image_size,
                   m->image_address);
        }
    }

    /* Update the first physical region, to exclude everthing we've just
     * allocated. */
    uint64_t space_used= allocbase - mmap[first_region].PhysicalStart;
    mmap[first_region].PhysicalStart= allocbase;
    mmap[first_region].VirtualStart= allocbase;
    mmap[first_region].NumberOfPages-= roundpage(space_used);

    /* Load the shim at the beginning of the updated free region: this way the
     * CPU driver will simply reuse the memory, without needing to know that
     * the shim was ever used. */

    /* Load the ELF. */
    size_t shim_size, shim_raw_alloc;
    void *shim_raw= load_file(shim_path, &shim_size, &shim_raw_alloc);
    Elf *shim_elf= elf_memory((char *)shim_raw, shim_size);
    if(!shim_elf) elf_fail("elf_memory");

    /* Relocate and initialise the shim. n.b. it jumps to the *physical*
     * kernel entry point. */
    uint64_t shim_loaded_size, shim_alloc, shim_entry;
    uint64_t shim_base= mmap[first_region].PhysicalStart;
    void *shim=
        load_shim(shim_elf, shim_raw, shim_size, shim_base,
                  &shim_loaded_size, &shim_alloc, kernel_table,
                  /* Stack must be 16-byte aligned. */
                  config->kernel_stack + kernel_stack_alloc - 16,
                  multiboot + KERNEL_OFFSET,
                  cpudriver_entry - KERNEL_OFFSET, &shim_entry,
                  debug_details);

    /* Print the memory map. */
    if(!debug_details) print_mmap(mmap, mmap_len);

    FILE *outfile;

    /* Open the output file for writing. */
    outfile= fopen(out_path, "w");
    if(!outfile) fail("fopen");

    size_t image_size= 0;

    /* Write the loaded & relocated CPU driver. */
    if(!debug_details) {
        if(fwrite(cpudriver, 1, cpudriver_alloc, outfile) !=
           cpudriver_alloc) {
            fail("fwrite");
        }
    }
    image_size+= cpudriver_alloc;

    /* Write the (empty) kernel stack. */
    void *stack= calloc(1, PAGE_4k);
    if(!stack) fail("calloc");
    for(size_t i= 0; i < roundpage(config->stack_size); i++) {
        if(!debug_details) {
            if(fwrite(stack, 1, PAGE_4k, outfile) != PAGE_4k) {
                fail("fwrite");
            }
        }
        image_size+= PAGE_4k;
    }
    free(stack);

    /* Write the root page table. */
    if(!debug_details) {
        if(fwrite(kernel_pt, 1, kernel_pt_size, outfile) != kernel_pt_size) {
            fail("fwrite");
        }
    }
    image_size+= kernel_pt_size;

    /* Write the multiboot header (including the memory map). */
    if(!debug_details) {
        if(fwrite(config->multiboot, 1, config->multiboot_alloc, outfile)
                != config->multiboot_alloc) {
            fail("fwrite");
        }
    }
    image_size+= config->multiboot_alloc;

    /* Write the kernel ELF. */
    if(!debug_details) {
        if(fwrite(config->kernel->image, 1,
                  config->kernel->alloc_size, outfile)
                != config->kernel->alloc_size) {
            fail("fwrite");
        }
    }
    image_size+= config->kernel->alloc_size;

    /* Write all module ELFs. */
    for(m= config->first_module; m; m= m->next) {
        if(!debug_details) {
            if(fwrite(m->image, 1, m->alloc_size, outfile) != m->alloc_size) {
                fail("fwrite");
            }
        }
        image_size+= m->alloc_size;
    }

    /* Write the loaded shim. */
    if(!debug_details) {
        if(fwrite(shim, 1, shim_loaded_size, outfile) != shim_loaded_size) {
            fail("fwrite");
        }
    }
    image_size+= shim_loaded_size;

    if(!debug_details) {
        printf("Load address: 0x%lx\n", loadbase);
        printf("Image size (bytes): %lu\n", image_size);
        printf("Entry point: 0x%lx\n", shim_entry);
    }

    if(debug_details) {
        fprintf(outfile, "load_address\t0x%lx\n", loadbase);
        fprintf(outfile, "shim_address\t0x%lx\n", shim_base);
        fprintf(outfile, "vminit_address\t0x%lx\n", kernel_start);
        fprintf(outfile, "cpudriver_address\t0x%lx\n",
                         KERNEL_OFFSET + kernel_start);
        fprintf(outfile, "entry_point\t0x%lx\n", shim_entry);
    }

    if(fclose(outfile)) fail("fclose");

    return EXIT_SUCCESS;
}
