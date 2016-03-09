#include <sys/types.h>
#include <sys/stat.h>

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <libfdt.h>

#define KERNEL_OFFSET 0xffff000000000000

#include "build_multiboot.h"
#include "config.h"
#include "efi.h"
#include "util.h"

void usage(char *name) {
    fprintf(stderr, "usage: %s <config> <fdt blob> <shim image> <fs root>"
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

void fdt_fail(char *name, int fdt_err) {
    fprintf(stderr, "%s: %s\n", name, fdt_strerror(fdt_err));
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

/* Locate RAM regions, using the FDT. */
fdt32_t *
fdt_regions(void *fdt, uint32_t *n_addr_cells,
            uint32_t *n_size_cells, size_t *mmap_len,
            size_t freespace) {
    int fdt_err;

    /* Get the size of addresses (in 32-bit cells) from the root node. */
    fdt32_t *p_addr_cells=(fdt32_t *)
        fdt_getprop(fdt, 0, "#address-cells", &fdt_err);
    if(!p_addr_cells) fdt_fail("fdt_getprop", fdt_err);
    *n_addr_cells= fdt32_to_cpu(*p_addr_cells);

    if(*n_addr_cells != 1 && *n_addr_cells != 2) {
        fprintf(stderr, "Don't know how to deal with %dB values\n",
                        *n_addr_cells * 4);
        exit(EXIT_FAILURE);
    }

    fdt32_t *p_size_cells=(fdt32_t *)
        fdt_getprop(fdt, 0, "#size-cells", &fdt_err);
    if(!p_size_cells) fdt_fail("fdt_getprop", fdt_err);
    *n_size_cells= fdt32_to_cpu(*p_size_cells);

    if(*n_size_cells != 1 && *n_size_cells != 2) {
        fprintf(stderr, "Don't know how to deal with %dB values\n",
                        *n_size_cells * 4);
        exit(EXIT_FAILURE);
    }

    /* Find the memory node. */
    int mem_off= fdt_path_offset(fdt, "/memory");
    if(mem_off < 0) fdt_fail("fdt_path_offset", mem_off);

    /* Get the raw list of regions from the memory node. */
    int lenp;
    const struct fdt_property *p_mem_reg=
        fdt_get_property(fdt, mem_off, "reg", &lenp);
    if(!p_mem_reg) fdt_fail("fdt_get_property", lenp);
    
    size_t entry_size= *n_addr_cells + *n_size_cells;

    /* How many entries are there? */
    *mmap_len= fdt32_to_cpu(p_mem_reg->len) / (entry_size * sizeof(fdt32_t));

    /* Sanity check. */
    if(*mmap_len * (entry_size * sizeof(fdt32_t))
       != fdt32_to_cpu(p_mem_reg->len)) {
        fprintf(stderr, "len(/memory/reg) wasn't a multiple of %lu\n", 
                (entry_size * sizeof(fdt32_t)));
        exit(EXIT_FAILURE);
    }

    /* Allocate 'freespace' entries at the beginning, to be filled with
     * BF-specific entries later. */
    *mmap_len+= freespace;

    return (fdt32_t *)p_mem_reg->data;
}

/* Fill the preallocated EFI memory map in, using the given list of cells. */
efi_memory_descriptor *
build_efi_mmap(struct config *config, fdt32_t *cells,
               uint32_t n_addr_cells, uint32_t n_size_cells,
               size_t mmap_len, size_t freespace) {
    efi_memory_descriptor *mmap=
        (efi_memory_descriptor *)config->mmap_start;

    /* Write the tag. */
    config->mmap_tag->type= MULTIBOOT_TAG_TYPE_EFI_MMAP;
    config->mmap_tag->size= sizeof(struct multiboot_tag_efi_mmap) +
                            mmap_len * sizeof(efi_memory_descriptor);
    config->mmap_tag->descr_size= sizeof(efi_memory_descriptor);
    config->mmap_tag->descr_vers= 1;

    /* Parse the FDT region list and construct EFI entries. */
    for(size_t i= freespace; i < mmap_len; i++) {
        uint64_t base, size;

        /* Read the base address' first cell. */
        base= (uint64_t)fdt32_to_cpu(*cells);
        cells++;

        /* Maybe read the second. */
        if(n_addr_cells == 2) {
            base <<= 32;
            base+= (uint64_t)fdt32_to_cpu(*cells);
            cells++;
        }

        /* Read the size. */
        size= (uint64_t)fdt32_to_cpu(*cells);
        cells++;
        if(n_size_cells == 2) {
            size <<= 32;
            size+= (uint64_t)fdt32_to_cpu(*cells);
            cells++;
        }

        if((size & (PAGE_4k - 1)) != 0) {
            fprintf(stderr,
                "Size of region %016lx-%016lx isn't a multiple of 4096.\n",
                base, base + size);
            exit(EXIT_FAILURE);
        }

        mmap[i].Type=          EfiConventionalMemory;
        mmap[i].PhysicalStart= base;
        mmap[i].VirtualStart=  base;
        mmap[i].NumberOfPages= size / PAGE_4k;
        mmap[i].Attribute=     0; /* XXX - this should change. */
    }

    return mmap;
}

void
check_alloc(uint64_t allocbase, efi_memory_descriptor *mmap, size_t pr1) {
    if(allocbase >= mmap[pr1].PhysicalStart +
                    mmap[pr1].NumberOfPages * PAGE_4k) {
        fprintf(stderr, "Ran out of room in the first memory region.\n");
        fprintf(stderr, "Region: %lx-%lx, allocbase=%lx\n",
                mmap[pr1].PhysicalStart,
                mmap[pr1].PhysicalStart + mmap[pr1].NumberOfPages * PAGE_4k,
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

#define BIT(n) (1 << (n))
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
alloc_kernel_pt(void) {
    /* Allocate one L1 table. */
    union armv8_l1_entry *table=
        calloc(PTABLE_NUM_ENTRIES, PTABLE_ENTRY_SIZE);
    if(!table) fail("calloc");

    /* Map the first 512GB of physical addresses. */
    for(size_t i= 0; i < PTABLE_NUM_ENTRIES; i++) {
        table[i].block.type= ARMv8_Ln_BLOCK;
        table[i].block.ai=   0; /* Page type 0 */
        table[i].block.ns=   0;
        table[i].block.ap=   0; /* R/W EL1, no access EL0 */
        table[i].block.sh=   3; /* Inner-shareable, fully coherent */
        table[i].block.af=   1; /* Accessed/dirty - don't fault */
        table[i].block.ng=   0; /* Global mapping */
        table[i].block.base_address= i; /* PA = i << 30 */
        table[i].block.ch=   1; /* Contiguous, combine TLB entries */
        table[i].block.pxn=  0; /* Privileged executable. */
        table[i].block.xn=   1; /* Unprivileged non-executable. */
    }

    return (void *)table;
}

int
main(int argc, char *argv[]) {
    if(argc < 6 || argc > 7) usage(argv[0]);

    const char *config_path= argv[1],
               *fdt_path=    argv[2],
               *shim_path=   argv[3],
               *base_path=   argv[4],
               *out_path=    argv[5];

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

    /* Load the FDT blob. */
    size_t fdt_size, fdt_alloc;
    void *fdt= load_file(fdt_path, &fdt_size, &fdt_alloc);

    /* Sanity check. */
    int fdt_err= fdt_check_header(fdt);
    if(fdt_err != 0) fdt_fail("fdt_check_header", fdt_err);

    /* Parse the FDT until we know how many regions we have, but put off
     * building the EFI mmap until we've build the MB image. */
    size_t mmap_len, pr1= n_modules + 5;
    uint32_t n_addr_cells, n_size_cells;
    fdt32_t *region_cells=
        fdt_regions(fdt, &n_addr_cells, &n_size_cells, &mmap_len, pr1);

    if(mmap_len < 1) {
        fprintf(stderr, "No memory regions defined.\n");
        exit(EXIT_FAILURE);
    }

    /* Create the multiboot header, now that we know how big the memory map
     * needs to be.  */
    void *mb_header=
        create_multiboot2_info(config, kernel_elf,
                               mmap_len * sizeof(efi_memory_descriptor));
    if(!mb_header) {
        fprintf(stderr, "Couldn't build the multiboot header.\n");
        exit(EXIT_FAILURE);
    }

    /* Build the EFI memory map from FDT information.  We leave uninitialised
     * entries at the beginning for the kernel, all modules, the CPU driver's
     * loadable segment & stack, the MB header, and the boot page table. */
    efi_memory_descriptor *mmap=
        build_efi_mmap(config, region_cells, n_addr_cells,
                       n_size_cells, mmap_len, pr1);

    /* Start allocating at the beginning of the first physical region. */
    uint64_t allocbase= mmap[pr1].PhysicalStart;
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
    check_alloc(allocbase, mmap, pr1);
    mmap[0].Type= EfiBarrelfishCPUDriver;
    mmap[0].PhysicalStart= kernel_start;
    mmap[0].VirtualStart= kernel_start;
    mmap[0].NumberOfPages= roundpage(cpudriver_size);
    mmap[0].Attribute= 0; /* XXX */

    /* Now allocate the CPU driver's stack. */
    config->kernel_stack= allocbase;
    uint64_t kernel_stack_alloc= ROUNDUP(config->stack_size, PAGE_4k);
    allocbase= ROUNDUP(allocbase + kernel_stack_alloc, PAGE_4k);
    check_alloc(allocbase, mmap, pr1);
    mmap[1].Type= EfiBarrelfishCPUDriverStack;
    mmap[1].PhysicalStart= config->kernel_stack;
    mmap[1].VirtualStart= config->kernel_stack;
    mmap[1].NumberOfPages= roundpage(config->stack_size);
    mmap[1].Attribute= 0; /* XXX */

    /* Allocate one frame for the CPU driver's root page table.  The shim will
     * initialise this, not us. */
    uint64_t kernel_table= allocbase;
    mmap[2].Type= EfiBarrelfishBootPageTable;
    mmap[2].PhysicalStart= allocbase;
    mmap[2].VirtualStart= allocbase;
    mmap[2].NumberOfPages= 1;
    mmap[2].Attribute= 0; /* XXX */
    allocbase+= PAGE_4k;
    check_alloc(allocbase, mmap, pr1);

    /* Allocate space for the multiboot header. */
    uint64_t multiboot= allocbase;
    mmap[3].Type= EfiBarrelfishMultibootData;
    mmap[3].PhysicalStart= allocbase;
    mmap[3].VirtualStart= allocbase;
    mmap[3].NumberOfPages= roundpage(config->multiboot_size);
    mmap[3].Attribute= 0; /* XXX */
    allocbase= ROUNDUP(allocbase + config->multiboot_size, PAGE_4k);
    check_alloc(allocbase, mmap, pr1);

    /* Allocate room for the CPU driver ELF. */
    config->kernel->image_address= allocbase;
    allocbase= ROUNDUP(allocbase + config->kernel->image_size, PAGE_4k);
    check_alloc(allocbase, mmap, pr1);
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
        check_alloc(allocbase, mmap, pr1);
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
    uint64_t space_used= allocbase - mmap[pr1].PhysicalStart;
    mmap[pr1].PhysicalStart= allocbase;
    mmap[pr1].VirtualStart= allocbase;
    mmap[pr1].NumberOfPages-= roundpage(space_used);

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
    uint64_t shim_base= mmap[pr1].PhysicalStart;
    void *shim=
        load_shim(shim_elf, shim_raw, shim_size, shim_base,
                  &shim_loaded_size, &shim_alloc, kernel_table,
                  config->kernel_stack + kernel_stack_alloc - 8,
                  multiboot, cpudriver_entry - KERNEL_OFFSET, &shim_entry,
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
        void *kernel_pt= alloc_kernel_pt();
        if(fwrite(kernel_pt, 1, PAGE_4k, outfile) != PAGE_4k) {
            fail("fwrite");
        }
    }
    image_size+= PAGE_4k;

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
        fprintf(outfile, "cpudriver_address\t0x%lx\n", kernel_start);
        fprintf(outfile, "entry_point\t0x%lx\n", shim_entry);
    }

    if(fclose(outfile)) fail("fclose");

    return EXIT_SUCCESS;
}
