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

#include "build_multiboot.h"
#include "config.h"
#include "efi.h"
#include "util.h"

void usage(char *name) {
    fprintf(stderr, "usage: %s <config> <fdt blob> <fs root>"
                    " <output image>\n", name);
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
    if(!realpath(basepath, path)) fail("relpath");

    /* Load the ELF */
    printf("Loading component %s\n", path);
    comp->image= load_file(path, &comp->image_size, &comp->alloc_size);

    return 0;
}

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

int
main(int argc, char *argv[]) {
    if(argc != 5) usage(argv[0]);

    const char *config_path= argv[1],
               *fdt_path=    argv[2],
               *base_path=   argv[3],
               *out_path=    argv[4];

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

    /* Find the CPU driver's loadable segment. */
    size_t phnum;
    if(elf_getphdrnum(kernel_elf, &phnum))
        elf_fail("elf_getphdrnum");
    Elf64_Phdr *phdrs= elf64_getphdr(kernel_elf);
    if(!phdrs) elf_fail("elf64_getphdr");
    size_t kseg;
    for(kseg= 0; phdrs[kseg].p_type != PT_LOAD && kseg < phnum; kseg++);
    if(phdrs[kseg].p_type != PT_LOAD) {
        fprintf(stderr, "No loadable segments in CPU driver ELF.\n");
        exit(EXIT_FAILURE);
    }

    /* Fill the first MMAP entry with this segment. */
    mmap[0].Type= EfiBarrelfishCPUDriver;
    mmap[0].PhysicalStart= phdrs[kseg].p_paddr;
    mmap[0].VirtualStart= phdrs[kseg].p_vaddr;
    mmap[0].NumberOfPages= roundpage(phdrs[kseg].p_memsz);
    mmap[0].Attribute= 0; /* XXX */

    /* Ensure that the segment actually lies in the first region. */
    if(mmap[0].PhysicalStart < mmap[pr1].PhysicalStart ||
       mmap[0].PhysicalStart + mmap[0].NumberOfPages * PAGE_4k >
       mmap[pr1].PhysicalStart + mmap[pr1].NumberOfPages * PAGE_4k) {
        fprintf(stderr, "CPU driver's loadable segment lies outside the"
                        " first physical region.\n");
        exit(EXIT_FAILURE);
    }

    /* Start allocating from the end of the loadable segment. n.b. anything
     * *before* the CPU driver is lost. */
    uint64_t allocbase= 
       mmap[0].PhysicalStart + mmap[0].NumberOfPages * PAGE_4k;

    /* Now allocate the CPU driver's stack. */
    config->kernel_stack= allocbase;
    allocbase= ROUNDUP(allocbase + config->stack_size, PAGE_4k);
    check_alloc(allocbase, mmap, pr1);
    mmap[1].Type= EfiBarrelfishCPUDriverStack;
    mmap[1].PhysicalStart= config->kernel_stack;
    mmap[1].VirtualStart= config->kernel_stack;
    mmap[1].NumberOfPages= roundpage(config->stack_size);
    mmap[1].Attribute= 0; /* XXX */

    /* Allocate one frame for the CPU driver's root page table.  The shim will
     * initialise this, not us. */
    mmap[2].Type= EfiBarrelfishBootPageTable;
    mmap[2].PhysicalStart= allocbase;
    mmap[2].VirtualStart= allocbase;
    mmap[2].NumberOfPages= 1;
    mmap[2].Attribute= 0; /* XXX */
    allocbase+= PAGE_4k;
    check_alloc(allocbase, mmap, pr1);

    /* Allocate space for the multiboot header. */
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

    printf("ELF %.*s %luB @ 0x%lx\n",
           (int)config->kernel->path_len,
           config_raw + config->kernel->path_start,
           config->kernel->image_size,
           config->kernel->image_address);

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

        printf("ELF %.*s %luB @ 0x%lx\n",
               (int)m->path_len,
               config_raw + m->path_start,
               m->image_size,
               m->image_address);
    }

    /* Update the first physical region, to exclude everthing we've just
     * allocated. */
    uint64_t space_used= allocbase - mmap[pr1].PhysicalStart;
    mmap[pr1].PhysicalStart= allocbase;
    mmap[pr1].VirtualStart= allocbase;
    mmap[pr1].NumberOfPages-= roundpage(space_used);

    /* Print the memory map. */
    print_mmap(mmap, mmap_len);

    /* Open the output file for writing. */
    FILE *outfile= fopen(out_path, "w");
    if(!outfile) fail("fopen");

    /* We only output data for the multiboot header onward - The CPU driver
     * segment is loaded by the simulator, the stack is uninitialised, and
     * the boot page table is constructed by the shim. */
    printf("Image load address (MB header): 0x%lx\n", mmap[3].PhysicalStart);

    size_t image_size= 0;

    /* Write the multiboot header (including the memory map). */
    if(fwrite(config->multiboot, 1, config->multiboot_alloc, outfile)
            != config->multiboot_alloc) {
        fail("fwrite");
    }
    image_size+= config->multiboot_size;

    /* Write the kernel ELF. */
    if(fwrite(config->kernel->image, 1, config->kernel->alloc_size, outfile)
            != config->kernel->alloc_size) {
        fail("fwrite");
    }
    image_size+= config->kernel->alloc_size;

    /* Write all module ELFs. */
    for(m= config->first_module; m; m= m->next) {
        if(fwrite(m->image, 1, m->alloc_size, outfile) != m->alloc_size) {
            fail("fwrite");
        }
        image_size+= m->alloc_size;
    }

    printf("Image size (bytes): %lu\n", image_size);

    fclose(outfile);

    return EXIT_SUCCESS;
}
