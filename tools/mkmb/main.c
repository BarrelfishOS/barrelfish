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
    comp->image= load_file(path, &comp->image_size);

    return 0;
}

void
print_mmap(efi_memory_descriptor *mmap, size_t mmap_len) {
    size_t i;

    printf("EFI Memory Map:\n");
    for(i= 0; i < mmap_len; i++) {
        printf("%016lx-%016lx    ",
               mmap[i].PhysicalStart,
               mmap[i].PhysicalStart + (mmap[i].NumberOfPages * PAGE_4k) - 1);
        switch(mmap[i].Type) {
            case EfiConventionalMemory:
                printf("EfiConventionalMemory");
                break;
            case EfiBarrelfishCPUDriver:
                printf("EfiBarrelfishCPUDriver");
                break;
            case EfiBarrelfishCPUDriverStack:
                printf("EfiBarrelfishCPUDriverStack");
                break;
            case EfiBarrelfishMultibootData:
                printf("EfiBarrelfishMultibootData");
                break;
            case EfiBarrelfishELFData:
                printf("EfiBarrelfishELFData");
                break;
            case EfiBarrelfishBootPageTable:
                printf("EfiBarrelfishBootPageTable");
                break;
            default:
                printf("Unrecognised");
        }
        printf(" (%lukB)\n", mmap[i].NumberOfPages * 4);
    }
}

/* Build an EFI memory map from the list of memory regions given in an FDT
 * blob. */
efi_memory_descriptor *
build_efi_mmap(void *fdt, size_t *len, size_t freespace) {
    int fdt_err;

    /* Get the size of addresses (in 32-bit cells) from the root node. */
    fdt32_t *p_addr_cells=(fdt32_t *)
        fdt_getprop(fdt, 0, "#address-cells", &fdt_err);
    if(!p_addr_cells) fdt_fail("fdt_getprop", fdt_err);
    uint32_t n_addr_cells= fdt32_to_cpu(*p_addr_cells);

    if(n_addr_cells != 1 && n_addr_cells != 2) {
        fprintf(stderr, "Don't know how to deal with %dB values\n",
                        n_addr_cells * 4);
        exit(EXIT_FAILURE);
    }

    fdt32_t *p_size_cells=(fdt32_t *)
        fdt_getprop(fdt, 0, "#size-cells", &fdt_err);
    if(!p_size_cells) fdt_fail("fdt_getprop", fdt_err);
    uint32_t n_size_cells= fdt32_to_cpu(*p_size_cells);

    if(n_size_cells != 1 && n_size_cells != 2) {
        fprintf(stderr, "Don't know how to deal with %dB values\n",
                        n_size_cells * 4);
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
    
    size_t entry_size= n_addr_cells + n_size_cells;

    /* How many entries are there? */
    size_t mmap_len=
        fdt32_to_cpu(p_mem_reg->len) / (entry_size * sizeof(fdt32_t));

    /* Sanity check. */
    if(mmap_len * (entry_size * sizeof(fdt32_t))
       != fdt32_to_cpu(p_mem_reg->len)) {
        fprintf(stderr, "len(/memory/reg) wasn't a multiple of %lu\n", 
                (entry_size * sizeof(fdt32_t)));
        exit(EXIT_FAILURE);
    }

    /* Allocate 'freespace' entries at the beginning, to be filled with
     * BF-specific entries later. */
    mmap_len+= freespace;

    /* Allocate the memory map. */
    efi_memory_descriptor *mmap=
        calloc(mmap_len, sizeof(efi_memory_descriptor));
    if(!mmap) fail("calloc");

    /* Parse the FDT region list and construct EFI entries. */
    fdt32_t *cells= (fdt32_t *)p_mem_reg->data;
    size_t i;
    for(i= freespace; i < mmap_len; i++) {
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

    *len= mmap_len;
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
    size_t config_size;
    char *config_raw= (char *)load_file(config_path, &config_size);

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
    size_t fdt_size;
    void *fdt= load_file(fdt_path, &fdt_size);

    /* Sanity check. */
    int fdt_err= fdt_check_header(fdt);
    if(fdt_err != 0) fdt_fail("fdt_check_header", fdt_err);

    /* Build the EFI memory map from FDT information.  We leave uninitialised
     * entries at the beginning for the kernel, all modules, the CPU driver's
     * loadable segment & stack, the MB header, and the boot page table. */
    size_t mmap_len, pr1= n_modules + 5;
    efi_memory_descriptor *mmap= build_efi_mmap(fdt, &mmap_len, pr1);

    if(mmap_len < 1) {
        fprintf(stderr, "No memory regions defined.\n");
        exit(EXIT_FAILURE);
    }

    /* Create the multiboot header. */
    void *mb_header=
        create_multiboot2_info(config, kernel_elf,
                               mmap_len * sizeof(efi_memory_descriptor));
    if(!mb_header) {
        fprintf(stderr, "Couldn't build the multiboot header.\n");
        exit(EXIT_FAILURE);
    }

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
    if(fwrite(config->multiboot, 1, config->multiboot_size, outfile)
            != config->multiboot_size) {
        fail("fwrite");
    }
    image_size+= config->multiboot_size;

    /* Write the kernel ELF. */
    if(fwrite(config->kernel->image, 1, config->kernel->image_size, outfile)
            != config->kernel->image_size) {
        fail("fwrite");
    }
    image_size+= config->kernel->image_size;

    /* Write all module ELFs. */
    for(m= config->first_module; m; m= m->next) {
        if(fwrite(m->image, 1, m->image_size, outfile) != m->image_size) {
            fail("fwrite");
        }
        image_size+= m->image_size;
    }

    printf("Image size (bytes): %lu\n", image_size);

    fclose(outfile);

    return EXIT_SUCCESS;
}
