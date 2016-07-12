#include <sys/stat.h>
#include <sys/types.h>

#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <libelf.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../../include/grubmenu.h"

#define DEBUG

#ifdef DEBUG
#define DBG(format, ...) printf(format, ## __VA_ARGS__)
#else
#define DBG(format, ...)
#endif

static uint32_t phys_alloc_start;

static uint32_t
round_up(uint32_t x, uint32_t y) {
    assert(y > 0);
    uint32_t z= x + (y - 1);
    return z - (z % y);
}

static uint32_t
align_alloc(uint32_t align) {
    phys_alloc_start= round_up(phys_alloc_start, align);
    return phys_alloc_start;
}

static uint32_t
phys_alloc(size_t size, size_t align) {
    align_alloc(align);
    uint32_t addr= phys_alloc_start;
    phys_alloc_start+= size;
    return addr;
}

void
fail(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

static void
fail_errno(const char *s) {
    perror(s);
    exit(EXIT_FAILURE);
}

static void
fail_elf(const char *s) {
    fprintf(stderr, "%s: %s\n", s, elf_errmsg(elf_errno()));
    exit(EXIT_FAILURE);
}

static void
do_write(int fd, void *src, size_t towrite) {
    while(towrite > 0) {
        ssize_t written= write(fd, src, towrite);
        if(written < 0) fail_errno("write");
        src+= written;
        towrite-= written;
    }
}

#define SEGMENT_ALIGN 8

static void *
load(int in_fd, size_t *loaded_size, uint32_t *entry_reloc,
     uint32_t *loaded_base, uint32_t offset) {
    Elf *elf= elf_begin(in_fd, ELF_C_READ, NULL);
    if(!elf) fail_elf("elf_begin");

    Elf32_Ehdr *ehdr= elf32_getehdr(elf);
    if(!ehdr) fail_elf("elf32_getehdr");
    uint32_t entry= ehdr->e_entry;

    size_t hdrsz;
    int phnum= elf_getphnum(elf, &hdrsz);
    if(phnum == 0) fail_elf("elf_getphnum");

    Elf32_Phdr *ph= elf32_getphdr(elf);
    if(!ph) fail_elf("elf_getphdr");

    printf("%d program segments.\n", phnum);

    /* Grab the ELF data. */
    size_t elfsize;
    void *elfdata= elf_rawfile(elf, &elfsize);
    if(!elfdata) fail_elf("elf_rawfile");

    /* We need the dynamic relocation section, the string table, and the
     * dynamic symbol table. */
    Elf32_Shdr *shdr_rel= NULL, *shdr_sym= NULL;

    Elf_Scn *scn;
    for(scn= elf_nextscn(elf, NULL); scn; scn= elf_nextscn(elf, scn)) {

        Elf32_Shdr *shdr= elf32_getshdr(scn);
        if(!shdr) fail_elf("elf_getshdr");

        switch(shdr->sh_type) {
            case SHT_REL:
                if(shdr_rel) fail("Found two relocation tables.\n");
                shdr_rel= shdr;
                break;
            case SHT_DYNSYM:
                if(shdr_sym) fail("Found two dynamic symbol tables.\n");
                shdr_sym= shdr;
                break;
            case SHT_RELA:
                fail("Didn't expect a RELA section.\n");
        }
    }

    if(!shdr_rel) fail("Didn't find a relocation table.\n");
    if(!shdr_sym) fail("Didn't find a dynamic symbol table.\n");

    /* Find the got_base symbol, and its unrelocated address. */
    uint32_t got_base, got_base_reloc;
    int got_symidx= -1;
    {
        /* The dynamic symbol table should link to the dynamic string table. */
        int dynstr_idx= shdr_sym->sh_link;
        if(dynstr_idx == 0) fail("No link to the string table.\n");
        Elf_Scn *scn_str= elf_getscn(elf, dynstr_idx);
        if(!scn_str) fail_elf("elf_getscn");
        Elf32_Shdr *shdr_str= elf32_getshdr(scn_str);
        if(!shdr_str) fail_elf("elf32_getshdr");
        if(shdr_str->sh_type != SHT_STRTAB)
            fail("Invalid dynamic string table.\n");
        char *str_base= (char *)elfdata + shdr_str->sh_offset;

        size_t entries= shdr_sym->sh_size / shdr_sym->sh_entsize;
        void *base= elfdata + shdr_sym->sh_offset;
        for(size_t i= 0; i < entries; i++) {
            Elf32_Sym *sym=
                (Elf32_Sym *)(base + i * shdr_sym->sh_entsize);
            if(!strcmp("got_base", str_base + sym->st_name)) {
                printf("Found got_base at %08x\n", sym->st_value);
                got_base= sym->st_value;
                /* Remember the symbol index, so we can spot the got
                 * relocation later. */
                got_symidx= i;
            }
        }
    }
    if(got_symidx == -1) fail("No got_base symbol.\n");

    uint32_t *segment_base= malloc(phnum * sizeof(uint32_t));
    if(!segment_base) fail_errno("malloc");

    size_t *segment_offset= malloc(phnum * sizeof(uint32_t));
    if(!segment_offset) fail_errno("malloc");

    size_t total_size= 0;
    void *loaded_image= NULL;

    uint32_t alloc_base= align_alloc(SEGMENT_ALIGN);
    *loaded_base= alloc_base;

    int found_got_base= 0, found_entry= 0;
    for(size_t i= 0; i < phnum; i++) {
        if(ph[i].p_type == PT_LOAD) {
            /* Allocate target memory. */
            //printf("%d\n", ph[i].p_align);
            //assert(ph[i].p_align <= SEGMENT_ALIGN);
            uint32_t base= phys_alloc(ph[i].p_memsz, ph[i].p_align);
            printf("Allocated %dB at VA %08x (PA %08x) for segment %d\n",
                   ph[i].p_memsz, base + offset, base, i);

            /* Record the relocated base address of the segment. */
            segment_base[i]= base + offset;
            segment_offset[i]= base - alloc_base;

            if(ph[i].p_vaddr <= got_base &&
               (got_base - ph[i].p_vaddr) < ph[i].p_memsz) {
                got_base_reloc = base + (got_base - ph[i].p_vaddr);
                printf("got_base is in segment %d, relocated %08x to VA %08x\n",
                       i, got_base, got_base_reloc + offset);
                found_got_base= 1;
            }

            if(ph[i].p_vaddr <= entry &&
               (entry - ph[i].p_vaddr) < ph[i].p_memsz) {
                *entry_reloc = base + (entry - ph[i].p_vaddr);
                printf("entry is in segment %d, relocated %08x to VA %08x\n",
                       i, entry, *entry_reloc + offset);
                found_entry= 1;
            }

            /* Enlarge our buffer. */
            total_size= segment_offset[i] + ph[i].p_memsz;
            loaded_image= realloc(loaded_image, total_size);
            if(!loaded_image) fail_errno("realloc");
            bzero(loaded_image + segment_offset[i], ph[i].p_memsz);

            if(ph[i].p_offset + ph[i].p_filesz > elfsize) {
                fail("Segment extends outside file.\n");
            }

            /* Copy it to the buffer. */
            memcpy(loaded_image + segment_offset[i],
                   elfdata + ph[i].p_offset,
                   ph[i].p_filesz);
        }
        else {
            printf("Segment %d is non-loadable.\n", i);
        }
    }
    if(!found_got_base) fail("got_base not in any loadable segment.\n");
    if(!found_entry)    fail("entry point not in any loadable segment.\n");

    printf("Total loaded size is %dB\n", total_size);
    *loaded_size= total_size;

    /* Now that all segments have been allocated, apply relocations. */
    {
        size_t entries= shdr_rel->sh_size / shdr_rel->sh_entsize;
        void *base= elfdata + shdr_rel->sh_offset;
        for(size_t i= 0; i < entries; i++) {
            Elf32_Rel *rel=
                (Elf32_Rel *)(base + i * shdr_rel->sh_entsize);

            int sym= ELF32_R_SYM(rel->r_info),
                typ= ELF32_R_TYPE(rel->r_info);

            /* Figure out which segment this relocation lies in. */
            size_t i;
            for(i= 0; i < phnum; i++) {
                /* Process the relocation if it's in a loadable segment,
                 * otherwise ignore it. */
                if(ph[i].p_vaddr <= rel->r_offset &&
                   rel->r_offset < ph[i].p_vaddr + ph[i].p_memsz) {
                    if(ph[i].p_type == PT_LOAD) break;
                }
            }
            if(i == phnum) {
                printf("Ignoring relocation at %08x outside all "
                       "loadable segments.\n", rel->r_offset);
                break;
            }

            /* Find the value to relocate. */
            uint32_t offset_within_segment= rel->r_offset - ph[i].p_vaddr;
            void *segment= loaded_image + segment_offset[i];
            uint32_t *value=
                (uint32_t *)(segment + offset_within_segment);

            /* There should be only section-relative relocations, except for
             * one absolute relocation for the GOT. */
            if(typ == R_ARM_RELATIVE && sym == 0) {
                /* Perform the relocation. */
                uint32_t reloc_offset= segment_base[i] - ph[i].p_vaddr;
                DBG("Rel @ %08x: %08x -> %08x\n",
                    rel->r_offset, *value, *value + reloc_offset);
                *value+= reloc_offset;
            }
            else if(typ == R_ARM_ABS32 && sym == got_symidx) {
                DBG("Rel @ %08x: %08x -> %08x\n",
                    rel->r_offset, *value, got_base_reloc);
                *value= got_base_reloc;
            }
            else fail("Invalid relocation at %08x, typ=%d, sym=%d\n",
                      rel->r_offset, typ, sym);
        }
    }

    if(elf_end(elf) < 0) fail_elf("elf_end");

    return loaded_image;
}

static char *strings;
static size_t strings_size= 0;

static void
init_strings(void) {
    strings_size= 1;
    strings= calloc(strings_size, 1);
    if(!strings) fail_errno("malloc");
}

static size_t
add_string(const char *s) {
    /* The new string begins just past the current end. */
    size_t start= strings_size;

    /* Extend the buffer. */
    strings_size+= strlen(s) + 1;
    strings= realloc(strings, strings_size);
    if(!strings) fail_errno("realloc");

    /* Copy the new string in. */
    strcpy(strings + start, s);

    /* Return the index of the new string. */
    return start;
}

static Elf32_Shdr *
add_image(Elf *elf, const char *name, void *image, size_t size,
          uint32_t vaddr) {
    /* Create the section. */
    Elf_Scn *scn= elf_newscn(elf);
    if(!scn) fail_elf("elf_newscn");

    /* Add the image as a new data blob. */
    Elf_Data *data= elf_newdata(scn);
    if(!data) fail_elf("elf_newdata");

    data->d_align=   1;
    data->d_buf=     image;
    data->d_off=     0;
    data->d_size=    size;
    data->d_type=    ELF_T_BYTE;
    data->d_version= EV_CURRENT;

    /* Initialise the section header. */
    Elf32_Shdr *shdr= elf32_getshdr(scn);
    if(!shdr) fail_elf("elf32_getshdr");

    if(name) shdr->sh_name= add_string(name);
    else     shdr->sh_name= 0;
    shdr->sh_type=  SHT_PROGBITS;
    shdr->sh_flags= SHF_WRITE | SHF_ALLOC | SHF_EXECINSTR;
    shdr->sh_addr=  vaddr;

    return shdr;
}

static void
add_strings(Elf *elf) {
    Elf_Scn *scn= elf_newscn(elf);
    if(!scn) fail_elf("elf_newscn");

    Elf_Data *data= elf_newdata(scn);
    if(!data) fail_elf("elf_newdata");

    data->d_align=   1;
    data->d_buf=     strings;
    data->d_off=     0;
    data->d_size=    strings_size;
    data->d_type=    ELF_T_BYTE;
    data->d_version= EV_CURRENT;

    /* Initialise the string table section header. */
    Elf32_Shdr *shdr= elf32_getshdr(scn);
    if(!shdr) fail_elf("elf32_getshdr");

    shdr->sh_name=    0;
    shdr->sh_type=    SHT_STRTAB;
    shdr->sh_flags=   SHF_STRINGS;

    elf_setshstrndx(elf, elf_ndxscn(scn));
};

int
main(int argc, char **argv) {
    char pathbuf[PATH_MAX+1];
    /* XXX - argument checking. */

    const char *menu_lst=   argv[1],
               *bootdriver= argv[2],
               *outfile=    argv[3],
               *buildroot=  argv[4];

    errno= 0;
    uint32_t kernel_base= strtoul(argv[5], NULL, 0);
    if(errno) fail_errno("strtoul");

    struct menu_lst *menu= read_menu_lst(menu_lst);

    /* Begin allocation at the start of the first MMAP entry. */
    if(menu->mmap_len == 0) fail("No MMAP.\n");
    if(menu->mmap[0].base > (uint64_t)UINT32_MAX)
        fail("This seems to be a 64-bit memory map.\n");
    uint32_t phys_base= (uint32_t)menu->mmap[0].base;
    uint32_t kernel_offset= kernel_base - phys_base;

    if(elf_version(EV_CURRENT) == EV_NONE)
        fail("ELF library version out of date.\n");

    phys_alloc_start= phys_base;
    printf("Beginning allocation at PA %08x (VA %08x)\n",
           phys_base, phys_base + kernel_offset);

    /* Open the boot driver ELF. */
    printf("Loading %s\n", bootdriver);
    int bd_fd= open(bootdriver, O_RDONLY);
    if(bd_fd < 0) fail_errno("open");

    /* Load and relocate it. */
    size_t bd_size;
    uint32_t bd_entry, bd_base;
    void *bd_image=
        load(bd_fd, &bd_size, &bd_entry, &bd_base, 0);

    printf("Boot driver entry point: PA %08x\n", bd_entry);

    /* Close the ELF. */
    if(close(bd_fd) < 0) fail_errno("close");

    /* Open the kernel ELF. */
    strcpy(pathbuf, buildroot);
    pathbuf[strlen(buildroot)]= '/';
    strcpy(pathbuf + strlen(buildroot) + 1, menu->kernel.path);
    printf("Loading %s\n", pathbuf);
    int cpu_fd= open(pathbuf, O_RDONLY);
    if(cpu_fd < 0) fail_errno("open");

    /* Load and relocate it. */
    size_t cpu_size;
    uint32_t cpu_entry, cpu_base;
    void *cpu_image=
        load(cpu_fd, &cpu_size, &cpu_entry, &cpu_base, kernel_offset);

    printf("CPU driver entry point: VA %08x\n", cpu_entry);

    /* Close the ELF. */
    if(close(cpu_fd) < 0) fail_errno("close");

    /*** Write the output file. ***/

    init_strings();

    /* Open the output image file. */
    printf("Writing to %s\n", outfile);
    int out_fd= open(outfile, O_WRONLY | O_CREAT | O_TRUNC);
    if(out_fd < 0) fail_errno("open");

    /* Create the output ELF file. */
    Elf *out_elf= elf_begin(out_fd, ELF_C_WRITE, NULL);
    if(!out_elf) fail_elf("elf_begin");

    /* Create the ELF header. */
    Elf32_Ehdr *out_ehdr= elf32_newehdr(out_elf);
    if(!out_ehdr) fail_elf("elf32_newehdr");

    /* Little-endian ARM executable. */
    out_ehdr->e_ident[EI_DATA]= ELFDATA2LSB;
    out_ehdr->e_type=           ET_EXEC;
    out_ehdr->e_machine=        EM_ARM;
    out_ehdr->e_entry=          bd_entry;

    /* Create a single program header (segment) to cover everything that we
     * need to load. */
    Elf32_Phdr *out_phdr= elf32_newphdr(out_elf, 1);
    if(!out_phdr) fail_elf("elf32_newphdr");

    /* The boot driver, CPU driver and multiboot image all get their own
     * sections. */
    Elf32_Shdr *bd_shdr=
        add_image(out_elf, "bootdriver", bd_image, bd_size, bd_base);
    Elf32_Shdr *cpu_shdr=
        add_image(out_elf, "cpudriver", cpu_image, cpu_size, cpu_base);

    /* Add the string table. */
    add_strings(out_elf);

    /* Lay the file out, and calculate offsets. */
    if(elf_update(out_elf, ELF_C_NULL) < 0) fail_elf("elf_update");

    out_phdr->p_type=   PT_LOAD;
    out_phdr->p_offset= out_ehdr->e_phoff;
    out_phdr->p_filesz= elf32_fsize(ELF_T_PHDR, 1, EV_CURRENT);
    out_phdr->p_offset= bd_shdr->sh_offset;
    out_phdr->p_vaddr=  bd_base;
    out_phdr->p_paddr=  bd_base;
    out_phdr->p_memsz=  bd_size + cpu_size;
    out_phdr->p_filesz= bd_size + cpu_size;
    out_phdr->p_flags=  PF_X | PF_W | PF_R;

    elf_flagphdr(out_elf, ELF_C_SET, ELF_F_DIRTY);

    /* Write the file. */
    if(elf_update(out_elf, ELF_C_WRITE) < 0) fail_elf("elf_update");

    if(elf_end(out_elf) < 0) fail_elf("elf_update");
    free(bd_image);
    if(close(out_fd) < 0) fail_errno("close");

    return EXIT_SUCCESS;
}
