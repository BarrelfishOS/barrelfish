/*
 * An EFI loader for Barrelfish
 *
 * This is an EFI app which loads the Multiboot2 image and execute
 * the bootloader.
 * This object file is linked together with the Multiboot2 image into one object
 *
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>

#include <efi/efi.h>
#include <efi/efilib.h>
#include <multiboot2.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/arm_core_data.h>
#include "blob.h"
#include "vm.h"

typedef enum {
    EfiBarrelfishFirstMemType=      0x80000000,

    EfiBarrelfishCPUDriver=         0x80000000,
    EfiBarrelfishCPUDriverStack=    0x80000001,
    EfiBarrelfishMultibootData=     0x80000002,
    EfiBarrelfishELFData=           0x80000003,
    EfiBarrelfishBootPageTable=     0x80000004,
    EfiBarrelfishCoreData=          0x80000005,

    EfiBarrelfishMaxMemType
} EFI_BARRELFISH_MEMORY_TYPE;

static const char *mmap_types[] = {
    "reserved",
    "LD code",
    "LD data",
    "BS code",
    "BS data",
    "RS code",
    "RS data",
    "available",
    "unusable",
    "ACPI reclaim",
    "ACPI NVS",
    "MMIO",
    "ports",
    "PAL code",
    "persist"
};

static const char *bf_mmap_types[] = {
    "BF code",
    "BF stack",
    "BF multiboot",
    "BF module",
    "BF page table",
    "BF core data",
};

#define MAX_L1_TABLES 512
struct page_tables {
    size_t nL1;

    union aarch64_descriptor *L0_table;
    union aarch64_descriptor *L1_tables[MAX_L1_TABLES];
} page_tables;

struct config {
    struct multiboot_info *multiboot;
    struct multiboot_tag_efi_mmap *mmap_tag;
    struct multiboot_tag_string *cmd_tag;
    EFI_PHYSICAL_ADDRESS modules;
    EFI_VIRTUAL_ADDRESS boot_driver_entry;
    EFI_PHYSICAL_ADDRESS cpu_driver_entry;
    EFI_PHYSICAL_ADDRESS cpu_driver_stack;
    size_t cpu_driver_stack_size;
    struct page_tables *tables;
};

typedef void boot_driver(uint32_t magic, void *pointer);

extern char barrelfish_blob_start[1];

#define KERNEL_OFFSET       0xffff000000000000
#define KERNEL_STACK_SIZE   0x4000

#define ATTR_CACHED 0
#define ATTR_DEVICE 1

#define ROUND_UP(x, y) (((x) + ((y) - 1)) & ~((y) - 1))
#define COVER(x, y) (((x) + ((y)-1)) / (y))
#define ROUND_DOWN(x, y) (((x) / (y)) * (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define BLOB_ADDRESS(offset) (barrelfish_blob_start + (offset))

/* Copy a base+length string into a null-terminated string.  Destination
 * buffer must be large enough to hold the terminator i.e. n+1 characters. */
static inline void
ntstring(char *dest, const char *src, size_t len) {
    memcpy(dest, src, len);
    dest[len]= '\0';
}

#define MEM_MAP_SIZE 8192
char mmap[MEM_MAP_SIZE];
UINTN mmap_size, mmap_key, mmap_d_size;
UINT32 mmap_d_ver;

static EFI_STATUS
update_memory_map(void)
{
    EFI_STATUS status;
    size_t mmap_n_desc, i;

    /* Grab the current table from UEFI. */
    mmap_size = MEM_MAP_SIZE;
    status = ST->BootServices->GetMemoryMap(
        &mmap_size,
        (void *) &mmap,
        &mmap_key,
        &mmap_d_size,
        &mmap_d_ver
    );
    if (status == EFI_BUFFER_TOO_SMALL) {
        Print(L"The memory map is %dB, but MEM_MAP_SIZE is %d.\n",
            mmap_size, MEM_MAP_SIZE);
        Print(L"This is compile-time limit in Hagfish - please report "
              L"this overflow, it's a bug.\n");
        return status;
    } else if (EFI_ERROR(status)) {
        Print(L"Unable to get memory map: %x\n", status);
        return status;
    }

    mmap_n_desc = mmap_size / mmap_d_size;

    return EFI_SUCCESS;
}

static EFI_STATUS
relocate_memory_map(void) {
    if (!mmap_size) {
        return EFI_LOAD_ERROR;
    }

    size_t mmap_n_desc = mmap_size / mmap_d_size;

    for (size_t i= 0; i < mmap_n_desc; i++) {
        EFI_MEMORY_DESCRIPTOR *desc =
            (EFI_MEMORY_DESCRIPTOR *)(mmap + i * mmap_d_size);
        // 1:1 mapping into kernel window
        desc->VirtualStart = desc->PhysicalStart + KERNEL_OFFSET;
    }

    return EFI_SUCCESS;
}

static void
print_memory_map(int update)
{
    if (update) {
        update_memory_map();
    }

    size_t mmap_n_desc = mmap_size / mmap_d_size;

    Print(L"Memory map at %lx, key: %x, descriptor version: %x\n",
            mmap, mmap_key, mmap_d_ver);
    Print(L"Got %d memory map entries of %dB (%dB).\n",
               mmap_n_desc, mmap_d_size, mmap_size);

    Print(L"Type          PStart           PEnd        "
               "      Size       Attributes\n");
    for (UINTN i = 0; i < mmap_n_desc; i++) {
        EFI_MEMORY_DESCRIPTOR *desc = ((void *) mmap) + (mmap_d_size * i);

        const char *description;
        if (desc->Type < EfiMaxMemoryType) {
            description= mmap_types[desc->Type];
        }
        else if (
            EfiBarrelfishFirstMemType <= desc->Type &&
            desc->Type < EfiBarrelfishMaxMemType
        ) {
            description = bf_mmap_types[desc->Type - EfiBarrelfishFirstMemType];
        }
        else {
            description= "???";
        }
        
        Print(L"%-13a %016lx %016lx %9ldkB %01x\n",
              description,
              desc->PhysicalStart,
              desc->PhysicalStart + (desc->NumberOfPages << 12) - 1,
              (desc->NumberOfPages << 12) / 1024,
              desc->Attribute);
    }
}

#define BLOCK_16G (ARMv8_HUGE_PAGE_SIZE * 16ULL)
#define BLOCK_16G_MASK (ARMv8_HUGE_PAGE_SIZE * 16ULL)

static EFI_STATUS
page_table_set_attr(struct config *cfg, uint64_t start, uint64_t end, uint64_t attr) {
    uint64_t base = ROUND_DOWN(start, ARMv8_HUGE_PAGE_SIZE)
            / ARMv8_HUGE_PAGE_SIZE;

    uint64_t last = COVER(end, ARMv8_HUGE_PAGE_SIZE);

    while (base < last) {
        size_t table_number = base >> ARMv8_BLOCK_BITS;
        size_t table_index = base & ARMv8_BLOCK_MASK;
        union aarch64_descriptor *desc =
            &cfg->tables->L1_tables[table_number][table_index];
        desc->block_l1.attrindex = attr;

        base++;
    }

    return EFI_SUCCESS;
}

static EFI_STATUS
build_page_tables(struct config *cfg) {
    EFI_STATUS status = EFI_SUCCESS;

    /* We need the current memory map to set memory attributes */
    status = update_memory_map();
    if (EFI_ERROR(status)) {
        Print(L"Failed to update memory map\n");
    }

    /* Page table book keeping in static buffer
     * so we don't need malloc & friends
     */
    cfg->tables = &page_tables;

    /* Map up to the highest RAM address supplied by EFI.  XXX - this is a
     * heuristic, and may fail.  Unless there's a more clever way to do
     * discovery, we might need to bite the bullet and map all 48 bits (2MB of
     * kernel page tables!).  All we really need is that the kernel gets all
     * RAM, and the debug serial port - it shouldn't actually touch anything
     * else. */
    uint64_t first_address, last_address;
    first_address = 0;
    last_address = ((1UL << 48) - 1);

    /* We will map in aligned 16G blocks, as each requires only one TLB
     * entry. */
    uint64_t window_start, window_length;
    window_start = first_address & ~BLOCK_16G;
    window_length = ROUND_UP(last_address - window_start, BLOCK_16G);

    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishBootPageTable,
        1,
        (EFI_PHYSICAL_ADDRESS *)&cfg->tables->L0_table
    );
    if (EFI_ERROR(status)) {
        Print(L"Failed to allocate L0 page table.\n");
        goto build_page_tables_fail;
    }
    memset(cfg->tables->L0_table, 0, BASE_PAGE_SIZE);

    /* Count the number of L1 tables (512GB) blocks required to cover the
     * physical mapping window. */
    cfg->tables->nL1 = 0;
    uint64_t L1base = window_start & ~ARMv8_TOP_TABLE_SIZE;
    uint64_t L1addr;
    for (L1addr = window_start & ~ARMv8_TOP_TABLE_SIZE;
        L1addr < window_start + window_length;
        L1addr += ARMv8_TOP_TABLE_SIZE) {
        cfg->tables->nL1++;
    }
    ASSERT(cfg->tables->nL1 <= MAX_L1_TABLES)

    /* Allocate the L1 tables.
     * We allocate them all in one big chunk
     * as otherwise the memory map size explodes
     */
    Print(L"Allocating %d L1 tables (%dB)\n", cfg->tables->nL1, cfg->tables->nL1 * BASE_PAGE_SIZE);
    EFI_PHYSICAL_ADDRESS L1_memory;
    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishBootPageTable,
        cfg->tables->nL1,
        &L1_memory
    );
    if (EFI_ERROR(status)) {
        Print(L"Failed to allocate L1 page tables.\n");
        goto build_page_tables_fail;
    }
    memset((void *)L1_memory, 0, cfg->tables->nL1 * BASE_PAGE_SIZE);
    Print(L"L1 tables start at 0x%lx\n", L1_memory);

    for (size_t i = 0; i < cfg->tables->nL1; i++) {
        cfg->tables->L1_tables[i] = (union aarch64_descriptor *)(L1_memory + i * BASE_PAGE_SIZE);

        /* Map the L1 into the L0. */
        size_t L0_index = (L1base >> ARMv8_TOP_TABLE_BITS) + i;
        cfg->tables->L0_table[L0_index].d.base =
            (uint64_t)cfg->tables->L1_tables[i] >> ARMv8_BASE_PAGE_BITS;
        cfg->tables->L0_table[L0_index].d.mb1 = 1; /* Page table */
        cfg->tables->L0_table[L0_index].d.valid = 1;
    }

    /* Install the 1GB block mappings. */
    uint64_t firstblock = window_start / ARMv8_HUGE_PAGE_SIZE;
    uint64_t nblocks = window_length / ARMv8_HUGE_PAGE_SIZE;
    for (uint64_t block = firstblock; block < firstblock + nblocks; block++) {
        size_t table_number= block >> ARMv8_BLOCK_BITS;
        size_t table_index= block & ARMv8_BLOCK_MASK;
        union aarch64_descriptor *desc =
            &cfg->tables->L1_tables[table_number][table_index];

        // We first map all block non-contiguous but later set the bit
        // if possible
        desc->block_l1.contiguous = 0;
        desc->block_l1.base = block;
        /* Mark the accessed flag, so we don't get a fault. */
        desc->block_l1.af = 1;
        /* Outer shareable - coherent. */
        desc->block_l1.sh = 3;
        /* EL1+ only. */
        desc->block_l1.ap = 0;
        // device memory by default
        desc->block_l1.attrindex = ATTR_DEVICE;
        /* A block. */
        desc->block_l1.mb0 = 0;
        desc->block_l1.valid = 1;
    }

    // set all memory regions to cached
    size_t mmap_n_desc = mmap_size / mmap_d_size;
    for (size_t i = 0; i < mmap_n_desc; i++) {
        EFI_MEMORY_DESCRIPTOR *desc = (void *) (mmap + i * mmap_d_size);
        /* We're only looking for MMIO. */
        if (desc->Type != EfiMemoryMappedIO
                && desc->Type != EfiMemoryMappedIOPortSpace) {
            page_table_set_attr(cfg, desc->PhysicalStart, desc->PhysicalStart + BASE_PAGE_SIZE * desc->NumberOfPages, ATTR_CACHED);
        }
    }

    // set the contiguous bit if possible
    for (uint64_t block = firstblock; block < firstblock + nblocks; block += 16) {
        BOOLEAN all_same = TRUE;
        uint64_t attr;
        for (uint64_t offset = 0; offset < 16; offset++) {
            size_t table_number = (block + offset) >> ARMv8_BLOCK_BITS;
            size_t table_index = (block + offset) & ARMv8_BLOCK_MASK;
            union aarch64_descriptor *desc =
                &cfg->tables->L1_tables[table_number][table_index];
            if (offset == 0) {
                attr = desc->block_l1.attrindex;
            }
            else if (attr != desc->block_l1.attrindex) {
                all_same = FALSE;
                break;
            }
        }
        if (all_same) {
            // all entries in current block have the same attrindex value
            // so we can set the contiguous bit
            for (uint64_t offset = 0; offset < 16; offset++) {
                size_t table_number = (block + offset) >> ARMv8_BLOCK_BITS;
                size_t table_index = (block + offset) & ARMv8_BLOCK_MASK;
                union aarch64_descriptor *desc =
                    &cfg->tables->L1_tables[table_number][table_index];
                desc->block_l1.contiguous = 1;
            }
        }
    }

    return EFI_SUCCESS;

build_page_tables_fail:
    if (cfg->tables) {
        if (cfg->tables->L1_tables) {
            size_t i;
            for (i= 0; i < cfg->tables->nL1; i++) {
                if (cfg->tables->L1_tables[i])
                    BS->FreePages((EFI_PHYSICAL_ADDRESS)cfg->tables->L1_tables[i], 1);
            }
        }
        if (cfg->tables->L0_table) {
            BS->FreePages((EFI_PHYSICAL_ADDRESS)cfg->tables->L0_table, 1);
        }
    }

    return status;
}

static void
relocate_elf(EFI_PHYSICAL_ADDRESS segment_start, uint64_t virtual_offset,
                 struct Blob_relocation *relocations,
                 uint64_t no_relocations)
{
    Print(L"Relocating ELF %lx %lx %d\n",
        segment_start, relocations, no_relocations);
    for (uint64_t i = 0; i < no_relocations; i++) {
        *(uint64_t *)(segment_start + relocations[i].offset) =
            segment_start + virtual_offset + relocations[i].addend;
    }
}

static EFI_STATUS
relocate_boot_driver(struct Blob *blob_info, struct config *cfg)
{
    EFI_STATUS status;

    /* Should be page aligend */
    ASSERT(blob_info->boot_driver_segment_size % BASE_PAGE_SIZE == 0);

    EFI_PHYSICAL_ADDRESS boot_driver;
    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishCPUDriver,
        blob_info->boot_driver_segment_size / BASE_PAGE_SIZE,
        &boot_driver
    );
    if (EFI_ERROR(status)) {
        Print(L"Error allocating memory for boot driver segment: %d\n", status);
        return status;
    }

    memcpy((void *)boot_driver, BLOB_ADDRESS(blob_info->boot_driver_segment), blob_info->boot_driver_segment_size);

    struct Blob_relocation *boot_driver_relocations =
        (struct Blob_relocation *)BLOB_ADDRESS(blob_info->boot_driver_relocations);
    relocate_elf(
        boot_driver,
        0,
        boot_driver_relocations,
        blob_info->boot_driver_relocations_count
    );

    cfg->boot_driver_entry = boot_driver + blob_info->boot_driver_entry;
    
    return EFI_SUCCESS;
}

static EFI_STATUS
relocate_cpu_driver(struct Blob *blob_info, struct config *cfg)
{
    EFI_STATUS status;

    /* Should be page aligend */
    ASSERT(blob_info->cpu_driver_segment_size % BASE_PAGE_SIZE == 0);

    EFI_PHYSICAL_ADDRESS cpu_driver;
    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishCPUDriver,
        blob_info->cpu_driver_segment_size / BASE_PAGE_SIZE,
        &cpu_driver
    );
    if (EFI_ERROR(status)) {
        Print(L"Error allocating memory for CPU driver segment: %d\n", status);
        return status;
    }

    memcpy((void *)cpu_driver, BLOB_ADDRESS(blob_info->cpu_driver_segment), blob_info->cpu_driver_segment_size);

    struct Blob_relocation *cpu_driver_relocations =
        (struct Blob_relocation *)BLOB_ADDRESS(blob_info->cpu_driver_relocations);
    relocate_elf(
        cpu_driver,
        KERNEL_OFFSET,
        cpu_driver_relocations,
        blob_info->cpu_driver_relocations_count
    );

    cfg->cpu_driver_entry = cpu_driver + blob_info->cpu_driver_entry + KERNEL_OFFSET;

    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishCPUDriverStack,
        KERNEL_STACK_SIZE / BASE_PAGE_SIZE,
        &cfg->cpu_driver_stack
    );
    if (EFI_ERROR(status)) {
        Print(L"Error allocating memory for CPU driver stack: %d\n", status);
        return status;
    }

    cfg->cpu_driver_stack_size = KERNEL_STACK_SIZE;

    Print(
        L"Relocated CPU driver entry point is %lx, stack at %lx\n",
        cfg->cpu_driver_entry,
        cfg->cpu_driver_stack
    );
    
    return EFI_SUCCESS;
}

static EFI_STATUS
relocate_modules(struct Blob *blob_info, struct config *cfg)
{
    EFI_STATUS status;
    
    /* Should be page aligend */
    ASSERT(blob_info->modules_size % BASE_PAGE_SIZE == 0);

    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishELFData,
        blob_info->modules_size / BASE_PAGE_SIZE,
        &cfg->modules
    );
    if (EFI_ERROR(status)) {
        Print(L"Error allocating memory for modules: %d\n", status);
        return status;
    }

    memcpy((void *)cfg->modules, BLOB_ADDRESS(blob_info->modules), blob_info->modules_size);
    return EFI_SUCCESS;
}

static EFI_STATUS
relocate_multiboot(struct Blob *blob_info, struct config *cfg)
{
    EFI_STATUS status;

    /* Should be page aligend */
    ASSERT(blob_info->multiboot_size % BASE_PAGE_SIZE == 0);

    EFI_PHYSICAL_ADDRESS memory;
    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishMultibootData,
        blob_info->multiboot_size / BASE_PAGE_SIZE,
        (EFI_PHYSICAL_ADDRESS *)&cfg->multiboot
    );
    if (EFI_ERROR(status)) {
        Print(L"Error allocating memory for multiboot info: %d\n", status);
        return status;
    }

    memcpy(cfg->multiboot, BLOB_ADDRESS(blob_info->multiboot), blob_info->multiboot_size);

    /* Module start & end pointed into the blob.
     * Now they need to point into the module region
     */
    uint64_t module_offset = (uint64_t)cfg->modules - blob_info->modules;

    Print(L"Relocating multiboot info: %lx\n", cfg->multiboot);
    /* We don't have an end tag yet, but EFI mmap tag is last */
    struct multiboot_tag *tag;
    for (tag = cfg->multiboot->tags; tag->type != MULTIBOOT_TAG_TYPE_EFI_MMAP; tag = (void *)tag + tag->size) {
        Print(L"%lx: tag %d:%d\n", tag, tag->type, tag->size);

        if (tag->type == MULTIBOOT_TAG_TYPE_MODULE_64) {
            struct multiboot_tag_module_64 *mtag = (struct multiboot_tag_module_64 *)tag;
            Print(L"\tbefore %lx:%lx\n", mtag->mod_start, mtag->mod_end);
            mtag->mod_start += module_offset;
            mtag->mod_end += module_offset;
            Print(L"\tafter  %lx:%lx\n", mtag->mod_start, mtag->mod_end);
        }
        else if (tag->type == MULTIBOOT_TAG_TYPE_CMDLINE) {
            cfg->cmd_tag = (struct multiboot_tag_string *)tag;
        }
    }

    ASSERT(tag->type == MULTIBOOT_TAG_TYPE_EFI_MMAP);
    cfg->mmap_tag = (struct multiboot_tag_efi_mmap *)tag;

    return EFI_SUCCESS;
}

static struct armv8_core_data *
create_core_data(struct config *cfg)
{
    EFI_STATUS status;

    struct armv8_core_data *core_data;
    status = BS->AllocatePages(
        AllocateAnyPages,
        EfiBarrelfishCoreData,
        1,
        (EFI_PHYSICAL_ADDRESS *)&core_data
    );
    if (EFI_ERROR(status)) {
        Print(L"Error allocating memory for core data: %d\n", status);
        return NULL;
    }

    memset(core_data, 0, BASE_PAGE_SIZE);

    core_data->boot_magic = ARMV8_BOOTMAGIC_BSP;
    core_data->cpu_driver_stack =
        (lpaddr_t)cfg->cpu_driver_stack + cfg->cpu_driver_stack_size - 16;
    core_data->cpu_driver_stack_limit = (lpaddr_t)cfg->cpu_driver_stack;
    core_data->cpu_driver_entry = (lvaddr_t)cfg->cpu_driver_entry;
    core_data->page_table_root = (genpaddr_t)cfg->tables->L0_table;
    ntstring(
        core_data->cpu_driver_cmdline,
        cfg->cmd_tag->string,
        MIN(
            cfg->cmd_tag->size - sizeof(struct multiboot_tag_string),
            sizeof(core_data->cpu_driver_cmdline) - 1
        )
    );
    
    core_data->multiboot_image.base = (lpaddr_t)cfg->multiboot;
    core_data->multiboot_image.length = cfg->multiboot->total_size;
    core_data->efi_mmap = (lpaddr_t)cfg->mmap_tag;

    return core_data;
}

EFI_STATUS EFIAPI efi_main(EFI_HANDLE ImageHandle,
                           EFI_SYSTEM_TABLE * SystemTable)
{
    EFI_STATUS status;

    InitializeLib(ImageHandle, SystemTable);
    
    struct Blob *blob_info = (struct Blob *)BLOB_ADDRESS(0);
    Print(L"Blob is at: 0x%lx\n", blob_info);
    Print(L"Magic: %lx\n", blob_info->magic);

    struct config cfg;

    status = relocate_boot_driver(blob_info, &cfg);
    if (EFI_ERROR(status)) {
        Print(L"Failed to relocate boot driver\n");
        return status;
    }

    status = relocate_cpu_driver(blob_info, &cfg);
    if (EFI_ERROR(status)) {
        Print(L"Failed to relocate CPU driver\n");
        return status;
    }

    status = relocate_modules(blob_info, &cfg);
    if (EFI_ERROR(status)) {
        Print(L"Failed to relocate modules\n");
        return status;
    }

    status = relocate_multiboot(blob_info, &cfg);
    if (EFI_ERROR(status)) {
        Print(L"Failed to relocate multiboot info\n");
        return status;
    }

    status = build_page_tables(&cfg);
    if (EFI_ERROR(status)) {
        Print(L"Failed to build page tables\n");
        return status;
    }

    struct armv8_core_data *core_data = create_core_data(&cfg);

    Print(L"Terminating boot services and jumping to image at 0x%lx\n", cfg.boot_driver_entry);
    Print(L"Core data pointer is %lx\n", core_data);

    print_memory_map(1);

    status = update_memory_map();
    if (EFI_ERROR(status)) {
        Print(L"Failed to update memory map\n");
    }

    status = ST->BootServices->ExitBootServices(ImageHandle, mmap_key);
    if (EFI_ERROR(status)) {
        Print(L"Error exiting boot services: %d, %x\n", status, mmap_key);
        return status;
    }

    /*** EFI boot services are now terminated, we're on our own. */
    status = relocate_memory_map();
    if (EFI_ERROR(status)) {
        return EFI_SUCCESS;
    }

    /* The last thing we do is complete the multiboot info:
     * Set the EFI mmap and end tag
     */
    cfg.mmap_tag->size = ROUND_UP(sizeof(struct multiboot_tag_efi_mmap) + mmap_size, 8);
    cfg.mmap_tag->descr_size = mmap_d_size;
    cfg.mmap_tag->descr_vers = mmap_d_ver;
    memcpy(cfg.mmap_tag->efi_mmap, mmap, mmap_size);

    struct multiboot_tag *end_tag = (void *)cfg.mmap_tag + cfg.mmap_tag->size;
    end_tag->type = MULTIBOOT_TAG_TYPE_END;
    end_tag->size = ROUND_UP(sizeof(struct multiboot_tag), 8);
    cfg.multiboot->total_size = (void *)end_tag + end_tag->size - (void *)cfg.multiboot;

    status = ST->RuntimeServices->SetVirtualAddressMap(
        mmap_size,
        mmap_d_size,
        mmap_d_ver,
        (void *) &mmap
    );
    if (EFI_ERROR(status)) {
        return status;
    }

    // Jump to the bootloader, the blob can be reused
    (*((boot_driver *) (cfg.boot_driver_entry))) (MULTIBOOT2_BOOTLOADER_MAGIC, core_data);

    return EFI_SUCCESS;
}
