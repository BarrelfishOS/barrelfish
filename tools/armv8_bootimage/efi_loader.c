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
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <efi/efi.h>
#include <efi/efilib.h>
#include "multiboot2.h"
#include "blob.h"

void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);

extern uint64_t barrelfish_blob_start[1];
extern unsigned char barrelfish_blob_end[1];

#define HIGH_MEMORY 0xffff000000000000

// Multiboot2 tags
struct Tag {
    uint32_t type;
    uint32_t size;
};

struct Module64_Tag {
    uint32_t type;
    uint32_t size;
    uint64_t mod_start;
    uint64_t mod_end;
    char cmdline[0];
};

struct Efi_Mmap_Tag {
    uint32_t type;
    uint32_t size;
    uint32_t descr_size;
    uint32_t descr_vers;
    uint8_t efi_mmap[0];
};

struct Efi64_Tag {
    uint32_t type;
    uint32_t size;
    uint64_t pointer;
};

#define MEM_MAP_SIZE 8192
char mmap[MEM_MAP_SIZE];
UINTN mmap_size, mmap_key, mmap_d_size;
UINT32 mmap_d_ver;

void get_memory_map(void)
{
    EFI_STATUS status;
    unsigned mmap_n_desc, i;

    /* Grab the current table from UEFI. */
    mmap_size = 8192;
    status = ST->BootServices->GetMemoryMap(&mmap_size, (void *) &mmap,
                                            &mmap_key, &mmap_d_size,
                                            &mmap_d_ver);
    if (status == EFI_BUFFER_TOO_SMALL) {
        Print(L"The memory map is %dB, but MEM_MAP_SIZE is %d.\n",
              mmap_size, MEM_MAP_SIZE);
        Print(L"This is compile-time limit in Hagfish - please report "
              L"this overflow, it's a bug.\n");
        return;
    } else if (EFI_ERROR(status)) {
        Print(L"GetMemoryMap: %r\n", status);
        return;
    }

    Print(L"Memory map at %lx, key: %x, descriptor version: %x\n",
          mmap, mmap_key, mmap_d_ver);
    mmap_n_desc = mmap_size / mmap_d_size;
    Print(L"Got %d memory map entries of %dB (%dB).\n",
          mmap_n_desc, mmap_d_size, mmap_size);

    Print
        (L"Type          VStart           PStart           PEnd      Attributes\n");
    for (i = 0; i < mmap_n_desc; i++) {
        EFI_MEMORY_DESCRIPTOR *desc = ((void *) mmap) + (mmap_d_size * i);
        const CHAR16 *description;
        Print(L"%3d %016lx %016lx %16lx %016lx\n",
              desc->Type, desc->VirtualStart,
              desc->PhysicalStart,
              desc->PhysicalStart + (desc->NumberOfPages << 12),
              desc->Attribute);
        desc->VirtualStart = desc->PhysicalStart;
    }
}

void relocateMultiboot(void *base, uint64_t offset,
                       uint64_t kernel_segment)
{
    uint32_t *magic = base + offset;
    struct Tag *tag;

    get_memory_map();

    Print(L"Relocating multiboot: %lx %08x %08x %08x %08x\n", magic,
          magic[0], magic[1], magic[2], magic[3]);
    tag = base + offset + 8;
    for (;;) {
        Print(L"%lx: tag %d:%d\n", tag, tag->type, tag->size);
        if (tag->type == 12) {
            struct Efi64_Tag *etag = (struct Efi64_Tag *) tag;
            Print(L"before %lx\n", etag->pointer);
            etag->pointer +=
                kernel_segment + (uint64_t) base + HIGH_MEMORY;
            Print(L"after %lx\n", etag->pointer);
        } else if (tag->type == 19) {
            struct Module64_Tag *mtag = (struct Module64_Tag *) tag;
            Print(L"\tbefore %lx:%lx\n", mtag->mod_start, mtag->mod_end);
            mtag->mod_start += (uint64_t) base;
            mtag->mod_end += (uint64_t) base;
            Print(L"\tafter  %lx:%lx\n", mtag->mod_start, mtag->mod_end);
        } else if (tag->type == 17) {
            struct Efi_Mmap_Tag *emtag = (struct Efi_Mmap_Tag *) tag;
            emtag->descr_size = mmap_d_size;
            emtag->descr_vers = mmap_d_ver;
            emtag->size = sizeof(struct Efi_Mmap_Tag) + mmap_size;
            memcpy((void *) emtag + sizeof(struct Efi_Mmap_Tag),
                   (uint64_t *) mmap, mmap_size);

            struct Tag *tag = (void *) emtag + emtag->size;
            tag->type = 0;
            tag->size = 8;
            uint32_t size = (void *) tag + 8 - base - offset;
            *(uint32_t *) (base + offset) = size;
            Print(L"Size: %x\n", size);
            break;
        }
        tag = (void *) tag + tag->size;
    }
}

void relocateElf(void *base, uint64_t offset, uint64_t virtual_offset,
                 struct Blob_relocation *relocations,
                 unsigned no_relocations)
{
    unsigned i;

    Print(L"Relocating ELF %lx %lx %lx %d\n", base, offset, relocations,
          no_relocations);
    for (i = 0; i < no_relocations; i++) {
        *(uint64_t *) (base + offset + relocations[i].offset) =
            relocations[i].addend + (uint64_t) base + offset +
            virtual_offset;
    }
}

typedef void boot_driver(uint32_t magic, void *pointer, void *stack_top);

// remove the no-execute protection from all pages
void remove_protection(void)
{
    uint64_t i, j, k, l;

    uint64_t el, ttbr0;
  __asm("mrs %0, currentel\n":"=r"(el));
    el = el / 4;

    if (el == 1) {
      __asm("mrs %0, ttbr0_el1\n":"=r"(ttbr0));
    } else {                    // 2
      __asm("mrs %0, ttbr0_el2\n":"=r"(ttbr0));
        // __asm("mrs %0, ttbr1_el2\n": "=r" (ttbr1));
    }
    uint64_t *table0, *table1, *table2, *table3;
    uint64_t offset0, offset1, offset2, offset3;

    table0 = (uint64_t *) ttbr0;
    for (i = 0; i < 512; i++) {
        if (table0[i]) {
            offset0 = i << 39;
            if (!(table0[i] & 2)) {
                table0[i] &= 0x0000ffffffffffff;
                table0[i] |= 0x0000000000000304;
                continue;
            }
            table1 = (uint64_t *) (table0[i] & 0x0000fffffffff000);
            for (j = 0; j < 512; j++) {
                if (table1[j]) {
                    offset1 = offset0 | (j << 30);
                    if (!(table1[j] & 2)) {
                        table1[j] &= 0x0000ffffffffffff;
                        table1[j] |= 0x0000000000000304;
                        continue;
                    }
                    table2 = (uint64_t *) (table1[j] & 0x0000fffffffff000);
                    for (k = 0; k < 512; k++) {
                        if (table2[k]) {
                            offset2 = offset1 | (k << 21);
                            if (!(table2[k] & 2)) {
                                table2[k] &= 0x0000ffffffffffff;
                                table2[k] |= 0x0000000000000304;
                                continue;
                            }
                            table3 =
                                (uint64_t *) (table2[k] &
                                              0x0000fffffffff000);
                            for (l = 0; l < 512; l++) {
                                if (table3[l]) {
                                    offset3 = offset2 | (l << 12);
                                    table3[l] &= 0x0000ffffffffffff;
                                    table3[l] |= 0x0000000000000304;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

void dump_pages(void)
{
    uint64_t i, j, k, l;

    uint64_t el, ttbr0;
  __asm("mrs %0, currentel\n":"=r"(el));
    el = el / 4;

    if (el == 1) {
      __asm("mrs %0, ttbr0_el1\n":"=r"(ttbr0));
    } else {                    // 2
      __asm("mrs %0, ttbr0_el2\n":"=r"(ttbr0));
    }
    Print(L"el:%ld  ttbr0:%lx\n", el, ttbr0);

    uint64_t *table0, *table1, *table2, *table3;
    uint64_t offset0, offset1, offset2, offset3;

    table0 = (uint64_t *) ttbr0;
    for (i = 0; i < 512; i++) {
        if (table0[i]) {
            offset0 = i << 39;
            Print(L"%016lx: %016lx\n", offset0, table0[i]);
            if (!(table0[i] & 2)) {
                continue;
            }
            table1 = (uint64_t *) (table0[i] & 0x0000fffffffff000);
            for (j = 0; j < 512; j++) {
                if (table1[j]) {
                    offset1 = offset0 | (j << 30);
                    Print(L"  %016lx: %016lx\n", offset1, table1[j]);
                    if (!(table1[j] & 2)) {
                        continue;
                    }
                    table2 = (uint64_t *) (table1[j] & 0x0000fffffffff000);
                    for (k = 0; k < 512; k++) {
                        if (table2[k]) {
                            offset2 = offset1 | (k << 21);
                            Print(L"    %016lx: %016lx\n", offset2,
                                  table2[k]);
                            if (!(table2[k] & 2)) {
                                continue;
                            }
                            table3 =
                                (uint64_t *) (table2[k] &
                                              0x0000fffffffff000);
                            for (l = 0; l < 512; l++) {
                                if (table3[l]) {
                                    offset3 = offset2 | (l << 12);
                                    Print(L"      %016lx: %016lx\n",
                                          offset3, table3[l]);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

EFI_STATUS EFIAPI efi_main(EFI_HANDLE ImageHandle,
                           EFI_SYSTEM_TABLE * SystemTable)
{
    uint64_t i, j, k, l;
    EFI_STATUS status;
    EFI_RUNTIME_SERVICES *RuntimeServices = SystemTable->RuntimeServices;
    EFI_GET_VARIABLE GetVariable = RuntimeServices->GetVariable;

    InitializeLib(ImageHandle, SystemTable);

    Print(L"Blob: %lx %lx\n", barrelfish_blob_start, barrelfish_blob_end);

    uint64_t blob_size =
        (void *) barrelfish_blob_end - (void *) barrelfish_blob_start;
    uint64_t blob_no_pages = (blob_size + 4095) / 4096;

    void *relocated_blob;
    status =
        BS->AllocatePages(AllocateAnyPages, EfiLoaderCode, blob_no_pages,
                          (uint64_t *) & relocated_blob);
    if (EFI_ERROR(status)) {
        Print(L"AllocatePages: ERROR %d, %x\n", status, mmap_key);
        return status;
    }
    Print(L"Blob  size:%ld  pages:%ld -> %lx %d\n", blob_size,
          blob_no_pages, relocated_blob);
    // move the blob to the unprotected region so we can execute it
    memcpy(relocated_blob, barrelfish_blob_start, blob_size);

    struct Blob *blob = (struct Blob *) relocated_blob;
    Print(L"Magic: %lx\n", blob->magic);
    Print(L"Multiboot: %lx\n", blob->multiboot);
    Print(L"Boot driver entry: %lx\n", blob->boot_driver_entry);
    Print(L"Boot driver segment: %lx\n", blob->boot_driver_segment);
    Print(L"Boot driver relocations: %lx\n",
          blob->boot_driver_relocations);
    Print(L"Boot driver relocations count: %lx\n",
          blob->boot_driver_relocations_count);
    Print(L"Kernel entry: %lx\n", blob->kernel_entry);
    Print(L"Kernel segment: %lx\n", blob->kernel_segment);
    Print(L"Kernel relocations: %lx\n", blob->kernel_relocations);
    Print(L"Kernel relocations count: %lx\n",
          blob->kernel_relocations_count);

    relocateMultiboot((void *) blob, blob->multiboot,
                      blob->kernel_segment);
    relocateElf((void *) blob, blob->boot_driver_segment, 0,
                (struct Blob_relocation *) ((void *) blob +
                                            blob->boot_driver_relocations),
                blob->boot_driver_relocations_count);
    relocateElf((void *) blob, blob->kernel_segment, HIGH_MEMORY,
                (struct Blob_relocation *) ((void *) blob +
                                            blob->kernel_relocations),
                blob->kernel_relocations_count);

    uint32_t *pc;
    pc = (void *) blob + blob->boot_driver_segment +
        blob->boot_driver_entry;

    void *multiboot, *stack;
    multiboot = (void *) blob + blob->multiboot;
    stack = blob;

    // uint64_t sctlr, tcr;
    // uint64_t el;
    // __asm(  "mrs %0, currentel\n"
    //         "mrs %1, sctlr_el2\n"
    //         "mrs %2, tcr_el2\n": "=r" (el), "=r" (sctlr), "=r" (tcr));
    // el = el / 4;
    // Print(L"EL:%ld  SCTLR:%016lx  TCR:%016lx\n", el, sctlr, tcr);
    Print(L"args(%lx, %lx, %lx)\n", MULTIBOOT2_BOOTLOADER_MAGIC, multiboot,
          blob + 4096 - 16);
    Print(L"%x %x %x %x\n", pc[0], pc[1], pc[2], pc[3]);

    status = ST->BootServices->ExitBootServices(ImageHandle, mmap_key);
    if (EFI_ERROR(status)) {
        Print(L"ExitBootServices: ERROR %d, %x\n", status, mmap_key);
        return status;
    }

    status =
        ST->RuntimeServices->SetVirtualAddressMap(mmap_size, mmap_d_size,
                                                  mmap_d_ver,
                                                  (void *) &mmap);
    if (EFI_ERROR(status)) {
        Print(L"SetVirtualAddressMap: ERROR %d\n", status);
        return status;
    }
    // remove_protection();

    // __asm(  "tlbi alle2\n"
    //         "ic iallu\n"
    //         "dsb sy\n"
    //         "isb\n");

// Jump to the bootloader, the blob can be reused
    (*((boot_driver *) (pc))) (MULTIBOOT2_BOOTLOADER_MAGIC, multiboot,
                               blob + 4096 - 16);

    return EFI_SUCCESS;
}
