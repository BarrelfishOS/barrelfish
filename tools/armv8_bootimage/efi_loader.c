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

#include <efi/efi.h>
#include <efi/efilib.h>
#include <multiboot2.h>
#include "blob.h"

void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);

extern uint64_t barrelfish_blob_start[1];
extern uint64_t barrelfish_blob_end[1];

#define HIGH_MEMORY 0xffff000000000000

#define ROUND_UP(x, y) (((x) + ((y) - 1)) & ~((y) - 1))

#define MEM_MAP_SIZE 8192
char mmap[MEM_MAP_SIZE];
UINTN mmap_size, mmap_key, mmap_d_size;
UINT32 mmap_d_ver;

void get_memory_map(void)
{
    EFI_STATUS status;
    unsigned mmap_n_desc, i;

    /* Grab the current table from UEFI. */
    mmap_size = MEM_MAP_SIZE;
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
        desc->VirtualStart = desc->PhysicalStart + HIGH_MEMORY;
        Print(L"%3d %016lx %016lx %016lx %016lx\n",
              desc->Type, desc->VirtualStart,
              desc->PhysicalStart,
              desc->PhysicalStart + (desc->NumberOfPages << 12),
              desc->Attribute);
    }
}

void relocateMultiboot(void *base, uint64_t offset,
                       uint64_t kernel_segment)
{
    struct multiboot_info *multiboot = (struct multiboot_info *)(base + offset);

    get_memory_map();

    struct multiboot_tag *tag;
    Print(L"Relocating multiboot: %lx\n", multiboot);
    for (tag = multiboot->tags; tag->type != MULTIBOOT_TAG_TYPE_END; tag = (void *)tag + tag->size) {
        Print(L"%lx: tag %d:%d\n", tag, tag->type, tag->size);
        if (tag->type == MULTIBOOT_TAG_TYPE_MODULE_64) {
            struct multiboot_tag_module_64 *mtag = (struct multiboot_tag_module_64 *) tag;
            Print(L"\tbefore %lx:%lx\n", mtag->mod_start, mtag->mod_end);
            mtag->mod_start += (uint64_t) base;
            mtag->mod_end += (uint64_t) base;
            Print(L"\tafter  %lx:%lx\n", mtag->mod_start, mtag->mod_end);
        } else if (tag->type == MULTIBOOT_TAG_TYPE_EFI_MMAP) {
            struct multiboot_tag_efi_mmap *emtag = (struct multiboot_tag_efi_mmap *)tag;
            emtag->descr_size = mmap_d_size;
            emtag->descr_vers = mmap_d_ver;
            emtag->size = sizeof(struct multiboot_tag_efi_mmap) + mmap_size;
            memcpy((void *) emtag + sizeof(struct multiboot_tag_efi_mmap),
                   (uint64_t *) mmap, mmap_size);

            /* Add the end tag now that we know how large the memory map is */
            struct multiboot_tag *end_tag = (void *)emtag + emtag->size;
            end_tag->type = MULTIBOOT_TAG_TYPE_END;
            end_tag->size = ROUND_UP(sizeof(struct multiboot_tag), MULTIBOOT_TAG_ALIGN);
            multiboot->total_size = (void *)end_tag + end_tag->size - (void *)multiboot;
            Print(L"Size: %x\n", multiboot->total_size);
        }
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

struct armv8

typedef void boot_driver(uint32_t magic, void *pointer);

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
    Print(L"Blob  size:%ld  pages:%ld -> %lx\n", blob_size,
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

    void *multiboot;
    multiboot = (void *) blob + blob->multiboot;

    Print(L"args(%lx, %lx)\n", MULTIBOOT2_BOOTLOADER_MAGIC, multiboot);

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

    // Jump to the bootloader, the blob can be reused
    (*((boot_driver *) (pc))) (MULTIBOOT2_BOOTLOADER_MAGIC, multiboot);

    return EFI_SUCCESS;
}
