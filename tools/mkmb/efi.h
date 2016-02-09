#ifndef __EFI_H
#define __EFI_H

/* We allocate three new EFI memory types to signal to the CPU driver where
 * its code, data, stack and Multiboot info block are located, so that it can
 * avoid trampling on them. */
typedef enum {
    EfiBarrelfishFirstMemType=   0x80000000,

    EfiBarrelfishCPUDriver=      0x80000000,
    EfiBarrelfishCPUDriverStack= 0x80000001,
    EfiBarrelfishMultibootData=  0x80000002,
    EfiBarrelfishELFData=        0x80000003,
    EfiBarrelfishBootPageTable=  0x80000004,

    EfiBarrelfishMaxMemType
} efi_barrelfish_memory_type;

/* Copyright (c) 2006 - 2015, Intel Corporation. All rights reserved.
   This program and the accompanying materials are licensed and made available
   under the terms and conditions of the BSD License that accompanies this
   distribution.  The full text of the license may be found at
   http://opensource.org/licenses/bsd-license.php.

   THE PROGRAM IS DISTRIBUTED UNDER THE BSD LICENSE ON AN "AS IS" BASIS,
   WITHOUT WARRANTIES OR REPRESENTATIONS OF ANY KIND, EITHER EXPRESS OR
   IMPLIED.
*/

/* Core EFI memory types - taken from the UDK. */
typedef enum {
    EfiReservedMemoryType,
    EfiLoaderCode,
    EfiLoaderData,
    EfiBootServicesCode,
    EfiBootServicesData,
    EfiRuntimeServicesCode,
    EfiRuntimeServicesData,
    EfiConventionalMemory,
    EfiUnusableMemory,
    EfiACPIReclaimMemory,
    EfiACPIMemoryNVS,
    EfiMemoryMappedIO,
    EfiMemoryMappedIOPortSpace,
    EfiPalCode,
    EfiPersistentMemory,
    EfiMaxMemoryType
} efi_memory_type;

/* An EFI memory region descriptor. */
typedef struct {
  uint32_t Type;
  uint64_t PhysicalStart;
  uint64_t VirtualStart;
  uint64_t NumberOfPages;
  uint64_t Attribute;
} efi_memory_descriptor;

void print_mmap(efi_memory_descriptor *mmap, size_t mmap_len);

#endif /* __EFI_H */
