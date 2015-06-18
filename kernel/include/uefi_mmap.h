/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef UEFI_MMAP_H
#define UEFI_MMAP_H

//*******************************************************
//EFI_VIRTUAL_ADDRESS
// UEFI Spec v2.5, p.158
//*******************************************************
typedef uint64_t EFI_VIRTUAL_ADDRESS;

//*******************************************************
//EFI_PHYSICAL_ADDRESS
// UEFI Spec v2.5, p.153
//*******************************************************
typedef uint64_t EFI_PHYSICAL_ADDRESS;


//*******************************************************
//EFI_MEMORY_TYPE
// UEFI Spec v2.5, p.153
//*******************************************************
// These type values are discussed in Table 25 and Table 26
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
} EFI_MEMORY_TYPE;

//*******************************************************
// Memory Attribute Definitions
// from UEFI Spec v2.5, p.157
//*******************************************************
// These types can be “ORed” together as needed.
typedef enum {
    EFI_MEMORY_UC            = 0x0000000000000001,
    EFI_MEMORY_WC            = 0x0000000000000002,
    EFI_MEMORY_WT            = 0x0000000000000004,
    EFI_MEMORY_WB            = 0x0000000000000008,
    EFI_MEMORY_UCE           = 0x0000000000000010,
    EFI_MEMORY_WP            = 0x0000000000001000,
    EFI_MEMORY_RP            = 0x0000000000002000,
    EFI_MEMORY_XP            = 0x0000000000004000,
    EFI_MEMORY_NV            = 0x0000000000008000,
    EFI_MEMORY_MORE_RELIABLE = 0x0000000000010000,
    EFI_MEMORY_RO            = 0x0000000000020000,
    EFI_MEMORY_RUNTIME       = 0x8000000000000000,
} EFI_MEMORY_ATTR;

//*******************************************************
// Memory Descriptor Version Number
//*******************************************************
#define EFI_MEMORY_DESCRIPTOR_VERSION  1

//*******************************************************
//EFI_MEMORY_DESCRIPTOR
//*******************************************************
typedef struct {
    uint32_t              Type;
    EFI_PHYSICAL_ADDRESS  PhysicalStart;
    EFI_VIRTUAL_ADDRESS   VirtualStart;
    uint64_t              NumberOfPages;
    uint64_t              Attribute;
} EFI_MEMORY_DESCRIPTOR;

#endif // UEFI_MMAP_H
