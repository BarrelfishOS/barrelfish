/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_XEON_PHI_H_
#define XEON_PHI_XEON_PHI_H_

/// The maximum number of coprocessor cards in a system
#define XEON_PHI_NUM_MAX 8

#define XEON_PHI_CBOX_BASE           0x0ULL              /* P54C Core */
#define XEON_PHI_TXS0_BASE           0x0800780000ULL     /* Texture Sampler */
#define XEON_PHI_TXS1_BASE           0x0800770000ULL     /* Texture Sampler */
#define XEON_PHI_TXS2_BASE           0x0800760000ULL     /* Texture Sampler */
#define XEON_PHI_TXS3_BASE           0x0800750000ULL     /* Texture Sampler */
#define XEON_PHI_TXS4_BASE           0x0800740000ULL     /* Texture Sampler */
#define XEON_PHI_TXS5_BASE           0x0800730000ULL     /* Texture Sampler */
#define XEON_PHI_TXS6_BASE           0x0800720000ULL     /* Texture Sampler */
#define XEON_PHI_TXS7_BASE           0x0800710000ULL     /* Texture Sampler */
#define XEON_PHI_DBOX0_BASE          0x08007C0000ULL     /* Display Box Registers */
#define XEON_PHI_DBOX1_BASE          0x0800620000ULL     /* Display Box Registers */
#define XEON_PHI_TD0_BASE            0x08007C0000ULL     /* Dbox Tag Directory TD */
#define XEON_PHI_TD1_BASE            0x0800620000ULL     /* Dbox Tag Directory TD */
#define XEON_PHI_VBOX_BASE           0x08007B0000ULL     /* Video Box Registers */
#define XEON_PHI_SBOX_BASE           0x08007D0000ULL     /* PCIE Box Registers */
#define XEON_PHI_GBOX0_BASE          0x08007A0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX1_BASE          0x0800790000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX2_BASE          0x0800700000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX3_BASE          0x08006F0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX4_BASE          0x08006D0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX5_BASE          0x08006C0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX6_BASE          0x08006B0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX7_BASE          0x08006A0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_REUT0_BASE          0x08007A0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT1_BASE          0x0800790000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT2_BASE          0x0800700000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT3_BASE          0x08006F0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT4_BASE          0x08006D0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT5_BASE          0x08006C0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT6_BASE          0x08006B0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT7_BASE          0x08006A0000ULL     /* Gbox REUT interface Registers */

#define XEON_PHI_GBOX_CHANNEL0_BASE  0x0
#define XEON_PHI_GBOX_CHANNEL1_BASE  0x800
#define XEON_PHI_GBOX_CHANNEL2_BASE  0x800
#define XEON_PHI_GBOX_CHANNEL3_BASE  0x1000

#define XEON_PHI_SBOX_SIZE (64*1024)
#define XEON_PHI_SBOX_SIZE_BITS (16)

// TODO: Verify these...
#define XEON_PHI_GBOX_SIZE (64*1024)
#define XEON_PHI_DBOX_SIZE (64*1024)
#define XEON_PHI_CBOX_SIZE (64*1024)

#define XEON_PHI_SYSMEM_BASE 0x8000000000ULL
#define XEON_PHI_SYSMEM_SIZE_BITS 39
#define XEON_PHI_SYSMEM_SIZE (1ULL << XEON_PHI_SYSMEM_SIZE_BITS)
#define XEON_PHI_SYSMEM_PAGE_SIZE (16UL*1024*1024*1024)
#define XEON_PHI_SYSMEM_PAGE_BITS 34

#define XEON_PHI_SYSMEM_KNC_BASE (XEON_PHI_SYSMEM_BASE + 25 * XEON_PHI_SYSMEM_PAGE_SIZE)

#define XEON_PHI_MEM_MASK 0xFFFFFFFFFFULL

struct xeon_phi_boot_params
{
    uint8_t reserved[0x54];
    uint64_t tboot_addr; /* 0x058 */
    uint8_t _pad3[128]; /* 0x070 */
    uint8_t dummy[256];
    uint8_t _pad1[4];
    uint32_t scratch; /* Scratch field! *//* 0x1e4 */
    uint8_t _pad6[13]; /* 0x1eb */
    uint8_t setup_sects;    /// must be at this very location !!!
    uint16_t root_flags;
    uint32_t syssize;       /// must be at this very location !!!
    uint16_t _pad2[5];
    uint32_t header;        /// must be at this very location !!!
    uint16_t version;
    uint32_t realmode_swtch;
    uint16_t start_sys;
    uint16_t kernel_version;
    uint8_t type_of_loader;
    uint8_t loadflags;
    uint16_t setup_move_size;
    uint32_t code32_start;
    uint32_t ramdisk_image;  /// pointer to the multiboot image
    uint32_t ramdisk_size;  /// multiboot image size
    uint32_t bootsect_kludge;
    uint16_t loader_name;
    uint8_t ext_loader_ver;
    uint8_t ext_loader_type;
    uint32_t payload_offset;
    uint32_t initrd_addr_max;
    uint32_t kernel_alignment;
    uint8_t _pad4[4];
    uint32_t cmdline_size;  /// size of the command line
    uint32_t hardware_subarch;
    uint64_t hardware_subarch_data;
    uint32_t cmdline_ptr;   /// pointer to the command line
    uint32_t xeon_phi_id;
    uint64_t msg_base;      /// pointer to the host message base address
    uint8_t msg_size_bits;      /// size of the messaging channel
    uint64_t multiboot;     /// pointer to the multiboot information
}__attribute__((packed));


typedef uint8_t xphi_chan_type_t;

typedef uint8_t xphi_id_t;

typedef uint64_t xphi_dom_id_t;

static inline xphi_dom_id_t xeon_phi_domain_build_id(xphi_id_t xid,
                                                     uint8_t core,
                                                     uint8_t is_host,
                                                     domainid_t domid)
{
    xphi_dom_id_t did = xid;
    did <<= 8;
    did |= core;
    did <<= 32;
    did |=domid;
    if (is_host) {
        did |= (1UL << 63);
    }
    return did;
}

static inline xphi_id_t xeon_phi_domain_get_xid(xphi_dom_id_t domid)
{
    return (0xFF & (domid >> 40));
}

static inline xphi_id_t xeon_phi_domain_get_core(xphi_dom_id_t domid)
{
    return (0xFF & (domid >> 32));
}

static inline uint8_t xeon_phi_domain_is_on_host(xphi_dom_id_t domid)
{
    return !!((1UL << 63) & domid);
}


#endif // XEON_PHI_XEON_PHI_H_
