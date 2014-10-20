/**
 * \file
 * \brief Struct definition for the boot param struct supplied by the
 *        K1OM boot loader
 */

/*
 * Copyright (c) 2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 *
 *
 */

#ifndef KERNEL_BOOT_PARAM_H
#define KERNEL_BOOT_PARAM_H

#include <kernel_boot_param/screen_info.h>
#include <kernel_boot_param/apm_bios.h>
#include <kernel_boot_param/ist_bios.h>
#include <kernel_boot_param/edd.h>
#include <kernel_boot_param/e820.h>

struct edid_info
{
    unsigned char dummy[128];
};

/* setup data types */
#define SETUP_NONE			0
#define SETUP_E820_EXT			1

/* extensible setup data list node */
struct setup_data
{
    uint64_t next;
    uint32_t type;
    uint32_t len;
    uint8_t data[0];
};

struct setup_header
{
    uint8_t setup_sects;
    uint16_t root_flags;
    uint32_t syssize;
    uint16_t ram_size;
#define RAMDISK_IMAGE_START_MASK	0x07FF
#define RAMDISK_PROMPT_FLAG		0x8000
#define RAMDISK_LOAD_FLAG		0x4000
    uint16_t vid_mode;
    uint16_t root_dev;
    uint16_t boot_flag;
    uint16_t jump;
    uint32_t header;
    uint16_t version;
    uint32_t realmode_swtch;
    uint16_t start_sys;
    uint16_t kernel_version;
    uint8_t type_of_loader;
    uint8_t loadflags;
#define LOADED_HIGH	(1<<0)
#define QUIET_FLAG	(1<<5)
#define KEEP_SEGMENTS	(1<<6)
#define CAN_USE_HEAP	(1<<7)
    uint16_t setup_move_size;
    uint32_t code32_start;
    uint32_t ramdisk_image;
    uint32_t ramdisk_size;
    uint32_t bootsect_kludge;
    uint16_t heap_end_ptr;
    uint8_t ext_loader_ver;
    uint8_t ext_loader_type;
    uint32_t cmd_line_ptr;
    uint32_t initrd_addr_max;
    uint32_t kernel_alignment;
    uint8_t relocatable_kernel;
    uint8_t _pad2[3];
    uint32_t cmdline_size;
    uint32_t hardware_subarch;
    uint64_t hardware_subarch_data;
    uint32_t payload_offset;
    uint32_t payload_length;
    uint64_t setup_data;
}__attribute__((packed));

struct sys_desc_table
{
    uint16_t length;
    uint8_t table[14];
};

/* Gleaned from OFW's set-parameters in cpu/x86/pc/linux.fth */
struct olpc_ofw_header
{
    uint32_t ofw_magic; /* OFW signature */
    uint32_t ofw_version;
    uint32_t cif_handler; /* callback into OFW */
    uint32_t irq_desc_table;
}__attribute__((packed));

struct efi_info
{
    uint32_t efi_loader_signature;
    uint32_t efi_systab;
    uint32_t efi_memdesc_size;
    uint32_t efi_memdesc_version;
    uint32_t efi_memmap;
    uint32_t efi_memmap_size;
    uint32_t efi_systab_hi;
    uint32_t efi_memmap_hi;
};

/* The so-called "zeropage" */
struct boot_params
{
    struct screen_info screen_info; /* 0x000 */
    struct apm_bios_info apm_bios_info; /* 0x040 */
    uint8_t _pad2[4]; /* 0x054 */
    uint64_t tboot_addr; /* 0x058 */
    struct ist_info ist_info; /* 0x060 */
    uint8_t _pad3[16]; /* 0x070 */
    uint8_t hd0_info[16]; /* obsolete! *//* 0x080 */
    uint8_t hd1_info[16]; /* obsolete! *//* 0x090 */
    struct sys_desc_table sys_desc_table; /* 0x0a0 */
    struct olpc_ofw_header olpc_ofw_header; /* 0x0b0 */
    uint8_t _pad4[128]; /* 0x0c0 */
    struct edid_info edid_info; /* 0x140 */
    struct efi_info efi_info; /* 0x1c0 */
    uint32_t alt_mem_k; /* 0x1e0 */
    uint32_t scratch; /* Scratch field! *//* 0x1e4 */
    uint8_t e820_entries; /* 0x1e8 */
    uint8_t eddbuf_entries; /* 0x1e9 */
    uint8_t edd_mbr_sig_buf_entries; /* 0x1ea */
    uint8_t _pad6[6]; /* 0x1eb */
    struct setup_header hdr; /* setup header *//* 0x1f1 */
    uint8_t _pad7[0x290 - 0x1f1 - sizeof(struct setup_header)];
    uint32_t edd_mbr_sig_buffer[EDD_MBR_SIG_MAX]; /* 0x290 */
    struct e820entry e820_map[E820MAX]; /* 0x2d0 */
    uint8_t _pad8[48]; /* 0xcd0 */
    struct edd_info eddbuf[EDDMAXNR]; /* 0xd00 */
    uint8_t _pad9[276]; /* 0xeec */
}__attribute__((packed));

enum
{
    X86_SUBARCH_PC = 0,
    X86_SUBARCH_LGUEST,
    X86_SUBARCH_XEN,
    X86_SUBARCH_MRST,
    X86_SUBARCH_CE4100,
    X86_NR_SUBARCHS,
};

#endif /* KERNEL_BOOT_PARAM_H */
