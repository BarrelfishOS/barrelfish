/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <elf/elf.h>

#include <dev/xeon_phi/xeon_phi_boot_dev.h>
#include <dev/xeon_phi/xeon_phi_apic_dev.h>

struct bootinfo *bi = NULL;

#include "xeon_phi.h"

/*
 * TODO: Verify these values if they are really needed
 */
#define MEMORY_RESERVE_PERCENT 50
#define UOS_RESERVE_SIZE_MIN    ((128) * 1024 * 1024)
#define UOS_RESERVE_SIZE_MAX    (((4) * 1024 * 1024 * 1024ULL) - ((4) * 1024))

/*
 * Helper macros
 */
#define MAX(a, b)   ( ((a) > (b)) ? (a) : (b) )
#define MIN(a, b)   ( ((a) < (b)) ? (a) : (b) )
#define ALIGN(x) ((x + BASE_PAGE_SIZE-1) & ~(BASE_PAGE_SIZE-1))


static xeon_phi_boot_t boot_registers;
static xeon_phi_apic_t apic_registers;

/**
 * \brief   get the load offset to where to place the bootloader
 *
 * The bootstrap on the card will write the offset into the SBOX_SCRATCH2
 * register once the bootstrap is finished
 */
static inline lvaddr_t get_load_offset(struct xeon_phi *phi)
{

    xeon_phi_boot_download_t offset;
    offset = xeon_phi_boot_download_rawrd(&boot_registers);
    return (offset & xeon_phi_boot_address_mask);
}

/**
 * \brief Loads the bootloader image onto the card
 *
 * \param phi           the xeon phi card information
 * \param xloader_img   name of the bootloader image
 * \param ret_imgsize   returned image size
 * \param ret_cmdoffset returned offset to load the next piece onto the card
 *
 * Note: it is important that the bootloader just uses statically allocated
 *       memory and does not exceed its image size with additional memory.
 *       Otherwise the CMD line or the multiboot image will be overwritten.
 */
static errval_t load_bootloader(struct xeon_phi *phi,
                                char *xloader_img,
                                uint32_t *ret_imgsize,
                                lvaddr_t *ret_cmdoffset)
{
    errval_t err;
    /*
     * find the boot loader image in the host multiboot
     */
    struct mem_region *module = multiboot_find_module(bi, xloader_img);
    if (module == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    lvaddr_t binary = 0;
    size_t imgsize = 0;

    err = spawn_map_module(module, &imgsize, &binary, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_ELF_MAP);
    }

    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *) binary;
    if (head->e_machine != EM_K1OM) {
        return SPAWN_ERR_DETERMINE_CPUTYPE;
    }

    /*
     * get the load offset: we do not want to write into the
     */
    lvaddr_t loadoffset = get_load_offset(phi);

    printf("Loading xloader onto card...\n");
    XBOOT_DEBUG("aper_base=0x%lx, offset = 0x%lx, size=0x%lx\n",
                phi->apt.vbase,
                loadoffset,
                imgsize);

    memcpy((void *) (phi->apt.vbase + loadoffset), (void *) binary, imgsize);

    if (ret_cmdoffset) {
        *ret_cmdoffset = loadoffset + imgsize;
    }

    if (ret_imgsize) {
        *ret_imgsize = imgsize;
    }

    return SYS_ERR_OK;
}

/**
 *
 */
static errval_t load_multiboot_image(struct xeon_phi *phi,
                                     char *multiboot_img,
                                     lvaddr_t load_offset,
                                     uint32_t *ret_imgsize)
{
    errval_t err;
    /*
     * find the boot loader image in the host multiboot
     */
    struct mem_region *module = multiboot_find_module(bi, multiboot_img);
    if (module == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    lvaddr_t image = 0;
    size_t imgsize = 0;

    err = spawn_map_module(module, &imgsize, &image, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_ELF_MAP);
    }

    lvaddr_t os_offset = get_load_offset(phi);

    printf("Loading multiboot image onto card...\n");
    XBOOT_DEBUG("aper_base=0x%lx, offset = 0x%lx, size=0x%lx\n",
                phi->apt.vbase,
                load_offset,
                imgsize);

    memcpy((void *) (phi->apt.vbase + load_offset), (void *) image, imgsize);

    /*
     * we are using the Linux style way in booting. The following will update
     * the corresponding fields in struct boot_param of the header.
     */
    uint32_t *ramfs_addr_ptr = (uint32_t *) (phi->apt.vbase + os_offset + 0x218);
    *ramfs_addr_ptr = load_offset;
    ramfs_addr_ptr = (uint32_t *) (phi->apt.vbase + os_offset + 0x21c);
    *ramfs_addr_ptr = imgsize;

    return SYS_ERR_OK;
}

/**
 * \brief   generates the cmdline supplied to the card kernel
 *
 * \param   phi         the card information structure
 * \param   load_offset offset where to load the cmdline
 * \param   ret_size    size of the cmdline in bytes
 */
static errval_t load_cmdline(struct xeon_phi *phi,
                             lvaddr_t load_offset,
                             uint32_t *ret_size)
{
    uint32_t cmdlen = 0;
    void *buf = (void *) (phi->apt.vbase + load_offset);

    if (phi->cmdline) {
        cmdlen += sprintf(buf, "%s foobar=%i", phi->cmdline, 123);
    } else {
        cmdlen += sprintf(buf, "foobar=%i", 123);
    }

    /*
     * TODO: Add multihop / communication information here..
     */

    if (ret_size) {
        *ret_size = load_offset;
    }

    return SYS_ERR_OK;
}

static errval_t bootstrap_notify(struct xeon_phi *phi,
                                 uint32_t os_imgsize)
{
    // set the bootimage size to tell the bootloader
    xeon_phi_boot_os_size_rawwr(&boot_registers, os_imgsize);

    xeon_phi_boot_meminfo_t meminfo = xeon_phi_boot_meminfo_rd(&boot_registers);

    uint32_t memsize = xeon_phi_boot_meminfo_size_kb_extract(meminfo) * 1024;

    switch (xeon_phi_boot_meminfo_usage_extract(meminfo)) {
        case xeon_phi_boot_mem_all:
            memsize /= 1;
            break;
        case xeon_phi_boot_mem_half:
            memsize /= 2;
            break;

        case xeon_phi_boot_mem_third:
            memsize /= 3;
            break;
        case xeon_phi_boot_mem_fourth:
            memsize /= 4;
            break;
    }

    uint32_t reserved = (uint32_t)(memsize * MEMORY_RESERVE_PERCENT / 100);


    // Keep in mind maximum uos reserve size is uint32_t, so we never overflow
    reserved = MIN(reserved, UOS_RESERVE_SIZE_MAX);
    reserved = MAX(reserved, UOS_RESERVE_SIZE_MIN);

    // Always align uos reserve size to a page
    reserved = (uint32_t)(reserved & ~(BASE_PAGE_SIZE-1));

    XBOOT_DEBUG("memsize = 0x%x, reserved size = 0x%x\n", memsize, reserved);

    xeon_phi_boot_res_size_wr(&boot_registers, reserved);

    // sending the bootstrap interrupt
    xeon_phi_apic_icr_lo_t icr_lo = xeon_phi_apic_icr_lo_default;
    icr_lo = xeon_phi_apic_icr_lo_vector_insert(icr_lo, xeon_phi_apic_vec_bsp);
    icr_lo = xeon_phi_apic_icr_lo_dlv_stat_insert(icr_lo, 0x1);

    assert(icr_lo == (229 | (1 << 13)));

    xeon_phi_apic_icr_hi_wr(&apic_registers, xeon_phi_apic_bootstrap, phi->apicid);



    xeon_phi_apic_icr_lo_wr(&apic_registers, xeon_phi_apic_bootstrap, icr_lo);

    return SYS_ERR_OK;
}

/**
 * \brief boots the card with the given loader and multiboot image
 *
 * \param phi           pointer to the card information
 * \param xloader_img   pointer to the card bootloader image
 * \param multiboot_img pointer to the card multiboot image
 */
errval_t xeon_phi_boot(struct xeon_phi *phi,
                       char *xloader_img,
                       char *multiboot_img)
{
    errval_t err;
    lvaddr_t offset;
    uint32_t size, osimg_size;

    if (bi == NULL) {
        return SYS_ERR_ILLEGAL_INVOCATION;
    }

    xeon_phi_boot_initialize(&boot_registers,
                             XEON_PHI_MMIO_TO_SBOX(phi),
                             XEON_PHI_MMIO_TO_DBOX(phi));
    xeon_phi_apic_initialize(&apic_registers, XEON_PHI_MMIO_TO_SBOX(phi));

    // load the coprocessor OS
    err = load_bootloader(phi, xloader_img, &osimg_size, &offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load bootloader image");
    }

    // round to next page
    offset = ALIGN(offset);

    // load cmdline
    err = load_cmdline(phi, offset, &size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // round to next page
    offset = ALIGN(offset);

    // load multiboot image
    err = load_multiboot_image(phi, multiboot_img, offset, &size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // start the receive thread
    serial_start_recv_thread(phi);

    // notify the bootstrap
    bootstrap_notify(phi, osimg_size);

    return SYS_ERR_OK;
}
