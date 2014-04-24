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

#include "xeon_phi.h"


static xeon_phi_boot_t boot_registers;

/**
 * \brief   get the load offset to where to place the bootloader
 *
 * The bootstrap on the card will write the offset into the SBOX_SCRATCH2
 * register once the bootstrap is finished
 */
static inline lvaddr_t
get_load_offset(struct xeon_phi *phi)
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
 */
static errval_t
load_os(struct xeon_phi *phi,
        char *xloader_img,
        uint32_t *ret_imgsize,
        lvaddr_t *ret_cmdoffset)
{
    errval_t err;
    /*
     * find the boot loader image in the host multiboot
     */
    struct mem_region *module = multiboot_find_module(NULL, xloader_img);
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
    XBOOT_DEBUG("aper_base=0x%lx, offset = 0x%lx, size=0x%lx\n", phi->aper_base,
                loadoffset, imgsize);

    memcpy((void *) (phi->aper_base + loadoffset), (void *)binary, imgsize);

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
static errval_t
load_multiboot_image(struct xeon_phi *phi,
                     char *multiboot_img,
                     lvaddr_t load_offset,
                     uint32_t *ret_imgsize)
{
    errval_t err;
    /*
     * find the boot loader image in the host multiboot
     */
    struct mem_region *module = multiboot_find_module(NULL, multiboot_img);
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
    XBOOT_DEBUG("aper_base=0x%lx, offset = 0x%lx, size=0x%lx\n", phi->aper_base,
                load_offset, imgsize);

    memcpy((void *) (phi->aper_base + load_offset), (void *)image, imgsize);

    /*
     * we are using the Linux style way in booting. The following will update
     * the corresponding fields in struct boot_param of the header.
     */
    uint32_t *ramfs_addr_ptr = (uint32_t *) (phi->aper_base + os_offset + 0x218);
    *ramfs_addr_ptr = load_offset;
    ramfs_addr_ptr = (uint32_t *) (phi->aper_base + os_offset + 0x21c);
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
static errval_t
load_cmdline(struct xeon_phi *phi,
             lvaddr_t load_offset,
             uint32_t *ret_size)
{
    uint32_t cmdlen = 0;
    void *buf = (void *) (phi->aper_base + load_offset);

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

static errval_t
bootstrap_notify(struct xeon_phi *phi,
                 uint32_t os_imgsize)
{
#if 0
    uint32_t scratch5;

    scratch5 = os_imgsize;
    // XPU_RACE_CONDITION: write to MMIO space is uncached and flushes WC buffers
    SBOX_WRITE(scratch5, mmio_va, SBOX_SCRATCH5);

    get_adapter_memsize(mmio_va, &adapter_memsize);

    // Program the register to inform the uOS of how much space to reserve
    get_uos_reserved_size(mmio_va, adapter_memsize, &uos_reserved_size);
    set_uos_reserved_size(mmio_va, uos_reserved_size);

    mic_send_bootstrap_intr(mic_ctx);
#endif
    return SYS_ERR_OK;
}

/**
 * \brief boots the card with the given loader and multiboot image
 *
 * \param phi           pointer to the card information
 * \param xloader_img   pointer to the card bootloader image
 * \param multiboot_img pointer to the card multiboot image
 */
errval_t
xeon_phi_boot(struct xeon_phi *phi,
              char *xloader_img,
              char *multiboot_img)
{
    errval_t err;
    lvaddr_t offset;
    uint32_t size, osimg_size;

    xeon_phi_boot_initialize(&boot_registers, XEON_PHI_MMIO_TO_SBOX(phi->mmio_base));

    // load the coprocessor OS
    err = load_os(phi, xloader_img, &osimg_size, &offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // load cmdline
    err = load_cmdline(phi, offset, &size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // round to next page
    offset  += size + BASE_PAGE_SIZE - (size & (BASE_PAGE_SIZE - 1 ));

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

/**
 * \brief performs a soft reset of the card
 *
 * \param phi   pointer to the card information
 */
errval_t
xeon_phi_reset(struct xeon_phi *phi)
{
    return SYS_ERR_OK;
}
