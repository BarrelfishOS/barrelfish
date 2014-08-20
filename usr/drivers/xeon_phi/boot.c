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
#include "sleep.h"

#define BOOT_TIMEOUT 3000
#define BOOT_COUNTER 0xFFFFF

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
    return ((lvaddr_t)xeon_phi_boot_download_offset_rdf(&boot_registers))<<12;
}


static uint64_t get_adapter_memsize(void)
{
    xeon_phi_boot_meminfo_t meminfo = xeon_phi_boot_meminfo_rd(&boot_registers);

    uint64_t memsize = xeon_phi_boot_meminfo_size_kb_extract(meminfo);
    memsize *= 1024;

    switch (xeon_phi_boot_meminfo_usage_extract(meminfo)) {
        case xeon_phi_boot_mem_all:
            return memsize;
        case xeon_phi_boot_mem_half:
            return (memsize / 2);
        case xeon_phi_boot_mem_third:
            return (memsize / 3);
            break;
        case xeon_phi_boot_mem_fourth:
            return (memsize / 4);
        default:
            return memsize;
    }
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
                                lvaddr_t *ret_loadoffset)
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

    imgsize = module->mrmod_size;

    /*
     * get the load offset: we do not want to write into the boot loade
     */
    lvaddr_t loadoffset = get_load_offset(phi);

    get_adapter_memsize();

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    XBOOT_DEBUG("bootloader offset = 0x%lx\n", phi->apt.vbase + loadoffset);

    printf("Loading xloader onto card...\n");
    XBOOT_DEBUG("aper_base=0x%lx, offset = 0x%lx, size=0x%lx\n",
                phi->apt.vbase,
                loadoffset,
                imgsize);

    memcpy((void *) (phi->apt.vbase + loadoffset), (void *) binary, imgsize);

    if (ret_loadoffset) {
        *ret_loadoffset = loadoffset;
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
                                     uint32_t os_offset)
{
    errval_t err;

    XBOOT_DEBUG("multiboot offset = 0x%lx\n", phi->apt.vbase + load_offset);

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

    imgsize = module->mrmod_size;

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

    XBOOT_DEBUG("cmdline offset = 0x%lx\n", phi->apt.vbase + load_offset);

    void *buf = (void *) (phi->apt.vbase + load_offset);

    if (phi->cmdline) {
        cmdlen += sprintf(buf+cmdlen, "%s foobar=%i", phi->cmdline, 123);
    } else {
        cmdlen += sprintf(buf+cmdlen, "foobar=%i", 123);
    }

    cmdlen += sprintf(buf+cmdlen, "card_id=%i", phi->id);

    /*
     * id
     */
    /*
     * TODO: Add multihop / communication information here..
     */

    if (ret_size) {
        *ret_size = cmdlen;
    }

    return SYS_ERR_OK;
}

static errval_t bootstrap_notify(struct xeon_phi *phi,
                                 uint32_t os_imgsize)
{
    // set the bootimage size to tell the bootloader
    xeon_phi_boot_os_size_rawwr(&boot_registers, os_imgsize);

    uint64_t memsize = get_adapter_memsize();

    uint64_t reserved = (memsize * MEMORY_RESERVE_PERCENT / 100);


    // Keep in mind maximum uos reserve size is uint32_t, so we never overflow
    reserved = MIN(reserved, UOS_RESERVE_SIZE_MAX);
    reserved = MAX(reserved, UOS_RESERVE_SIZE_MIN);

    // Always align uos reserve size to a page
    reserved = (reserved & ~(BASE_PAGE_SIZE-1));

    XBOOT_DEBUG("memsize = 0x%lx, reserved size = 0x%lx\n", memsize, reserved);

    xeon_phi_boot_res_size_rawwr(&boot_registers, (uint32_t)reserved);

    // sending the bootstrap interrupt
    xeon_phi_apic_icr_lo_t icr_lo = xeon_phi_apic_icr_lo_default;
    icr_lo = xeon_phi_apic_icr_lo_vector_insert(icr_lo, xeon_phi_apic_vec_bsp);
    icr_lo = xeon_phi_apic_icr_lo_boot_notify_insert(icr_lo, 0x1);

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
    lvaddr_t offset, os_offset;
    uint32_t size, osimg_size;

    if (bi == NULL) {
        return SYS_ERR_ILLEGAL_INVOCATION;
    }

    xeon_phi_boot_initialize(&boot_registers,
                             XEON_PHI_MMIO_TO_SBOX(phi),
                             XEON_PHI_MMIO_TO_DBOX(phi));
    xeon_phi_apic_initialize(&apic_registers, XEON_PHI_MMIO_TO_SBOX(phi));


    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);
    XBOOT_DEBUG("APICID = %u\n", phi->apicid);

    // load the coprocessor OS
    err = load_bootloader(phi, xloader_img, &osimg_size, &offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load bootloader image");
    }

    os_offset = offset;

    // round to next page
    offset = ALIGN(osimg_size + offset);

    // load cmdline
    err = load_cmdline(phi, offset, &size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // round to next page
    offset = ALIGN(offset);

    // load multiboot image
    err = load_multiboot_image(phi, multiboot_img, offset, os_offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    xeon_phi_boot_download_status_wrf(&boot_registers, 0x0);

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    xeon_phi_serial_init(phi);

    // notify the bootstrap
    bootstrap_notify(phi, osimg_size);

    xeon_phi_boot_postcode_t postcode;
    xeon_phi_boot_postcodes_t pc, pc_prev = 0;
    uint32_t counter = BOOT_COUNTER;
    while(--counter) {
        postcode = xeon_phi_boot_postcode_rd(&boot_registers);
        pc = xeon_phi_boot_postcode_code_extract(postcode);
        if (pc_prev != pc) {
            debug_printf("Xeon Phi Booting: %s\n", xeon_phi_boot_postcodes_describe(pc));
        }
        if (postcode == xeon_phi_boot_postcode_done) {
            break;
        }
        pc_prev = pc;
    }

    XBOOT_DEBUG("Bootstrap has finished execution. Waiting for Firmware...\n");

    uint32_t time = 0, time_steps = 0;
    while(time < BOOT_TIMEOUT) {
        /* read all the pending messages */
        xeon_phi_serial_handle_recv();
        milli_sleep(100);
        if (xeon_phi_boot_download_status_rdf(&boot_registers)) {
            break;
        }
        if (time % 50) {
            debug_printf("Xeon Phi Booting: Waiting for ready signal %u\n", time_steps);
            time_steps += 5;
        }
    }

    if (!xeon_phi_boot_download_status_rdf(&boot_registers)) {
        USER_PANIC("Firmware not responding with ready bit");
        // TODO return error code
    }

    return SYS_ERR_OK;
}
