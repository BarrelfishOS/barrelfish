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
#include <vfs/vfs.h>
#include <elf/elf.h>

#include <xeon_phi/xeon_phi.h>

#include <dev/xeon_phi/xeon_phi_boot_dev.h>
#include <dev/xeon_phi/xeon_phi_apic_dev.h>

struct bootinfo *bi = NULL;

#include "xeon_phi_internal.h"
#include "interphi.h"
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
    return ((lvaddr_t) xeon_phi_boot_download_offset_rdf(&boot_registers)) << 12;
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


static errval_t load_module(char *path,
                            void *buf,
                            uint64_t *ret_size)
{
    errval_t err;


    /* read file into memory */
    vfs_handle_t fh;
    err = vfs_open(path, &fh);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    struct vfs_fileinfo info;
    err = vfs_stat(fh, &info);
    if (err_is_fail(err)) {
        vfs_close(fh);
        return err_push(err, SPAWN_ERR_LOAD);
    }

    assert(info.type == VFS_FILE);

    uint8_t *image = buf;

    size_t pos = 0, readlen;
    do {
        err = vfs_read(fh, &image[pos], info.size - pos, &readlen);
        if (err_is_fail(err)) {
            vfs_close(fh);
            return err_push(err, SPAWN_ERR_LOAD);
        } else if (readlen == 0) {
            vfs_close(fh);
            return SPAWN_ERR_LOAD; // XXX
        } else {
            pos += readlen;
        }
    } while (err_is_ok(err) && readlen > 0 && pos < info.size);

    err = vfs_close(fh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to close file %s", path);
    }

    if (ret_size) {
        *ret_size = info.size;
    }

    return SYS_ERR_OK;

}

/**
 * \brief Loads the bootloader image onto the card
 *
 * \param phi           the xeon phi card information
 * \param xloader_img   name of the bootloader image
 * \param ret_imgsize   returned image size
 *
 * Note: it is important that the bootloader just uses statically allocated
 *       memory and does not exceed its image size with additional memory.
 *       Otherwise the CMD line or the multiboot image will be overwritten.
 */
static errval_t load_bootloader(struct xeon_phi *phi,
                                char *xloader_img)
{
    errval_t err;
#if 0
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

    XBOOT_DEBUG("Loading xloader onto card... offset = 0x%lx\n", loadoffset);

    memcpy((void *) (phi->apt.vbase + loadoffset), (void *) binary, imgsize);

#endif
    lvaddr_t loadoffset = get_load_offset(phi);
    uint64_t imgsize;

    void *buf = (void *) (phi->apt.vbase + loadoffset);

    err = load_module(xloader_img, buf, &imgsize);
    if (err_is_fail(err)) {
        return err;
    }

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    phi->os_offset = loadoffset;
    phi->os_size = imgsize;

    return SYS_ERR_OK;
}

/**
 *
 */
static errval_t load_multiboot_image(struct xeon_phi *phi,
                                     char *multiboot_img,
                                     lvaddr_t load_offset)
{
    errval_t err;

    assert(phi->os_offset != 0);

#if 0
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

    XBOOT_DEBUG("loading multiboot image onto card... offset = 0x%lx\n",
                load_offset);

    memcpy((void *) (phi->apt.vbase + load_offset), (void *) image, imgsize);
#endif

    uint64_t imgsize;

    void *buf = (void *) (phi->apt.vbase + load_offset);

    err = load_module(multiboot_img, buf, &imgsize);
    if (err_is_fail(err)) {
        return err;
    }

    /*
     * we are using the Linux style way in booting. The following will update
     * the corresponding fields in struct boot_param of the header.
     */
    struct xeon_phi_boot_params *bp;
    bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);
    bp->ramdisk_image = (uint32_t)load_offset;
    bp->ramdisk_size = (uint32_t)imgsize;

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
                             lvaddr_t load_offset)
{
    uint32_t cmdlen = 0;

    struct xeon_phi_boot_params *bp;
    bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);

    XBOOT_DEBUG("copying cmdline onto card, offset = 0x%lx\n", load_offset);

    void *buf = (void *) (phi->apt.vbase + load_offset);

    if (phi->cmdline) {
        cmdlen += sprintf(buf + cmdlen, "%s", phi->cmdline);
    }

    cmdlen += sprintf(buf + cmdlen, "card_id=%i", phi->id);

    /*
     * id
     */
    /*
     * TODO: Add multihop / communication information here..
     */

    printf("cmdline = %x,  %s\n", (uint32_t)load_offset, (char*)buf);

    phi->cmdline = buf;
    phi->cmdlen = cmdlen;


    bp->cmdline_ptr = (uint32_t)(load_offset);
    bp->cmdline_size = (uint32_t)cmdlen;

    return SYS_ERR_OK;
}

static errval_t bootstrap_notify(struct xeon_phi *phi)
{
    // set the bootimage size to tell the bootloader
    xeon_phi_boot_os_size_rawwr(&boot_registers, phi->os_size);

    uint64_t memsize = get_adapter_memsize();

    uint64_t reserved = (memsize * MEMORY_RESERVE_PERCENT / 100);

    // Keep in mind maximum uos reserve size is uint32_t, so we never overflow
    reserved = MIN(reserved, UOS_RESERVE_SIZE_MAX);
    reserved = MAX(reserved, UOS_RESERVE_SIZE_MIN);

    // Always align uos reserve size to a page
    reserved = (reserved & ~(BASE_PAGE_SIZE - 1));

    xeon_phi_boot_res_size_rawwr(&boot_registers, (uint32_t) reserved);

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
    lvaddr_t offset;

    xeon_phi_boot_initialize(&boot_registers,
                             XEON_PHI_MMIO_TO_SBOX(phi),
                             XEON_PHI_MMIO_TO_DBOX(phi));
    xeon_phi_apic_initialize(&apic_registers, XEON_PHI_MMIO_TO_SBOX(phi));

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    // load the coprocessor OS
    err = load_bootloader(phi, xloader_img);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load bootloader image");
    }

    // round to next page
    offset = ALIGN(phi->os_offset + phi->os_size);

    err = interphi_init(phi, NULL_CAP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize messaging");
    }

    // load cmdline
    err = load_cmdline(phi, offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // round to next page
    offset = ALIGN(offset+phi->cmdlen);

    // load multiboot image
    err = load_multiboot_image(phi, multiboot_img, offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    struct xeon_phi_boot_params *bp;
    bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);
    bp->xeon_phi_id = 0xFF00;
    bp->xeon_phi_id |= phi->id;

    xeon_phi_boot_download_status_wrf(&boot_registers, 0x0);

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    xeon_phi_serial_init(phi);

    // notify the bootstrap
    bootstrap_notify(phi);

    xeon_phi_boot_postcode_t postcode;
    xeon_phi_boot_postcodes_t pc, pc_prev = 0;
    uint32_t counter = BOOT_COUNTER;
    while (--counter) {
        postcode = xeon_phi_boot_postcode_rd(&boot_registers);
        pc = xeon_phi_boot_postcode_code_extract(postcode);
        if (pc_prev != pc) {
            debug_printf("Xeon Phi Booting: %s\n",
                         xeon_phi_boot_postcodes_describe(pc));
        }
        if (postcode == xeon_phi_boot_postcode_done) {
            break;
        }
        pc_prev = pc;
    }

    XBOOT_DEBUG("Bootstrap has finished execution. Waiting for Firmware...\n");

    uint32_t time = 0, time_steps = 0;
    while (time < BOOT_TIMEOUT) {
        /* read all the pending messages */
        xeon_phi_serial_handle_recv();
        milli_sleep(100);
        if (xeon_phi_boot_download_status_rdf(&boot_registers)) {
            XBOOT_DEBUG("Firmware signaled with ready bit. \n");
            break;
        }
        if (!(time % 50)) {
            debug_printf("Xeon Phi Booting: Waiting for ready signal %u\n",
                         time_steps);
            time_steps += 5;
        }
        time++;
    }

    if (!xeon_phi_boot_download_status_rdf(&boot_registers)) {
        USER_PANIC("Firmware not responding with ready bit");
        // TODO return error code
    }

    // we don't need the aperture mapped anymore so unmap it
    err = xeon_phi_unmap_aperture(phi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to map aperture range");
    }

    return SYS_ERR_OK;
}
