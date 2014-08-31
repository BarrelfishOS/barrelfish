/**
 * \file
 * \brief Card Configuration
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
#include <pci/pci.h>

#include <dev/xeon_phi/xeon_phi_boot_dev.h>

#include "xeon_phi_internal.h"
#include "interrupts.h"
#include "sleep.h"
#include "smpt.h"
#include "sysmem_caps.h"

static uint32_t initialized = 0;

#define XEON_PHI_RESET_TIME 3000
#define XEON_PHI_RESET_TIME_UNIT 100

static struct xeon_phi *card;

/// PCI Vendor ID of Intel
#define PCI_VENDOR_ID_INTEL     0x8086

#define PCI_SUBSYSTEM_DEVICE    0x2500

#define PCI_SUBSYSTEM_VENDOR    0x8086

/*
 * These are the possible device IDs of the Xeon PHi according to the
 * Intel MPSS implementation.
 *
 * Querying the host with lspci -nn | grep "8086:225" gives the actual
 * device ID of the built in cards to be the last one.
 *
 * 07:00.0 Co-processor [0b40]: Intel Corporation Device [8086:225e] (rev 11)
 * 82:00.0 Co-processor [0b40]: Intel Corporation Device [8086:225e] (rev 11)
 */
#define PCI_DEVICE_KNC_2250 0x2250
#define PCI_DEVICE_KNC_2251 0x2251
#define PCI_DEVICE_KNC_2252 0x2252
#define PCI_DEVICE_KNC_2253 0x2253
#define PCI_DEVICE_KNC_2254 0x2254
#define PCI_DEVICE_KNC_2255 0x2255
#define PCI_DEVICE_KNC_2256 0x2256
#define PCI_DEVICE_KNC_2257 0x2257
#define PCI_DEVICE_KNC_2258 0x2258
#define PCI_DEVICE_KNC_2259 0x2259
#define PCI_DEVICE_KNC_225a 0x225a
#define PCI_DEVICE_KNC_225b 0x225b
#define PCI_DEVICE_KNC_225c 0x225c
#define PCI_DEVICE_KNC_225d 0x225d
#define PCI_DEVICE_KNC_225e 0x225e

#define XEON_PHI_APT_BAR 0
#define XEON_PHI_MMIO_BAR 1
#define XEON_PHI_MMIO_BAR_SIZE (128*1024)
#define XEON_PHI_APT_BAR_SIZE (8ULL << 30)
#define XEON_PHI_BAR_TYPE 0

static void device_init(struct xeon_phi *phi)
{

#if 0
    scratch13 = SBOX_READ(mic_ctx->mmio.va, SBOX_SCRATCH13);
    mic_ctx->bi_stepping = SCRATCH13_STEP_ID(scratch13);
    mic_ctx->bi_substepping = SCRATCH13_SUB_STEP(scratch13);
#ifdef MIC_IS_EMULATION
    mic_ctx->bi_platform = PLATFORM_EMULATOR;
#else
    mic_ctx->bi_platform = SCRATCH13_PLATFORM_ID(scratch13);
#endif
    mic_enable_msi_interrupts(mic_ctx);
    mic_enable_interrupts(mic_ctx);

    mic_reg_irqhandler(mic_ctx, 1, "MIC SHUTDOWN DoorBell 1",
                    mic_shutdown_host_doorbell_intr_handler);

#endif

    initialized = true;
}



static void pci_init_card(struct device_mem* bar_info,
                          int bar_count)
{
    errval_t err;

    if (initialized) {
        debug_printf("WARNING > Device already initialized\n");
        return;
    }

    printf("Got: %i bars\n", bar_count);
    for (int i = 0; i < bar_count; ++i) {
        printf("> Bar[%i]: {type=%i, paddr=0x%lx, size=%u}\n", i, bar_info[i].type, bar_info[i].paddr, (uint32_t)(bar_info[i].bytes/1024));
    }

    if (bar_count != 2) {
        USER_PANIC("There is something wrong. The Card should have 2 MBARs.");
    }


    /*
     * TODO> install some checks that we got the correct caps
     */
    assert(bar_info[XEON_PHI_MMIO_BAR].bytes == XEON_PHI_MMIO_BAR_SIZE);
    assert(bar_info[XEON_PHI_APT_BAR].bytes >= XEON_PHI_APT_BAR_SIZE);
    assert(bar_info[XEON_PHI_MMIO_BAR].type == XEON_PHI_BAR_TYPE);
    assert(bar_info[XEON_PHI_APT_BAR].type == XEON_PHI_BAR_TYPE);

    struct frame_identity id;
    assert(bar_info[XEON_PHI_APT_BAR].nr_caps == 1);
    assert(!capref_is_null(bar_info[XEON_PHI_APT_BAR].frame_cap[0]));
    card->apt.cap = bar_info[XEON_PHI_APT_BAR].frame_cap[0];
    err = invoke_frame_identify(card->apt.cap, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to identify the aperture cap");
    }
    card->apt.bits = id.bits;
    card->apt.pbase = id.base;
    card->apt.bytes = bar_info[XEON_PHI_APT_BAR].bytes;

    assert(bar_info[XEON_PHI_MMIO_BAR].nr_caps == 1);
    assert(!capref_is_null(bar_info[XEON_PHI_MMIO_BAR].frame_cap[0]));
    card->mmio.cap = bar_info[XEON_PHI_MMIO_BAR].frame_cap[0];

    err = invoke_frame_identify(card->mmio.cap, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to identify the aperture cap");
    }
    card->mmio.bits = id.bits;
    card->mmio.pbase = id.base;
    card->mmio.bytes = bar_info[XEON_PHI_MMIO_BAR].bytes;

    err = xeon_phi_map_aperture(card, XEON_PHI_APERTURE_INIT_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to map aperture range");
    }

    err = sysmem_cap_manager_init(card->apt.cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the cap manager");
    }

    err = map_device(&bar_info[XEON_PHI_MMIO_BAR]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to map MMIO range");
    }

    card->mmio.vbase = (lvaddr_t) bar_info[XEON_PHI_MMIO_BAR].vaddr;
    card->mmio.length = bar_info[XEON_PHI_MMIO_BAR].bytes;

    card->state = XEON_PHI_STATE_PCI_OK;
}

static void pci_register(struct xeon_phi *phi, uint32_t bus, uint32_t dev, uint32_t fun)
{
    errval_t err;

    err = pci_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not connect to PCI\n");
    }

    err = pci_register_driver_irq(pci_init_card,
                                  PCI_DONT_CARE,
                                  PCI_DONT_CARE,
                                  PCI_DONT_CARE,
                                  PCI_VENDOR_ID_INTEL,
                                  PCI_DEVICE_KNC_225e,
                                  bus,
                                  dev,
                                  fun,
                                  interrupt_handler,
                                  phi);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not register the PCI device");
    }
}

/**
 * \brief initializes the coprocessor card
 *
 * \param phi pointer to the information structure
 */
errval_t xeon_phi_init(struct xeon_phi *phi, uint32_t bus, uint32_t dev, uint32_t fun)
{
    card = phi;

    pci_register(phi, bus,  dev,  fun);

    xeon_phi_reset(phi);

    interrupts_init(phi);

    device_init(phi);

    smpt_init(phi);

    xeon_phi_serial_init(phi);

    return SYS_ERR_OK;
}

/**
 * \brief performs a soft reset of the card
 *
 * \param phi   pointer to the card information
 */
errval_t xeon_phi_reset(struct xeon_phi *phi)
{
    if (phi->state == XEON_PHI_STATE_NULL) {
        return SYS_ERR_ILLEGAL_INVOCATION;
    }

    phi->state = XEON_PHI_STATE_RESET;

    xeon_phi_boot_t boot_registers;

    xeon_phi_boot_initialize(&boot_registers,
                             XEON_PHI_MMIO_TO_SBOX(phi),
                             XEON_PHI_MMIO_TO_DBOX(phi));

    // clearing the download status register before rest.
    xeon_phi_boot_download_wr(&boot_registers, 0x0);

    // perform the actual reset sequence
    xeon_phi_boot_reset_t res = xeon_phi_boot_reset_rd(&boot_registers);
    res = xeon_phi_boot_reset_reset_insert(res, 0x1);
    xeon_phi_boot_reset_wr(&boot_registers, res);

    // wait a bit to prevent potential problems
    milli_sleep(1000);

    xeon_phi_boot_postcode_t postcode;
    xeon_phi_boot_postcodes_t pc, pc_prev = 0;
    for (uint32_t time = 0; time < XEON_PHI_RESET_TIME; ++time) {
        postcode = xeon_phi_boot_postcode_rd(&boot_registers);
        pc = xeon_phi_boot_postcode_code_extract(postcode);
        if (pc_prev != pc) {
            debug_printf("Resetting: %s\n", xeon_phi_boot_postcodes_describe(pc));
            }
        if (postcode == xeon_phi_boot_postcode_invalid || postcode
                        == xeon_phi_boot_postcode_fatal
            || pc == xeon_phi_boot_postcode_memtf
            || pc == xeon_phi_boot_postcode_mempf) {
            break;
        }

        if (xeon_phi_boot_download_status_rdf(&boot_registers)) {
            phi->state = XEON_PHI_STATE_READY;
            debug_printf("Reset successful\n");
            /*
             * XXX; Maybe we should re-enable the IRQ if they were enabled beforehand
             * if (mic_ctx->msie)
             mic_enable_msi_interrupts(mic_ctx);
             mic_enable_interrupts(mic_ctx);
             mic_smpt_restore(mic_ctx);
             micscif_start(mic_ctx);
             */
            return SYS_ERR_OK;
        }

        pc_prev = pc;
        milli_sleep(XEON_PHI_RESET_TIME_UNIT);
    }

    if (phi->state != XEON_PHI_STATE_READY) {
        debug_printf("Reset Failed; %s\n", xeon_phi_boot_postcodes_describe(pc));
        XBOOT_DEBUG("Reset Failed (Post Code %c%c)\n",
                    xeon_phi_boot_postcode_raw_code0_extract(postcode),
                    xeon_phi_boot_postcode_raw_code1_extract(postcode));

        return 1; // todo> error code
    }

#if 0
#define ENABLE_MIC_INTERRUPTS(mmio) { \
    uint32_t sboxSice0reg = SBOX_READ((mmio), SBOX_SICE0); \
    sboxSice0reg |= SBOX_SICE0_DBR_BITS(0xf) | SBOX_SICE0_DMA_BITS(0xff); \
    SBOX_WRITE(sboxSice0reg, (mmio), SBOX_SICE0); }
#endif
    return SYS_ERR_OK;
}


/**
 * \brief maps the aperture memory range of the Xeon Phi into the drivers
 *        vspace to be able to load the coprocessor OS onto the card
 *
 * \param phi   pointer to the Xeon Phi structure holding aperture information
 * \param range how much bytes to map
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_map_aperture(struct xeon_phi *phi,
                               size_t range)
{
    if (phi->apt.vbase != 0) {
        return SYS_ERR_VM_ALREADY_MAPPED;
    }

    errval_t err;
    void *addr;
    err = vspace_map_one_frame(&addr, range, phi->apt.cap, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed for the apt mbar");
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    phi->apt.vbase = (lvaddr_t)addr;
    phi->apt.length = range;

    return SYS_ERR_OK;
}

/**
 * \brief unmaps the previously mapped aperture range when the programming
 *        completes.
 *
 * \param phi pointer to the Xeon Phi structure holiding mapping information
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_unmap_aperture(struct xeon_phi *phi)
{
    errval_t err;

    if (phi->apt.vbase == 0) {
        return SYS_ERR_OK;
    }

    err = vspace_unmap((void *)phi->apt.vbase);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed for the apt mbar");
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    phi->apt.vbase = 0;
    phi->apt.length = 0;

    return SYS_ERR_OK;
}
