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

#include "xeon_phi.h"
#include "interrupts.h"
#include "sleep.h"
#include "dma.h"

static uint32_t initialized = 0;

#define XEON_PHI_RESET_TIME 300


static struct xeon_phi *card;

static uint32_t pci_bus = PCI_DONT_CARE;
static uint32_t pci_device = PCI_DONT_CARE;
static uint32_t pci_function = PCI_DONT_CARE;

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
#define XEON_PHI_MMIO_BAR 4


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

    initialized=true;
}

static void
pci_init_card(struct device_mem* bar_info,
              int bar_count)
{
    errval_t err;

    if (initialized) {
        debug_printf("WARNING> Device already initialized\n");
        return;
    }

    // ok may be 5
    if (bar_count != 5) {
        USER_PANIC("There is something wrong. The Card should have 2 MBARs.");
    }

    err = map_device(&bar_info[XEON_PHI_APT_BAR]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to map aperture range");
    }

    err = map_device(&bar_info[XEON_PHI_MMIO_BAR]);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to map MMIO range");
    }

    card->apt.vbase = (lvaddr_t)bar_info[XEON_PHI_APT_BAR].vaddr;
    card->apt.length = bar_info[XEON_PHI_APT_BAR].bytes;
    card->apt.pbase = (lpaddr_t)bar_info[XEON_PHI_APT_BAR].paddr;
    card->mmio.vbase = (lvaddr_t)bar_info[XEON_PHI_MMIO_BAR].vaddr;
    card->mmio.length = bar_info[XEON_PHI_MMIO_BAR].bytes;
    card->mmio.pbase = (lpaddr_t)bar_info[XEON_PHI_MMIO_BAR].paddr;

    card->state = XEON_PHI_STATE_PCI_OK;
}

static void pci_register(struct xeon_phi *phi)
{
    errval_t err;

    err = pci_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not connect to PCI\n");
    }

    err = pci_register_driver_irq(pci_init_card, PCI_DONT_CARE,
                                  PCI_DONT_CARE,
                                  PCI_DONT_CARE,
                                  PCI_VENDOR_ID_INTEL,
                                  PCI_DEVICE_KNC_225e, pci_bus, pci_device,
                                  pci_function, interrupt_handler, phi);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not register the PCI device");
    }
}

/**
 * \brief initializes the coprocessor card
 *
 * \param phi pointer to the information structure
 */
errval_t xeon_phi_init(struct xeon_phi *phi)
{
    card = phi;

    pci_register(phi);

    xeon_phi_reset(phi);

    interrupts_init(phi);

    device_init(phi);

    dma_init(phi);

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
    xeon_phi_boot_postcodes_t pc;
    for (uint32_t time = 0; time < XEON_PHI_RESET_TIME; ++time) {
        postcode = xeon_phi_boot_postcode_rd(&boot_registers);
        pc = xeon_phi_boot_postcode_extract(postcode);
        debug_printf("Resetting; %s\n", xeon_phi_boot_postcodes_describe(pc));
        XBOOT_DEBUG("Resetting (Post Code %c%c)\n",
                     xeon_phi_boot_postcode_raw_code0_extract(postcode),
                     xeon_phi_boot_postcode_raw_code1_extract(postcode));

        if (postcode == xeon_phi_boot_postcode_invalid
                || postcode == xeon_phi_boot_postcode_fatal
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

        milli_sleep(1000);
    }

    if (phi->state != XEON_PHI_STATE_READY) {
        debug_printf("Reset Failed; %s\n", xeon_phi_boot_postcodes_describe(pc));
        XBOOT_DEBUG("Reset Failed (Post Code %c%c)\n",
                             xeon_phi_boot_postcode_raw_code0_extract(postcode),
                             xeon_phi_boot_postcode_raw_code1_extract(postcode));

        return 1;
    }

    return SYS_ERR_OK;
}


