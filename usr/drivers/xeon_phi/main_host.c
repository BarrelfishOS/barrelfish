/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
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
#include <barrelfish/nameservice_client.h>
#include <octopus/octopus.h>
#include <vfs/vfs.h>
#include <pci/pci.h>
#include <pci/devids.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_manager_client.h>

#include "xeon_phi_internal.h"
#include "smpt.h"
#include "dma_service.h"
#include "service.h"
#include "xphi_service.h"
#include "interphi.h"
#include "domain.h"
#include "sysmem_caps.h"

struct xeon_phi xphi;

uint8_t xphi_dma_enabled = 1;

/**
 * \brief mounts the NFS path given by parameter
 *
 * \param uri   NFS uri to mount
 *              NULL if multiboot path is used instead
 *
 * \return SYS_ERR_OK on success
 *         errval on error
 */
static errval_t mount_nfs_path(char *uri)
{
    errval_t err;

    if (uri == NULL || strncmp(uri, "nfs://", 6)) {
        return SYS_ERR_OK;
    }

    err = vfs_mkdir(XEON_PHI_NFS_MNT);
    switch(err_no(err)) {
        case SYS_ERR_OK:
        case FS_ERR_EXISTS:
            break;
        default:
            return err;
    }

    return  vfs_mount(XEON_PHI_NFS_MNT, uri);
}

/**
 * \brief handles events on the waitset and polls for completed DMA transfers
 *        and new data on the serial line (host only)
 *
 * \param do_yield if set, yield thread if no event was discovered
 *
 * \return SYS_ERR_OK if an event was handled
 *         LIB_ERR_NO_EVENT if there was no evetn
 */
errval_t xeon_phi_event_poll(uint8_t do_yield)
{
    errval_t err;

    uint8_t idle = 0x1;

    uint8_t serial_recv = 0xFF;
    while (serial_recv-- && xeon_phi_serial_handle_recv());


    err = xdma_service_poll(&xphi);
    switch(err_no(err)) {
        case SYS_ERR_OK:
            idle = 0;
            break;
        case DMA_ERR_DEVICE_IDLE:
            /* no op */
            break;
        default:
            return err;
            break;
    }

    err = event_dispatch_non_block(get_default_waitset());
    switch(err_no(err)) {
        case LIB_ERR_NO_EVENT :
            if (idle) {
                if (do_yield) {
                    thread_yield();
                    return SYS_ERR_OK;
                } else {
                    return LIB_ERR_NO_EVENT;
                }
            } else {
                return SYS_ERR_OK;
            }
        default:
            return err;
            break;
    }
    return SYS_ERR_OK;
}


/**
 * \brief Main function of the Xeon Phi Driver (Host Side)
 */
int main(int argc,
         char *argv[])
{
    errval_t err;

    char *xeon_phi_mod_uri = XEON_PHI_NFS_PATH;
    char *xeon_phi_mod_list = XEON_PHI_MOD_LIST;

    uint8_t xeon_phi_dma_enabled = 1;

    XDEBUG("Xeon Phi host module started.\n");

    memset(&xphi, 0, sizeof(xphi));

    /*
     * Parsing of cmdline arguments.
     *
     * When started by Kaluga, the last element of the cmdline will contain
     * the basic PCI information of the device.
     * VENDORID:DEVICEID:BUS:DEV:FUN
     */
    uint32_t vendor_id, device_id;
    uint32_t bus = PCI_DONT_CARE, dev = PCI_DONT_CARE, fun = PCI_DONT_CARE;

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1], "%x:%x:%x:%x:%x", &vendor_id,
                                 &device_id, &bus, &dev, &fun);
        if (parsed != 5) {
            XDEBUG("WARNING: cmdline parsing failed. Using PCI Address [0,0,0]");
            bus = PCI_DONT_CARE;
            dev = PCI_DONT_CARE;
            fun = PCI_DONT_CARE;
        } else {
            if (vendor_id != 0x8086 || ((device_id & 0xFFF0) != 0x2250)) {
                USER_PANIC("unexpected vendor / device id: [%x, %x]", vendor_id,
                           device_id);
                return -1;
            }
            XDEBUG("Initializing Xeon Phi with PCI address "
                   "[%u,%u,%u]\n",
                   bus, dev, fun);
        }
    } else {
        XDEBUG("WARNING: Initializing Xeon Phi with unknown PCI address "
               "[0,0,0]\n");
    }

    for (uint8_t i = 1; i < argc - 1; ++i) {
        if (strncmp(argv[i], "--tftp=", 7)==0) {
            xeon_phi_mod_uri = argv[i] + 7;
        } else if (strncmp(argv[i], "--nfs=", 6)==0) {
            xeon_phi_mod_uri = argv[i] + 6;
        } else if (strncmp(argv[i], "--modlist=", 10)==0) {
            xeon_phi_mod_list = argv[i] + 10;
        } else if (strncmp(argv[i], "--no-dma", 8)==0) {
            xeon_phi_dma_enabled = 0;
        } else if (strncmp(argv[i], "auto", 4)==0) {
            /* no op */
        } else {
            XDEBUG("WARNING: unknown argument {%s}\n", argv[i]);
        }
    }

    XDEBUG("Xeon Phi Images: mod_list: {%s}, mod_uri: {%s}\n", xeon_phi_mod_list,
           xeon_phi_mod_uri);

    /* set the client flag to false */
    xphi.is_client = XEON_PHI_IS_CLIENT;

    vfs_init();

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "initializing octopus\n");
    }

    // map the nfs path
    err = mount_nfs_path(xeon_phi_mod_uri);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not map the NFS paths");
    }

    err = service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the driver service\n");
    }

    uint8_t num;
    iref_t *irefs;
    err = xeon_phi_manager_client_register(xphi.iref, &xphi.id, &num, &irefs);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register with the Xeon Phi manager\n");
    }

    xphi.state = XEON_PHI_STATE_NULL;

    err = xeon_phi_init(&xphi, bus, dev, fun);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not do the card initialization\n");
    }

    err = xeon_phi_boot(&xphi, xeon_phi_mod_uri, xeon_phi_mod_list);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not boot the card\n");
    }

    interphi_wait_for_client(&xphi);

    err = service_register(&xphi, irefs, num);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register with the other drivers");
    }

    if (xeon_phi_dma_enabled) {
        err = xdma_service_init(&xphi);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not initialize the DMA engine\n");
        }
    }

    err = xeon_phi_service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the messaging service");
    }

    /*
     * in case there are more than one Xeon Phi present in the system, indicated
     * by an id > 0, the driver will register itself with the other Xeon Phi
     * driver instances running in the system and initializes the inter-Phi
     * messaging frame
     */
    if (xphi.id != 0) {
        XDEBUG("Doing Intra Xeon Phi setup with %u other instances\n", xphi.id);
        for (uint32_t i = 0; i < xphi.id; ++i) {
            /* initialize the messaging frame */
            err = interphi_init_xphi(i, &xphi, NULL_CAP, XEON_PHI_IS_CLIENT);
            if (err_is_fail(err)) {
                XDEBUG("Could not initialize messaging\n");
                continue;
            }
        }
    }


    char buf[20];
    snprintf(buf, 20, "xeon_phi.%u.ready", xphi.id);

    XDEBUG("registering ready\n");
    err = domain_register(buf, 0xcafebabe);
    assert(err_is_ok(err));

    /* signal for the test */
    debug_printf("Xeon Phi operational: %s\n", buf);

    XDEBUG("initialization done. Going into main message loop\n");

    /* starts the basic handler service. This function should not return */
    service_start(&xphi);

    XDEBUG("Xeon Phi host module terminated.\n");

    return 0;
}
