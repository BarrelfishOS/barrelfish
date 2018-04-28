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
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_manager_client.h>
#include <skb/skb.h>

#include "xeon_phi_internal.h"
#include "smpt.h"
#include "dma_service.h"
#include "service.h"
#include "xphi_service.h"
#include "interphi.h"
#include "domain.h"
#include "sysmem_caps.h"

uint8_t xeon_phi_dma_enabled = 1;
char *xeon_phi_mod_uri = XEON_PHI_NFS_PATH;
char *xeon_phi_mod_list = XEON_PHI_MOD_LIST;

struct xeon_phi *phis = NULL;

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
errval_t xeon_phi_event_poll(struct xeon_phi *phi, uint8_t do_yield)
{
    errval_t err;

    uint8_t idle = 0x1;
    uint8_t serial_recv = 0xFF;
    while (serial_recv-- && xeon_phi_serial_handle_recv(phi));

    err = xdma_service_poll(phi);
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
    }
}


/**
 * \brief Main function of the Xeon Phi Driver (Host Side)
 */
int main(int argc, char *argv[])
{
    errval_t err;


    debug_printf("Xeon Phi host module started.\n");

    for (uint8_t i = 1; i < argc; ++i) {
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


    vfs_init();

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "initializing octopus\n");
    }

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "initializing skb\n");
    }

    // map the nfs path
    err = mount_nfs_path(xeon_phi_mod_uri);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not map the NFS paths");
    }

    iref_t kaluga_iref = 0;
    err = nameservice_blocking_lookup("ddomain_controller", &kaluga_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to connect to ddomain controller");
    }

    err = ddomain_communication_init(kaluga_iref, atoi(argv[4]));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initiate communication with Kaluga");
    }

    debug_printf("Stopping and dispatching messagse\n");

    while(phis == NULL) {
        messages_wait_and_handle_next();
    }

    struct xeon_phi *phi = phis;
    while(true) {
        xeon_phi_event_poll(phi, true);
        phi = phi->next;
    }


    XDEBUG("Xeon Phi host module terminated.\n");

    return 0;
}
