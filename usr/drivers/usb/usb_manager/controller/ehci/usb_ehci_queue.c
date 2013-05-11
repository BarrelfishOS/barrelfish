/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_xfer.h>

#include "../../usb_controller.h"
#include "../../usb_xfer.h"

#include "usb_ehci.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_queue.h"



void usb_ehci_enqueue_xfer_intrq(struct usb_xfer *xfer)
{
    /*
     * check if the transfer is already finished
     */
    if (usb_ehci_xfer_is_finished(xfer)) {
        return;
    }

    /*
     * enqueue it on the transfer interrupt queue
     */
    usb_xfer_enqueue(&xfer->host_controller->intr_queue, xfer);

    /*
     * TODO: start timeout handler
     * if (xfer->timeout != 0) {
     *   usbd_transfer_timeout_ms(xfer, &ehci_timeout, xfer->timeout);
     * }
     */
}


/**
 * \brief enqueues a new split isochronus transaction descriptor into the queue
 *
 * \param sitd the siTD to insert into the list
 * \param last the last element of the list
 */
usb_ehci_sitd_t *usb_ehci_enq_fs_td(usb_ehci_sitd_t *sitd, usb_ehci_sitd_t *last)
{
    /*
     * update the virtual links
     */
    sitd->next = last->next;
    sitd->prev = last;
    last->next = sitd;

    /*
     * update the physical links
     */
    sitd->sitd_next = last->sitd_next;
    last->sitd_next.address = sitd->sitd_self;

    return (sitd);

}

/*
 * \brief enqueues a new isochronus transaction descriptor into the queue
 *
 * \param std the iTD to insert into the list
 * \para last the last element of the list
 */
usb_ehci_itd_t *usb_ehci_enq_hs_td(usb_ehci_itd_t *std, usb_ehci_itd_t *last)
{
    /*
     * update the virtual links
     */
    std->next = last->next;
    std->prev = last;
    last->next = std;

    /*
     * update the physical links
     */
    std->itd_next = last->itd_next;
    last->itd_next.address = std->itd_self;

    return (std);

}


