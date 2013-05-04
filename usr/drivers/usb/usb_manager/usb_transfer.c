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
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb_error.h>
#include <usb/usb_device.h>
#include <usb/usb_xfer.h>
#include <usb/usb_request.h>

#include "usb_controller.h"
#include "usb_transfer.h"
#include "usb_request.h"
#include "usb_xfer.h"

static const struct usb_xfer_config usb_control_ep_cfg[USB_DEVICE_CTRL_XFER_MAX] =
        {

        /* This transfer is used for generic control endpoint transfers */

        [0] = {
        .type = USB_TYPE_CTRL, .endpoint = 0x00, /* Control endpoint */
        .direction = 0xFF, .bufsize = 1024, /* bytes */
        .flags = {
        .ext_buffer = 1,
        }, .usb_mode = USB_MODE_DUAL,
        /* both modes */
        },

        /* This transfer is used for generic clear stall only */

        [1] = {
                .type = USB_TYPE_CTRL,
                .endpoint = 0x00, /* Control pipe */
                .direction = 0xFF,
                .bufsize = sizeof(struct usb_device_request),
                .timeout = 1000, /* 1 second */
                .interval = 50, /* 50ms */
                .usb_mode = USB_MODE_HOST,
        },
        };

/**
 * \brief   this function is used to allocate the structure and resources
 *          needed for the default USB control endpoint transfer
 *
 * \param   device  the usb device we want to setup a usb transfer
 */
void usb_transfer_setup_ctrl_default(struct usb_device *device,
        struct usb_request_state *st)
{
    /* setting up transfers for the USB root hub is not allowed */
    if (device->parent_hub == NULL) {
        return;
    }

    /*
     * since the control transfers are always on the special control
     * ep, we can cache them and reuse later
     */
    struct usb_xfer *xfer = device->ctrl_xfer[0];

    uint8_t xfer_reuse = 0;

    if (xfer) {
        xfer_reuse = ((xfer->device_address == device->device_address)
                && (device->ctrl_ep_desc.wMaxPacketSize
                        == device->device_desc.bMaxPacketSize0));

        xfer->usb_manager_request_state = st;
        xfer->usb_manager_request_callback = st->callback;

        if ((device->flags.usb_mode == USB_MODE_DEVICE) && xfer_reuse) {

            usb_transfer_start(xfer);
            return;
        }
    }

    /*
     * we cannot reuse the USB transfer so we have to update the fields
     */
    device->ctrl_ep_desc.wMaxPacketSize = device->device_desc.bMaxPacketSize0;

    usb_transfer_unsetup(device->ctrl_xfer, USB_DEVICE_CTRL_XFER_MAX);

    uint8_t iface = 0;
    if (usb_transfer_setup(device, &iface, device->ctrl_xfer,
            usb_control_ep_cfg, USB_DEVICE_CTRL_XFER_MAX)) {
        debug_printf("could not allocate default control transfer\n");
        return;
    }

    xfer->usb_manager_request_state = st;
    xfer->usb_manager_request_callback = st->callback;

    // start the usb transfer
    usb_transfer_start(xfer);
}

/**
 * \brief   starts a USB transfer
 *
 * \param   xfer    the USB transfer to start
 */
void usb_transfer_start(struct usb_xfer *xfer)
{
    if (xfer == NULL) {
        return;
    }

    /*
     * set the started flag
     */
    if (!xfer->flags_internal.started) {
        xfer->flags_internal.started = 1;
    }

    /*
     * if the transfer is already transferring, then we do not
     * have to do sth.
     */
    if (xfer->flags_internal.transferring) {
        return;
    }

    struct usb_xfer_queue *queue = &(xfer->host_controller->done_queue);

    if (queue->current != xfer) {
        usb_xfer_enqueue(queue, xfer);
    }
}

/**
 * \brief   stops an usb transfer
 *
 * \param   xfer    the usb transfer to stop
 *
 */
void usb_transfer_stop(struct usb_xfer *xfer)
{
    if (xfer == NULL) {
        return;
    }

    /*
     * check if the pipe has already been opened.
     * if not we simply reset the started flag and return
     */
    if (!xfer->flags_internal.pipe_open) {
        if (xfer->flags_internal.started) {
            xfer->flags_internal.started = 0;
        }
        return;
    }

    xfer->error = USB_ERR_CANCELLED;

    // clear the flags
    xfer->flags_internal.pipe_open = 0;
    xfer->flags_internal.started = 0;

    /*
     * now stop the transfer, depending on whether we are
     * currently transferring or not
     */
    if (xfer->flags_internal.transferring) {
        if (xfer->flags_internal.cancellable) {
            if (!xfer->flags_internal.transfer_closed) {
                (xfer->endpoint->pipe_fn->close)(xfer);
                xfer->flags_internal.transfer_closed = 1;
            }
        }
    } else {
        (xfer->endpoint->pipe_fn->close)(xfer);

        struct usb_endpoint *ep = xfer->endpoint;
        struct usb_xfer_queue *queue = &(ep->transfers);

        /*
         * check if we need to start the next transfer
         * i.e. dequeue it from the wait queue
         */
        if (queue->current == xfer) {
            xfer = ((&queue->head)->first);
            if (xfer) {
                if ((xfer->wait_entry.next) != NULL)
                    (xfer->wait_entry.next)->wait_entry.prev_next = xfer
                            ->wait_entry.prev_next;
                else {
                    (&queue->head)->last_next = xfer->wait_entry.prev_next;
                }
                *(xfer)->wait_entry.prev_next = (xfer->wait_entry.next);
                xfer->wait_queue = 0;
                queue->current = xfer;

                (queue->command)(queue);
            }
        }
    }

}

/**
 * \brief   this function undoes the setup of the usb transfers
 *
 * \param   xfers       array of pointers to usb transfers to unsetup
 * \param   xfer_count  the number of xfers to unsetup
 */
void usb_transfer_unsetup(struct usb_xfer **xfers, uint16_t xfer_count)
{
    if (*xfers == NULL || xfer_count == 0) {
        return;
    }

    struct usb_xfer *xfer;

    while (xfer_count--) {
        xfer = xfers[xfer_count];

        if (xfer == NULL) {
            continue;
        }

        xfers[xfer_count] = NULL;

        xfer->endpoint->ref_allocation--;


    }
}

/**
 * \brief   this function allocates the resources for a number of usb transfers
 *
 * \param   device      the device we want to allocate the transfers
 * \param   ifaces      array of interfaces
 * \param   usb_xfers   pointer to an array of usb_xfer
 * \param   setups      setup parameter array
 * \para    setup_count the number of setups we have to do
 * \
 */
usb_error_t usb_transfer_setup(struct usb_device *device, const uint8_t *ifaces,
        struct usb_xfer **usb_xfers, const struct usb_xfer_config *setups,
        uint16_t setup_count)
{
    if (setup_count == 0) {
        debug_printf("usb_transfer_setup(): invalid setup count\n");
        return USB_ERR_INVAL;
    }

    if (ifaces == 0) {
        debug_printf("usb_transfer_setup(): interfaces array is NULL!");
    }

    struct usb_xfer_setup_params params;
    memset(&params, 0, sizeof(params));

    params.device = device;
    params.speed = device->speed;
    params.hc_max_packet_count = 1;

    /*
     * TODO: CHECK FOR DEVICE SPEED MAX
     */


    /*
     * setting up all transfers
     */

     /*
      * todo: initialize done queue callback wrapper
      */


    return USB_ERR_OK;
}

/**
 * \brief   checks if the transfer is completed
 *
 * \param   xfer    the USB transer we want to check
 *
 * \return  1:  the transfer is completed
 *          0:  the transfer is in progress
 */
uint8_t usb_transfer_completed(struct usb_xfer *xfer)
{
    /*
     * there is no transfer, so it is completed
     */
    if (xfer == NULL) {
        return 1;
    }

    /*
     * if the flags say we are transferring at the moment,
     * then we are not completed
     */
    if (xfer->flags_internal.transferring) {
        return 0;
    }

    /*
     * if we are waiting on a queue then we are not finished
     */
    if (xfer->wait_queue) {
        return 0;
    }

    /*
     * checking the done queue now
     */
    struct usb_xfer_queue *queue;
    queue = &(xfer->host_controller->done_queue);

    /*
     * we are scheduled for callback so not quite finished yet
     */
    if (queue->current == xfer) {
        return 0;
    }

    return 1;

}
