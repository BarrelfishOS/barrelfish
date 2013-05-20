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
#include <usb/usb_descriptor.h>
#include <usb/usb_error.h>
#include <usb/usb_xfer.h>
#include <usb/usb_device.h>

#include "usb_controller.h"
#include "usb_xfer.h"



/**
 * \brief   this function adds a usb xfer to a usb transfer queue if it
 *          is not already in one.
 *
 * \param   queue   the queue the xfer should be added to
 * \param   xfer    the transfer to add to the queue
 * \
 */
void usb_xfer_enqueue(struct usb_xfer_queue *queue, struct usb_xfer *xfer)
{
    // check if already in a queue
    if (xfer->wait_queue == NULL) {
        xfer->wait_queue = queue;

        xfer->wait_entry.next = NULL;
        xfer->wait_entry.prev_next = (&queue->head)->last_next;

        *(&queue->head)->last_next = (xfer);
        (&queue->head)->last_next = &(((xfer))->wait_entry.next);
    }
}

/**
 * \brief   this function removes the usb xfer from the usb transfer queue
 *
 * \param   xfer    the transfer to remove to the queue
 *
 */
void usb_xfer_dequeue(struct usb_xfer *xfer)
{
    struct usb_xfer_queue *queue;

    queue = xfer->wait_queue;
    if (queue) {
        if ((xfer->wait_entry.next) != NULL)
            (xfer->wait_entry.next)->wait_entry.prev_next = xfer->wait_entry
                    .prev_next;
        else {
            (&queue->head)->last_next = xfer->wait_entry.prev_next;

        }
        *(xfer)->wait_entry.prev_next = (xfer->wait_entry.next);

        xfer->wait_queue = NULL;
    }
}

/**
 * \brief   this function handles complete transfers by removing them from
 *          the interrupt queue and inserting them into the done queue.
 *
 * \param   xfer   the completed usb transfer
 * \param   err    error condition of the transfer
 *
 */
void usb_xfer_done(struct usb_xfer *xfer, usb_error_t err)
{
    /*
     * if the transfer started, this flag has to be set to 1. Therefore
     * the transfer may be canceled. Just return then.
     */
    if (!xfer->flags_internal.transferring) {
        // clear the control active flag and return
        xfer->flags_internal.ctrl_active = 0;
        return;
    }

    // update error condition
    xfer->error = err;

    // dequeue from the queue
    usb_xfer_dequeue(xfer);

    struct usb_xfer_queue *done_queue = &xfer->host_controller->done_queue;

    if (done_queue->current != xfer) {
        usb_xfer_enqueue(done_queue, xfer);
    }

    // todo maybe some statistics?

    //todo: send message to driver that transfer is completed
    assert(!"Send message to client driver");
}

/**
 * \brief   this function is called from the xfer_setup function of the
 *          respective USB host controller driver. This function sets the
 *          correct values in the usb_xfer struct.
 *
 * \param   param   USB transfer setup parameters
 *
 */
void usb_xfer_setup_struct(struct usb_xfer_setup_params *param)
{
    struct usb_xfer *xfer = param->curr_xfer;

    if ((param->hc_max_packet_size == 0) || (param->hc_max_packet_count)
            || (param->hc_max_frame_size)) {

        param->err = USB_ERR_INVAL;

        xfer->max_hc_frame_size = 1;
        xfer->max_frame_size = 1;
        xfer->max_packet_size = 1;
        xfer->max_data_length = 0;
        xfer->num_frames = 0;
        xfer->max_frame_count = 0;
        return;
    }
    const struct usb_xfer_config *setup_config = param->xfer_setup;
    struct usb_endpoint_descriptor *ep_desc = xfer->endpoint->descriptor;
    uint8_t type = ep_desc->bmAttributes.xfer_type;

    uint32_t num_frlengths;
    uint32_t num_frbuffers;

    xfer->flags = setup_config->flags;
    xfer->num_frames = setup_config->frames;
    xfer->timeout = setup_config->timeout;
    xfer->interval = setup_config->interval;
    xfer->endpoint_number = ep_desc->bEndpointAddress.ep_number;
    xfer->max_packet_size = ep_desc->wMaxPacketSize;
    xfer->flags_internal.usb_mode = param->device->flags.usb_mode;

    param->bufsize = setup_config->bufsize;

    switch (param->speed) {
        case USB_SPEED_HIGH:
            switch (type) {
                case USB_ENDPOINT_TYPE_ISOCHR:
                case USB_ENDPOINT_TYPE_INTR:
                    xfer->max_packet_count += (xfer->max_packet_size >> 11) & 3;

                    if (xfer->max_packet_count > 3) {
                        xfer->max_packet_count = 3;
                    }
                    break;
                default:
                    /* nothing to do */
                    break;
            }
            xfer->max_packet_size &= 0x7FF;
            break;
        case USB_SPEED_SUPER:
            assert(!"NYI: No super speed support right now.");
            break;
        default:
            /* noop */
            break;
    }

    /*
     * range checks and filter values according to the host controller
     * values. Maximum packet size and count must not be bigger than supported
     * by the host controller.
     */

    if (xfer->max_packet_count > param->hc_max_packet_count) {
        xfer->max_packet_count = param->hc_max_packet_count;
    }

    if ((xfer->max_packet_size > param->hc_max_packet_size)
            || (xfer->max_packet_size == 0)) {
        xfer->max_packet_size = param->hc_max_packet_size;
    }

    /* TODO: Filter wMaxPacketSize according to standard sizes */

    /*
     * compute the maximum frame size as the maximum number of packets
     * multiplied by the maximum packet size
     */
    xfer->max_frame_size = xfer->max_packet_size * xfer->max_packet_count;

    switch (type) {
        case USB_ENDPOINT_TYPE_ISOCHR:
            /* TODO: ISOCHRONUS IMPLEMENTATION */
            assert(!"NYI: isochronus support not yet implemented");
            break;
        case USB_ENDPOINT_TYPE_INTR:
            /* handling of interrupt transfers */

            if (xfer->interval == 0) {
                /* interval is not set, get it from the endpoint descriptor */
                xfer->interval = ep_desc->bInterval;

                /*
                 * since the frames have different durations for FULL/LOW speed
                 * and high speed devices we have to do conversion
                 * from 125 us -> 1 ms
                 */
                if (param->speed != USB_SPEED_FULL
                        && param->speed != USB_SPEED_LOW) {
                    if (xfer->interval < 4) {
                        /* smallest interval 1 ms*/
                        xfer->interval = 1;
                    } else if (xfer->interval > 16) {
                        /* maximum interval 32ms */
                        xfer->interval = (0x1 << (16 - 4));
                    } else {
                        /* normal interval */
                        xfer->interval = (0x1 << (xfer->interval - 4));
                    }
                }
            }

            // ensure that the interval is at least 1
            xfer->interval += (xfer->interval ? 0 : 1);

            /*
             * calculate the frame down shift value
             */
            xfer->frame_shift = 0;
            uint32_t tmp = 1;
            while ((tmp != 0) && (tmp < xfer->interval)) {
                xfer->frame_shift++;
                tmp *= 2;
            }
            if (param->speed != USB_SPEED_FULL
                    && param->speed != USB_SPEED_LOW) {
                xfer->frame_shift += 3;
            }

            break;
        default:
            /* noop */
            break;
    }
    uint8_t max_length_zero = 0;
    if ((xfer->max_frame_size == 0) || (xfer->max_frame_size == 0)) {

        max_length_zero = 1;

        /*
         * check for minimum packet size
         */
        if ((param->bufsize <= 8) && (type != USB_ENDPOINT_TYPE_BULK)
                && (type != USB_ENDPOINT_TYPE_BULK)) {
            xfer->max_packet_size = 8;
            xfer->max_packet_count = 1;
            param->bufsize = 0;
            xfer->max_frame_size = xfer->max_packet_size
                    * xfer->max_packet_count;
        } else {
            /* error condition */
            param->err = USB_ERR_ZERO_MAXP;
            xfer->max_hc_frame_size = 1;
            xfer->max_frame_size = 1;
            xfer->max_packet_size = 1;
            xfer->max_data_length = 0;
            xfer->num_frames = 0;
            xfer->max_frame_count = 0;
            return;
        }
    }

    /*
     * if the buffer size is not given, we set it to the default size
     * such that the maximum frame fits into it. For isochronus we have
     * to multiply this by the expected number of frames.
     */
    if (param->bufsize == 0) {
        param->bufsize = xfer->max_frame_size;

        if (type == USB_ENDPOINT_TYPE_ISOCHR) {
            param->bufsize *= xfer->num_frames;
        }
    }

    if (xfer->flags.ext_buffer) {
        param->bufsize += (xfer->max_frame_size - 1);

        if (param->bufsize < xfer->max_frame_size) {
            param->err = USB_ERR_INVAL;
            xfer->max_hc_frame_size = 1;
            xfer->max_frame_size = 1;
            xfer->max_packet_size = 1;
            xfer->max_data_length = 0;
            xfer->num_frames = 0;
            xfer->max_frame_count = 0;
            return;
        }
        param->bufsize -= (param->bufsize % xfer->max_frame_size);
        if (type == USB_ENDPOINT_TYPE_CONTROL) {
            /* add the device request size for the setup message
             *  to the buffer length
             */
            param->bufsize += 8;
        }
    }
    xfer->max_data_length = param->bufsize;

    switch (type) {
        case USB_ENDPOINT_TYPE_ISOCHR:
            num_frlengths = xfer->num_frames;
            num_frbuffers = 1;
            break;

        case USB_ENDPOINT_TYPE_CONTROL:
            /* set the control transfer flag */

            xfer->flags_internal.ctrl_xfer = 1;
            if (param->bufsize <= 8) {
                /* no data stage of this control request */
                xfer->num_frames = 1;
            } else {
                /* there is a data stage of this control request */
                xfer->num_frames = 2;
            }
            num_frlengths = xfer->num_frames;
            num_frbuffers = xfer->num_frames;

            if (xfer->max_data_length < 8) {
                /* too small length: wrap around or too small buffer size */
                param->err = USB_ERR_INVAL;
                xfer->max_hc_frame_size = 1;
                xfer->max_frame_size = 1;
                xfer->max_packet_size = 1;
                xfer->max_data_length = 0;
                xfer->num_frames = 0;
                xfer->max_frame_count = 0;
                return;
            }
            xfer->max_data_length -= 8;
            break;

        default:
            break;
    }

    xfer->max_frame_count = xfer->num_frames;
    xfer->frame_lengths = param->xfer_length_ptr;

    if (!xfer->flags.ext_buffer) {
        assert(!"NYI: allocating a local buffer");
    }

    /*
     * we expect to have no data stage, so set it to the correct value
     */
    if (max_length_zero) {
        xfer->max_data_length = 0;
    }

    /* round up maximum buf size */
    if (param->bufsize_max < param->bufsize) {
        param->bufsize_max = param->bufsize;
    }

    xfer->dma_page = param->dma_page;

    xfer->max_hc_frame_size = (param->hc_max_frame_size
            - (param->hc_max_frame_size % xfer->max_frame_size));
    if (xfer->max_hc_frame_size == 0) {
        param->err = USB_ERR_INVAL;
        xfer->max_hc_frame_size = 1;
        xfer->max_frame_size = 1;
        xfer->max_packet_size = 1;
        xfer->max_data_length = 0;
        xfer->num_frames = 0;
        xfer->max_frame_count = 0;
        return;
    }

    if (param->buf) {
        //assert(!"NYI: initialize frame buffers");

    }

}

