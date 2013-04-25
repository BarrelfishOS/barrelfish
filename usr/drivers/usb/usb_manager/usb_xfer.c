/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "usb_xfer.h"

/**
 * \brief   this function adds a usb xfer to a usb transfer queue if it
 *          is not already in one.
 *
 * \param   queue   the queue the xfer should be added to
 * \param   xfer    the transfer to add to the queue
 * \
 */
void
usb_xfer_enqueue(struct usb_xfer_queue *queue, struct usb_xfer *xfer)
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
void
usb_xfer_dequeue(struct usb_xfer *xfer)
{
    struct usb_xfer_queue *queue;

    queue = xfer->wait_queue;
    if (queue) {
        if ((xfer->wait_entry.next) != NULL)
            (xfer->wait_entry.next)->wait_entry.prev_next = xfer
                    ->wait_entry.prev_next;
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
void
usb_xfer_done(struct usb_xfer *xfer, usb_error_t err)
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
