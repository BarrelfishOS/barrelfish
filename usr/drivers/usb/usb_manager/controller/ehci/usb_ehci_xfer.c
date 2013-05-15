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

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_device.h>
#include <usb/usb_xfer.h>

#include "../../usb_controller.h"
#include "../../usb_xfer.h"
#include "usb_ehci.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_memory.h"
#include "usb_ehci_queue.h"

void usb_ehci_xfer_remove(struct usb_xfer *xfer, usb_error_t error)
{
    // get the host controller of this transfer
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

    switch(xfer->type) {
        case USB_TYPE_BULK:
        case USB_TYPE_CTRL:
            usb_ehci_deq_qh(xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                            hc->qh_async_last);
            break;
        case USB_TYPE_INTR:
            usb_ehci_deq_qh(xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                            hc->qh_intr_last[xfer->intr_qh_pos]);
            break;
        case USB_TYPE_ISOC:
            if (xfer->hcd_td_first && xfer->hcd_td_last) {
                switch (xfer->device->speed) {
                    case USB_SPEED_HIGH:
                        usb_ehci_xfer_hs_isoc_done(xfer);
                    case USB_SPEED_FULL:
                    case USB_SPEED_LOW:
                        usb_ehci_xfer_fs_isoc_done(xfer);
                    default:
                        debug_printf("WARNING: Unknown speed\n");
                        return;
                }
                xfer->hcd_td_first = NULL;
                xfer->hcd_td_last = NULL;
            }
            break;
        default:
            debug_printf("ERROR: Invalid transfer type");
            return;
            break;
    }

    usb_xfer_done(xfer, error);
}





void usb_ehci_xfer_standard_setup(struct usb_xfer *xfer, usb_ehci_qh_t **qh_last)
{

}


void usb_ehci_xfer_fs_isoc_done(struct usb_xfer *xfer)
{
    assert(!"NYI: Full speed isochronous done handling!");
}
void usb_ehci_xfer_hs_isoc_done(struct usb_xfer *xfer)
{
    assert(!"NYI: High speed isochronous done handling!");
}


static void usb_ehci_update_dt(struct usb_xfer *xfer, uint16_t actual_length,
                               uint16_t xfer_length)
{
    uint8_t dt = (actual_length / xfer->max_packet_size) & 0x1;
    uint16_t remaining = actual_length % xfer->max_packet_size;

    if (remaining > 0) {
        dt ^= 1;
    } else if (actual_length != xfer_length) {
        dt ^= 1;
    } else if (xfer_length == 0) {
        dt ^=1;
    }

    xfer->endpoint->data_toggle = dt;
}


/**
 * \brief   this function is
 */
static usb_error_t usb_ehci_xfer_done_process_frames(struct usb_xfer *xfer)
{


    usb_ehci_qtd_t *qtd = xfer->hcd_td_cache;
    usb_ehci_qtd_t *qtd_alt_next = qtd->alt_next;

    /*
     * update the frame length
     */
    if(xfer->actual_frames != xfer->num_frames) {
        xfer->frame_lengths[xfer->actual_frames] = 0;
    }

    uint16_t actual_length;
    uint8_t status = qtd->qtd_token.status;
    while(1) {
        actual_length = qtd->qtd_token.bytes;

        if (actual_length > qtd->len) {
            debug_printf("WARNING: Invalid status length. Halting EP\n");
            status |= USB_EHCI_QTD_STATUS_HALTED;
        } else {
            xfer->frame_lengths[xfer->actual_frames] += qtd->len - actual_length;
            usb_ehci_update_dt(xfer, qtd->len - actual_length, qtd->len);
        }

        /*
         * last transfer
         *  - the current qtd equal to the last td pointer of the xfer
         *  - set the qtd to NULL and stop processing
         */
        if(((void *)qtd) == xfer->hcd_td_last) {
            qtd = NULL;
            break;
        }

        /*
         * check for error conditions, i.e. the endpoint is halted
         *  - set the qtd to NULL and stop processing
         */
        if(status & USB_EHCI_QTD_STATUS_HALTED) {
            qtd = NULL;
            break;
        }

        /*
         * check for short transfers
         *  - if they are ok, then follow the alternative next pointer
         *  - else we are done
         */
        if (actual_length > 0) {
            if (xfer->flags_internal.short_frames_ok) {
                qtd->qtd_alt_next;
            } else {
                qtd = NULL;
            }
            break;
        }

        qtd = qtd->obj_next;

        /*
         * this frame is complete
         */
        if (qtd->alt_next != qtd_alt_next) {
            break;
        }
    }

    /* update the transfer cache used to signal processing */
    xfer->hcd_td_cache = qtd;

    if (status & USB_EHCI_QTD_STATUS_HALTED) {
        return (USB_ERR_STALLED);
    } else {
        return (USB_ERR_OK);
    }
}

/**
 * \brief   handles the completition of non-isochronus transfers
 *
 * \param   xfer the usb transfer to be removed
 */
void usb_ehci_xfer_done(struct usb_xfer *xfer)
{
    usb_error_t err = USB_ERR_OK;

    /*
     * iterate over all queue transfer descriptors
     */
    xfer->hcd_td_cache = xfer->hcd_td_first;

    /* CASE 1: control transfers */
    if (xfer->flags_internal.ctrl_xfer) {

        if (xfer->flags_internal.ctrl_header) {
            /* the header was not sent */
            err = usb_ehci_xfer_done_process_frames(xfer);
        }
        /* control transfers have one frame max per transfer */
        xfer->actual_frames = 1;

        if (xfer->hcd_td_cache == NULL) {
            /* remove the xfer */
            usb_ehci_xfer_remove(xfer, err);
        }
    }
    /* loop over the xfer till we get actual frames == num frames */
    while (xfer->actual_frames < xfer->num_frames) {
        err = usb_ehci_xfer_done_process_frames(xfer);

        xfer->actual_frames++;

        if (xfer->hcd_td_cache == NULL) {
            usb_ehci_xfer_remove(xfer, err);
        }
    }

    /* the control transfer has not started yet, remove it */
    if (xfer->flags_internal.ctrl_xfer && !xfer->flags_internal.ctrl_active) {
        err = usb_ehci_xfer_done_process_frames(xfer);
    }
    usb_ehci_xfer_remove(xfer, err);
}


/*
 * \brief   checks if the transfer is finished
 *
 * \param xfer  the transfer to check for completition
 *
 * \return 0:    the transfer is not finihsed
 *         Else: the the transfer is finished
 */
uint8_t usb_ehci_xfer_is_finished(struct usb_xfer *xfer)
{
   if (xfer->type == USB_TYPE_ISOC) {
        /*
         * TODO Handling of isochronus transfers
         */
        if (xfer->device->speed == USB_SPEED_HIGH) {
            assert(!"NYI: handling of high speed isochr transfers");
        } else {
            assert(!"NYI: handling of full speed isochr transfers");
        }
        return (0);
    }

    /*
     * non isochronus transfer
     */
    usb_ehci_qtd_t *qtd = xfer->hcd_td_cache;
    usb_ehci_qh_t *qh = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];

    uint8_t status =  qh->qh_status.status;

    /*
     * check if the status is still pending
     */
    if (status & USB_EHCI_QTD_STATUS_ACTIVE) {
        return (1);
    }

    /*
     * check if there is still an active qtd in this qh
     * this indicats that the transfer is not done
     */
    while(1) {
        status =  qtd->qtd_token.status;

        if (status & USB_EHCI_QTD_STATUS_ACTIVE) {
            xfer->hcd_td_cache = qtd;
        }

        /*
         * the last transfer descriptor is not active, this makes
         * the whole transfer done
         */
        if (((void *)qtd) == xfer->hcd_td_last) {
            break;
        }

        /*
         * the transfer is halted, this indicates an error
         * and implies the transfer is done
         */
        if (status & USB_EHCI_QTD_STATUS_HALTED) {
            break;
        }

        /*
         * a short packet indicates that the transfer is done
         * iff there is no alternative transfer
         *
         * follow the alternate transfer
         */
        if(qtd->qtd_token.bytes) {
            if (xfer->flags_internal.short_frames_ok) {
                if (qtd->alt_next) {
                    qtd = qtd->alt_next;
                    continue;
                }
                break;
            }
        }

        qtd = qtd->obj_next;
    }
    usb_ehci_xfer_done(xfer);
    return (1);
}

