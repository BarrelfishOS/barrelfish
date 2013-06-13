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

#include <usb_controller.h>
#include <usb_xfer.h>
#include <usb_memory.h>
#include "usb_ehci.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_memory.h"
#include "usb_ehci_queue.h"

void usb_ehci_xfer_setup(struct usb_xfer_setup_params *param)
{
    USB_DEBUG_TR_ENTER;
    struct usb_xfer *xfer = param->curr_xfer;

    uint32_t num_qtd = 0;
    uint32_t num_qh = 0;
    uint32_t num_sitd = 0;
    uint32_t num_itd = 0;

    param->hc_max_packet_size = 0x400;
    param->hc_max_packet_count = 1;
    param->hc_max_frame_size = USB_EHCI_QTD_MAX_BYTES;

    /*
     * calculate the number of needed descriptors
     */

    switch (param->type) {
        case USB_TYPE_CTRL:
            param->num_pages = 5;
            usb_xfer_setup_struct(param);
            num_qh = 1;
            num_qtd = ((2 * xfer->num_frames) + 1 + /* status phase */
            (xfer->max_data_length / xfer->max_hc_frame_size));
            break;
        case USB_TYPE_BULK:
            param->num_pages = 5;
            usb_xfer_setup_struct(param);

            num_qh = 1;
            num_qtd = ((2 * xfer->num_frames)
                    + (xfer->max_data_length / xfer->max_hc_frame_size));
            break;
        case USB_TYPE_ISOC:
            if (xfer->device->speed == USB_SPEED_HIGH) {
                param->hc_max_frame_size = 0xC00;
                param->hc_max_packet_count = 3;
                param->num_pages = 7;
                usb_xfer_setup_struct(param);

                num_itd = ((xfer->num_frames + 7) / 8) << xfer->frame_shift;

            } else {
                param->num_pages = 2;
                param->hc_max_packet_size = 0x3FF;
                param->hc_max_frame_size = 0x3FF;

                usb_xfer_setup_struct(param);

                num_sitd = xfer->num_frames;
            }
            break;
        case USB_TYPE_INTR:
            switch (param->speed) {
                case USB_SPEED_HIGH:
                    param->num_pages = 7;
                    param->hc_max_packet_count = 3;
                    break;
                case USB_SPEED_FULL:
                    param->num_pages = 2;
                    /* FS bytes per HS frame */
                    param->hc_max_packet_size = 188;
                    break;
                default:
                    param->num_pages = 2;
                    /* FS bytes per HS frame / 8 */
                    param->hc_max_packet_size = (188 / 8);
                    break;
            }
            usb_xfer_setup_struct(param);
            num_qh = 1;
            num_qtd = ((2 * xfer->num_frames)
                    + (xfer->max_data_length / xfer->max_hc_frame_size));
            break;
        default:
            usb_xfer_setup_struct(param);
            param->hc_max_frame_size = 0x400;
            break;
    }
    uint8_t alloc_dma_set;

    USB_DEBUG_XFER_HC("usb_ehci_xfer_setup() Start Allocating queue...\n");

    do {
        alloc_dma_set = 0;
        if (param->err != USB_ERR_OK) {
            debug_printf("Error while setting up usb transfer \n");
            return;
        }

        /*
         * allocate the queue heads and transfer descriptors
         * according to the number we calculated abote
         */
        void *obj_last = NULL;
        uint32_t td_alloc;

        USB_DEBUG_XFER_HC("Allocating %u iTDs...\n", num_itd);
        for (td_alloc = 0; td_alloc < num_itd; td_alloc++) {
            usb_ehci_itd_t *itd = usb_ehci_itd_alloc();

            if (itd == NULL) {
                USB_DEBUG("ERROR: Failed to allocate iTD descriptor");
                itd = obj_last;
                while (itd != NULL) {
                    usb_ehci_itd_free(itd);
                    itd = itd->obj_next;
                }
                param->err = USB_ERR_NOMEM;
                break;
            }

            itd->obj_next = obj_last;

            obj_last = itd;
        }

        USB_DEBUG_XFER_HC("Allocating %u siTDs...\n", num_sitd);
        for (td_alloc = 0; td_alloc < num_sitd; td_alloc++) {
            usb_ehci_sitd_t *sitd = usb_ehci_sitd_alloc();

            if (sitd == NULL) {
                USB_DEBUG("ERROR: Failed to allocate iTD descriptor");
                sitd = obj_last;
                while (sitd != NULL) {
                    usb_ehci_sitd_free(sitd);
                    sitd = sitd->obj_next;
                }
                param->err = USB_ERR_NOMEM;
                break;
            }

            sitd->obj_next = obj_last;

            obj_last = sitd;
        }
        USB_DEBUG_XFER_HC("Allocating %u qTDs...\n", num_qtd);
        for (td_alloc = 0; td_alloc < num_qtd; td_alloc++) {
            usb_ehci_qtd_t *qtd = usb_ehci_qtd_alloc();

            if (qtd == NULL) {
                USB_DEBUG("ERROR: Failed to allocate qTD descriptor");
                qtd = obj_last;
                while (qtd != NULL) {
                    usb_ehci_qtd_free(qtd);
                    qtd = qtd->obj_next;
                }
                param->err = USB_ERR_NOMEM;
                break;
            }
            qtd->obj_next = obj_last;

            obj_last = qtd;
        }

        xfer->hcd_td_start[xfer->flags_internal.curr_dma_set] = obj_last;

        obj_last = NULL;

        //USB_DEBUG("usb_ehci_xfer_setup() Allocating %u QHs...\n", num_qh);
        for (td_alloc = 0; td_alloc < num_qh; td_alloc++) {
            usb_ehci_qh_t *qh = usb_ehci_qh_alloc();
            if (qh == NULL) {
                USB_DEBUG("ERROR: Failed to allocate iTD descriptor");
                qh = obj_last;
                while (qh != NULL) {
                    usb_ehci_qh_free(qh);
                    qh = qh->obj_next;
                }
                param->err = USB_ERR_NOMEM;
                break;
            }
            qh->obj_next = obj_last;
            obj_last = qh;
        }

        xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set] = obj_last;

        if (!xfer->flags_internal.curr_dma_set) {
            xfer->flags_internal.curr_dma_set = 1;
            alloc_dma_set = 1;
        }
    } while (alloc_dma_set);

    USB_DEBUG_TR_RETURN;
}

void usb_ehci_xfer_unsetup(struct usb_xfer *xfer)
{
    USB_DEBUG("NOTICE: freeing up the resources is nyi!\n");
    return;
}

void usb_ehci_xfer_remove(struct usb_xfer *xfer, usb_error_t error)
{
    USB_DEBUG_TR_ENTER;
    // get the host controller of this transfer
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control;

    switch (xfer->type) {
        case USB_TYPE_BULK:
        case USB_TYPE_CTRL:
            usb_ehci_deq_qh(
                    xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
                    hc->qh_async_last);
            break;
        case USB_TYPE_INTR:
            usb_ehci_deq_qh(
                    xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set],
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
            USB_DEBUG("ERROR: Invalid transfer type");
            USB_DEBUG_TR_RETURN;
            return;
            break;
    }

    usb_xfer_done(xfer, error);
    USB_DEBUG_TR_RETURN;
}

struct usb_ehci_qtd_setup_param {
    usb_ehci_hc_t *hc;
    usb_ehci_qtd_t *td;
    usb_ehci_qtd_t *td_next;
    usb_ehci_qtd_token_t qtd_token;
    uint32_t length_avg;
    uint32_t length;
    uint16_t max_frame_size;
    uint8_t shortpkt;
    uint8_t auto_data_toggle;
    uint8_t setup_alt_next;
    uint8_t last_frame;
    struct usb_dma_page *pages;
    uint16_t num_pages;
};

/**
 * \brief   setting up the queue element transfer descriptors for an
 *          USB transfer
 *
 * \param   setup   the setup parameters for the qtd
 */
static void usb_ehci_xfer_qtd_setup(struct usb_ehci_qtd_setup_param *setup)
{
    USB_DEBUG_TR_ENTER;
    /* pointers to the terminated queue head */
    usb_paddr_t terminate = setup->hc->qh_terminate->qh_self;
    usb_paddr_t qtd_alt_next = setup->hc->qh_terminate->qh_self;

    /* variables for tracking the length */

    uint32_t length_avg = 0;
    uint32_t length_old = setup->length;

    usb_ehci_qtd_t *td;
    usb_ehci_qtd_t *td_next;
    usb_ehci_qtd_t *td_alt_next;

    uint8_t shortpkt_old = setup->shortpkt;

    uint8_t precompute = 1;

    do {
        td = setup->td;
        td_next = setup->td_next;

        while (1) {
            if (setup->length == 0) {

                if (setup->shortpkt) {
                    /* the short packet */
                    break;
                }

                /* zero length packet, this one has to be sent last */
                setup->shortpkt = 1;
                length_avg = 0;
            } else {

                /* we have some data to process */
                length_avg = setup->length_avg;

                if (setup->length < length_avg) {
                    /*
                     * handle the case when the lenght of this packet is
                     * smaller than the average packet length, so update
                     * the average length and if it is smaller than the
                     * max_frame_size, indicate a short packet
                     */
                    if (setup->length % setup->max_frame_size) {
                        setup->shortpkt = 1;
                    }
                    length_avg = setup->length;
                }

            }

            assert(td_next != NULL);

            /* iterate to the next qTD */
            td = td_next;
            td_next = td->obj_next;

            if (precompute) {
                /*
                 * we are in the precomputing phase, so update remaining
                 * length and continue
                 */
                setup->length -= length_avg;
                continue;
            }

            /* we are not precomputing anymore so fill out the qTD */
            td->qtd_token = setup->qtd_token;
            td->qtd_token.ioc = 1;
            td->qtd_token.bytes = length_avg;

            if (length_avg == 0) {
                if (setup->auto_data_toggle == 0) {
                    /* update the data toggle for the last, zero length packet */
                    setup->qtd_token.data_toggle = 1;
                }
                td->len = 0;
                td->qtd_bp[0].bp = 0;
                td->qtd_bp[1].bp = 0;
                /* XXX: NO 64 bit support at the moment ! */
            } else {
                if (setup->auto_data_toggle == 0) {
                    /* update data toggle */
                    if (((length_avg + setup->max_frame_size - 1)
                            / setup->max_frame_size) & 1) {
                        setup->qtd_token.data_toggle = 1;
                    }
                }
                /* set the length of this qTD */
                td->len = length_avg;

                /* update the remaining length to process */
                setup->length -= length_avg;

                uint32_t buf_offset = 0;
                memset(td->qtd_bp, 0, sizeof(td->qtd_bp));
                td->qtd_bp[0].bp = (setup->pages->phys_addr) & (~0xFFF);  //usb_ehci_buffer_page_alloc();

                assert(!((setup->pages->phys_addr) & (0xFFF)));

                uint8_t pages_count = 1;

                /* fill in the buffer pointers */
                while (length_avg > USB_EHCI_BUFFER_SIZE) {

                    length_avg -= USB_EHCI_BUFFER_SIZE;
                    buf_offset += USB_EHCI_BUFFER_SIZE;
                    /* TODO: set to the pages ... */
                    td->qtd_bp[pages_count].bp = (setup->pages->phys_addr
                            + buf_offset) & (~0xFFF);

                    assert(!((setup->pages->phys_addr + buf_offset) & (0xFFF)));

                    pages_count++;
                }

                /* the last remainder < USB_EHCI_BUFFER_SIZE */
                buf_offset += length_avg;
                td->qtd_bp[pages_count].bp = (setup->pages->phys_addr
                        + buf_offset - 1) & (~0xFFF);
            }

            if (td_next) {
                td->qtd_next = td_next->qtd_self;
            }

            td->qtd_alt_next = qtd_alt_next;
            td->alt_next = td_alt_next;
        }

        if (precompute) {
            precompute = 0;

            qtd_alt_next = terminate;

            /* this was the last frame, so we have no alternative poiters */
            if (setup->last_frame) {
                td->alt_next = NULL;
            } else {
                td_alt_next = td_next;
                if (setup->setup_alt_next) {
                    qtd_alt_next = td_next->qtd_self;
                }
            }

            setup->shortpkt = shortpkt_old;
            setup->length = length_old;
        } else {
            /* break the loop */

            //precompute = 1;
            break;
        }

    } while (!precompute);

    setup->td = td;
    setup->td_next = td_next;

    USB_DEBUG_TR_RETURN;
}

void usb_ehci_xfer_standard_setup(struct usb_xfer *xfer,
        usb_ehci_qh_t **qh_last)
{
    USB_DEBUG_TR_ENTER;

    usb_ehci_qh_t *qh;

    // toggle the DMA set
    xfer->flags_internal.curr_dma_set ^= 1;

    usb_ehci_qtd_t *td = xfer->hcd_td_start[xfer->flags_internal.curr_dma_set];

    xfer->hcd_td_first = td;
    xfer->hcd_td_cache = td;

    /*
     * initialize some fields of the setup parameters
     */
    struct usb_ehci_qtd_setup_param setup = {
        .length_avg = xfer->max_hc_frame_size,
        .max_frame_size = xfer->max_frame_size,
        .hc = (usb_ehci_hc_t *) xfer->host_controller->hc_control,
        .td_next = td,
        .td = NULL,
        .setup_alt_next = xfer->flags_internal.short_frames_ok,
    };

    /* set the data toggle for control tansfers or normal transfers */
    if (xfer->flags_internal.ctrl_xfer) {
        if (xfer->endpoint->data_toggle) {
            setup.qtd_token.data_toggle = 1;
        }
        setup.auto_data_toggle = 0;
    } else {
        setup.auto_data_toggle = 1;
    }

    /*
     * set the number of retries to 3
     */
    if ((xfer->device->parent_hs_hub != NULL)
            || (xfer->device->device_address != 0)) {
        setup.qtd_token.err_count = 3;
    }

    uint32_t frames = 0;
    /*
     * prepend a setup message if it is a control transfer and
     * we need to send headers
     */
    if (xfer->flags_internal.ctrl_xfer) {
        if (xfer->flags_internal.ctrl_header) {

            xfer->endpoint->data_toggle = 0;
            memset(&setup.qtd_token, 0, sizeof(setup.qtd_token));
            setup.qtd_token.err_count = 3;
            setup.qtd_token.status = USB_EHCI_QTD_STATUS_ACTIVE;
            setup.qtd_token.pid = USB_EHCI_QTD_PID_SETUP;
            setup.qtd_token.data_toggle = 0;

            setup.length = xfer->frame_lengths[0];
            setup.shortpkt = setup.length ? 1 : 0;
            setup.pages = xfer->frame_buffers[0];

            /*
             * we have just one frame, this means we just have a setup phase
             * but no data or status stage.
             */
            if (xfer->num_frames == 1) {
                if (xfer->flags_internal.ctrl_active) {
                    setup.last_frame = 1;
                    setup.setup_alt_next = 0;
                }
            }

            usb_ehci_xfer_qtd_setup(&setup);
        }
        frames = 1;
    }

    /*
     * setup the remaining frames
     */
    while (frames < xfer->num_frames) {
        setup.length = xfer->frame_lengths[frames];
        setup.pages = xfer->frame_buffers[frames];
        frames++;

        if (frames == xfer->num_frames) {
            if (xfer->flags_internal.ctrl_xfer) {
                if (xfer->flags_internal.ctrl_active) {
                    setup.last_frame = 1;
                    setup.setup_alt_next = 0;
                }
            } else {
                setup.last_frame = 1;
                setup.setup_alt_next = 0;
            }
        }

        memset(&setup.qtd_token, 0, sizeof(setup.qtd_token));
        setup.qtd_token.err_count = 3;
        setup.qtd_token.data_toggle = 1;

        /* we have to send a short packet if length = 0 */
        if (setup.length == 0) {
            setup.shortpkt = 0;
        } else {
            setup.shortpkt = (xfer->flags.short_xfer_forced) ? 0 : 1;
        }

        setup.qtd_token.status = USB_EHCI_QTD_STATUS_ACTIVE;
        if (xfer->ed_direction == USB_ENDPOINT_DIRECTION_IN) {
            setup.qtd_token.pid = USB_EHCI_QTD_PID_IN;
        } else {
            setup.qtd_token.pid = USB_EHCI_QTD_PID_OUT;
        }

        usb_ehci_xfer_qtd_setup(&setup);
    }

    /*
     * we have to append a status stage, if the xfer is a control transfer
     * and the xfer is not active
     */
    if (xfer->flags_internal.ctrl_xfer && !xfer->flags_internal.ctrl_active) {

        memset(&setup.qtd_token, 0, sizeof(setup.qtd_token));
        setup.qtd_token.err_count = 3;
        setup.qtd_token.data_toggle = 1;
        setup.qtd_token.status = USB_EHCI_QTD_STATUS_ACTIVE;
        if (xfer->ed_direction == USB_ENDPOINT_DIRECTION_IN) {
            setup.qtd_token.pid = USB_EHCI_QTD_PID_OUT;
        } else {
            setup.qtd_token.pid = USB_EHCI_QTD_PID_IN;
        }
        setup.length = 0;
        setup.shortpkt = 0;
        setup.last_frame = 1;
        setup.setup_alt_next = 0;
        setup.pages = NULL;

        usb_ehci_xfer_qtd_setup(&setup);
    }

    /* all the qTDs have ben setup now. so we can update the QH */

    td = setup.td;

    /* set the links to terminate for the last TD */
    td->qtd_next = USB_EHCI_LINK_TERMINATE;
    td->qtd_alt_next = USB_EHCI_LINK_TERMINATE;

    xfer->hcd_td_last = td;

    /* get the qh */
    qh = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];

    qh->qh_ep.device_address = xfer->device_address;
    qh->qh_ep.ep_number = xfer->endpoint_number;
    qh->qh_ep.max_packet_size = xfer->max_packet_size;

    switch (xfer->device->speed) {
        case USB_SPEED_HIGH:
            qh->qh_ep.ep_speed = USB_EHCI_QH_SPEED_HIGH;
            if (xfer->type != USB_TYPE_INTR) {
                qh->qh_ep.nak_count_reload = 8;
            }
            break;
        case USB_SPEED_FULL:
            qh->qh_ep.ep_speed = USB_EHCI_QH_SPEED_FULL;
            if (xfer->type == USB_TYPE_CTRL) {
                qh->qh_ep.is_control_ep = 1;
            }
            if (xfer->type != USB_TYPE_INTR) {
                qh->qh_ep.nak_count_reload = 1;
            }
            break;
        case USB_SPEED_LOW:
            qh->qh_ep.ep_speed = USB_EHCI_QH_SPEED_LOW;
            if (xfer->type == USB_TYPE_CTRL) {
                qh->qh_ep.is_control_ep = 1;
            }
            if (xfer->type != USB_TYPE_INTR) {
                qh->qh_ep.nak_count_reload = 1;
            }
            break;
        default:
            assert(!"ERROR: WRING SPEED\n");
            break;
    }

    if (setup.auto_data_toggle == 0) {
        qh->qh_ep.data_toggle_ctrl = 1;
    }

    qh->qh_ep.complete_mask = xfer->endpoint->hs_cmask;
    qh->qh_ep.irq_mask = xfer->endpoint->hs_smask;
    qh->qh_ep.mult = xfer->max_packet_count & 0x3;
    qh->qh_ep.hub_addr = xfer->device->hs_hub_address;
    qh->qh_ep.port_number = xfer->device->hs_hub_port_number;

    qh->qh_curr_qtd = 0;

    memset(&qh->qh_status, 0, sizeof(qh->qh_status));
    if (setup.auto_data_toggle && xfer->endpoint->data_toggle) {
        qh->qh_status.data_togglet = 1;
    } else {
        qh->qh_status.data_togglet = 0;
    }

    td = xfer->hcd_td_first;

    assert(!(td->qtd_self & 0x1F));

    qh->qh_next_qtd = td->qtd_self;
    qh->qh_alt_next_qtd = USB_EHCI_LINK_TERMINATE;

    if (xfer->device->flags.self_suspended == 0) {
        usb_ehci_enq_qh(qh, *qh_last);
    }

    USB_DEBUG_TR_RETURN;
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
        dt ^= 1;
    }

    xfer->endpoint->data_toggle = dt;
}

/**
 * \brief   this function is
 */
static usb_error_t usb_ehci_xfer_done_process_frames(struct usb_xfer *xfer)
{
    USB_DEBUG_TR_ENTER;

    if (xfer == NULL) {
        USB_DEBUG("WARNING: xfer is NULL.\n");
        return (USB_ERR_OK);
    }

    usb_ehci_qtd_t *qtd = xfer->hcd_td_first;
    usb_ehci_qtd_t *qtd_alt_next = qtd->alt_next;

    qtd = xfer->hcd_td_cache;

    /*
     * update the frame length
     */
    if (xfer->actual_frames != xfer->num_frames) {
        xfer->frame_lengths[xfer->actual_frames] = 0;
    }

    uint16_t actual_length;
    uint8_t status;
    while (1) {
        actual_length = qtd->qtd_token.bytes;
        status = qtd->qtd_token.status;

        if (actual_length > qtd->len) {
            USB_DEBUG("WARNING: Invalid status length. Halting EP\n");
            status |= USB_EHCI_QTD_STATUS_HALTED;
        } else {
            xfer->frame_lengths[xfer->actual_frames] += qtd->len
                    - actual_length;
            usb_ehci_update_dt(xfer, qtd->len - actual_length, qtd->len);
        }

        /*
         * last transfer
         *  - the current qtd equal to the last td pointer of the xfer
         *  - set the qtd to NULL and stop processing
         */
        if (((void *) qtd) == xfer->hcd_td_last) {
            qtd = NULL;
            break;
        }

        /*
         * check for error conditions, i.e. the endpoint is halted
         *  - set the qtd to NULL and stop processing
         */
        if (status & USB_EHCI_QTD_STATUS_HALTED) {
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
        while (qtd) {
            USB_DEBUG(
                    "QTD Status: %x, errorcount = %u\n", qtd->qtd_token.status, qtd->qtd_token.err_count);
            qtd = qtd->obj_next;
        }
        USB_DEBUG("NOTICE: status = %x\n", status);
        USB_DEBUG(
                "---babble detected = %u\n", (status & USB_EHCI_QTD_STATUS_BABBLE) > 0);
        USB_DEBUG(
                "---transfer error = %u\n", (status & USB_EHCI_QTD_STATUS_TRANS_ERR) > 0);
        USB_DEBUG(
                "---buffer error = %u\n", (status & USB_EHCI_QTD_STATUS_DATA_ERR) > 0);
        USB_DEBUG(
                "---missed micro frame= %u\n", (status & USB_EHCI_QTD_STATUS_MISS) > 0);

        USB_DEBUG_TR_RETURN;

        return (USB_ERR_STALLED);
    } else {

        USB_DEBUG_TR_RETURN;
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
    USB_DEBUG_TR_ENTER;
    usb_error_t err = USB_ERR_OK;

    /*
     * iterate over all queue transfer descriptors
     */
    xfer->hcd_td_cache = xfer->hcd_td_first;

    /* CASE 1: control transfers */
    if (xfer->flags_internal.ctrl_xfer) {

        if (xfer->flags_internal.ctrl_header) {
            /* the header was not sent */
            USB_DEBUG_XFER_HC("usb_ehci_xfer_done: xfer->flags_internal.ctrl_header\n");
            err = usb_ehci_xfer_done_process_frames(xfer);
        }
        /* control transfers have one frame max per transfer */
        xfer->actual_frames = 1;

        if (xfer->hcd_td_cache == NULL) {
            /* remove the xfer */
            USB_DEBUG_XFER_HC("usb_ehci_xfer_done: xfer->hcd_td_cache == NULL\n");
            usb_ehci_xfer_remove(xfer, err);
            return;
        }
    }
    /* loop over the xfer till we get actual frames == num frames */
    while (xfer->actual_frames < xfer->num_frames) {
        err = usb_ehci_xfer_done_process_frames(xfer);

        xfer->actual_frames++;

        if (xfer->hcd_td_cache == NULL) {
            USB_DEBUG_XFER_HC("usb_ehci_xfer_done: xfer->hcd_td_cache == NULL loop\n");
            usb_ehci_xfer_remove(xfer, err);
            return;
        }
    }

    /* the control transfer has not started yet, remove it */
    if (xfer->flags_internal.ctrl_xfer && !xfer->flags_internal.ctrl_active) {
        err = usb_ehci_xfer_done_process_frames(xfer);
    }

    usb_ehci_xfer_remove(xfer, err);

    USB_DEBUG_TR_RETURN;
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
    USB_DEBUG_TR_ENTER;

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

    uint8_t status = qh->qh_status.status;

    /*
     * check if the status is still pending
     */
    if (status & USB_EHCI_QTD_STATUS_ACTIVE) {
        USB_DEBUG_XFER_HC("NOTICE: transfer still active..\n");
        USB_DEBUG_TR_RETURN;
        return (0);
    }

    /*
     * check if there is still an active qtd in this qh
     * this indicats that the transfer is not done
     */
    while (1) {
        status = qtd->qtd_token.status;

        if (status & USB_EHCI_QTD_STATUS_ACTIVE) {
            xfer->hcd_td_cache = qtd;
            USB_DEBUG_XFER_HC("NOTICE: transfer still active..\n");
            USB_DEBUG_TR_RETURN;
            return (0);
        }

        /*
         * the last transfer descriptor is not active, this makes
         * the whole transfer done
         */
        if (((void *) qtd) == xfer->hcd_td_last) {
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
        if (qtd->qtd_token.bytes) {
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
    USB_DEBUG_XFER_HC("NOTICE: transfer done..\n");

    xfer->flags_internal.done = 1;
    xfer->flags_internal.transferring = 0;

    usb_ehci_xfer_done(xfer);

    USB_DEBUG_TR_RETURN;
    return (1);
}

