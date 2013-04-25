/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "usb_ohci_descriptors.h"
#include "usb_ohci_xfer.h"
#include "../usb_endpoint.h"


static void usb_ohci_xfer_short_frames(struct usb_xfer *xfer)
{
    usb_ohci_td_t *td;
    usb_ohci_ed_t *ed;

    usb_ohci_td_ctrl_t *td_ctrl;
    uint16_t cc;
    usb_paddr_t td_next;
    usb_paddr_t current_buffer;

    td = xfer->hcd_td_cache;

    /*
     * loop over the frame, a frame may contain more than one short
     * packets, we have to make sure that we reached the last one
     */
    while(1) {
        /* TODO: invalidate chache ? */
        current_buffer = td->td_current_buffer;
        td_ctrl = &td->td_control;
        td_next = td->td_nextTD;

        /*
         * check if we have reached the last transfer descriptor
         * if so we are done
         */

        if (((void *) td) == xfer->hcd_td_last) {
            td = NULL;
            break;
        }

        /*
         * check the condition codes, if it is USB_OHCI_STATUS_OK then
         * the transfer is finished
         */
        cc = td_ctrl->condition_code;
        if (cc) {
            td = NULL;
            break;
        }

        /*
         * check if we have reached the last packet i.e. the td_nextTD is
         * NULL, but hwe have to mask out the last four bits, since these may
         * be used otherwise.
         * If we have a current buffer then there is something else in the
         * frame we follow the alternative and stop processing.
         */
        if (((td_next & (~0xF)) == 0) || current_buffer) {
            td = td->alt_next;
            break;
        }

        // go to next transfer descriptor
        td = td->obj_next;
    }

    // update of the cache
    xfer->hcd_td_cache = td;

    /*
     * we have found a non completed short transfer for this endpoint
     * this means we have to update the head pointer of the endpoint
     * descriptor to this one
     */
    if (td) {
        // get the associated endpoint
        ed = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];

        ed->ed_headP = td->td_self;

        // TODO: invalideate cache?

        /*
         * we need to make sure that the OHCI takes up this remaining
         * transfer descriptor for processing.
         */
        if (xfer->type == USB_XFER_TYPE_BULK) {
            /* TODO: write register BLF
             * OWRITE4(sc, OHCI_COMMAND_STATUS, OHCI_BLF);
             */
        }

        if (xfer->type == USB_XFER_TYPE_CTRL) {
            /* TODO: write register CLF
             * OWRITE4(sc, OHCI_COMMAND_STATUS, OHCI_CLF);
             */
        }
    }
}

/**
 * \brief   this function checks if a USB transfer is already finished or not
 *
 * \param   xfer    the usb transfer to check for completition
 *
 * \return  0       if the usb transfer is not finished
 *          else    if the usb transfer is finished
 */
uint8_t usb_ohci_xfer_is_finished(struct usb_xfer *xfer)
{
    usb_ohci_ed_t *ed;
    usb_paddr_t ed_headP;
    usb_paddr_t ed_tailP;

    // getting the endpoint from the queue head list
    ed = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];

    /* TODO: invalidate cache ? */

    // get the transfer descriptor pointers
    ed_headP = ed->ed_headP;
    ed_tailP = ed->ed_tailP;


    /*
     *  if the endpoint is halted or there are no transfer descriptors
     *  then there is no activitiy
     */
    if (USB_OHCI_EP_HALTED(ed) || !USB_OHCI_EP_HAS_TD(ed_headP, ed_tailP)) {
        if (xfer->type == USB_XFER_TYPE_ISOC) {
            // isochronus endponts have to be treated differently;
            usb_ohci_xfer_done_isoc(xfer);

            // transfer completed
            return 1;
        }

        if (xfer->flags_internal.short_frames_ok) {
            usb_ohci_xfer_short_frames(xfer);

            if (xfer->hcd_td_cache) {
                return 0;
            }
        }

        // handle the data toggle flag
        if (USB_OHCI_EP_CARRY(ed)) {
            xfer->endpoint->data_toggle = 1;
        } else {
            xfer->endpoint->data_toggle = 0;
        }

        // handle the completition of the xfer
        usb_ohci_xfer_done(xfer);

        // transfers completed
        return 1;
    }

    // transfer is not completed yet
    return 0;
}

/**
 * \brief This function updates the frame_lengths of the usb transfer
 *
 * \param xfer  the current USB transfer
 *
 * \return USB_ERR_OK on success
 *         USB_ERR_IO
 *         USB_ERR_STALLED
 */
static usb_error_t
usb_ohci_xfer_update_frame_lengths(struct usb_xfer *xfer)
{
    usb_ohci_td_t *td;
    usb_ohci_td_t *td_alt_next;
    uint32_t temp;
    usb_paddr_t phy_start;
    usb_paddr_t phy_end;
    usb_ohci_td_ctrl_t td_flags;
    uint16_t cc;


    td = xfer->hcd_td_cache;
    td_alt_next = td->alt_next;

    td_flags = 0;

    if (xfer->actual_frames != xfer->num_frames) {
        if (xfer->actual_frames < xfer->max_frame_count) {
            xfer->frame_lengths[xfer->actual_frames] = 0;
        } else{
            // TODO: error handling if actual frame is bigger than max frames
            assert(!"Frame overflow");
        }
    }

    while (1) {
        phy_start = td->td_current_buffer;
        td_flags = td->td_control;
        cc = td_flags.condition_code;

        if (phy_start) {
            /*
             * the current buffer pointer is non zero, this means we
             * have some data in the buffer, but buffer is not full.
             * i.e. we have to deal with short transfers
             */
            phy_end = td->td_buffer_end;

            /*
             * calculate the remaining bytes in the buffer.
             * Basically end-start + 1
             */
            temp = (USB_OHCI_PAGE(phy_start ^ phy_end) ?
                    (USB_OHCI_PAGE_SIZE + 1) : 0x0001);
            temp += USB_OHCI_PAGE_OFFSET(phy_end);
            temp -= USB_OHCI_PAGE_OFFSET(phy_start);

            /*
             * we may have more data in the buffer left as the transfer
             * size indicates. That case we stall the transfer.
             */
            if (temp > td->len) {
                cc = USB_OHCI_STATUS_STALL;
            } else if (xfer->actual_frames != xfer->num_frames) {
                /*
                 * set the frame_length of the actual frame
                 */
                xfer->frame_lengths[xfer->actual_frames] += td->len - temp;
            }
        } else {
            /*
             * the transfer was complete one frame before, so we
             * set the frame length of the actual frame to be the
             * total transfer length
             */
            if (xfer->actual_frames != xfer->num_frames) {
                xfer->frame_lengths[xfer->actual_frames] += td->len;
            }
        }

        /*
         * if this TD was the last of USB the transfer, we are done and
         * can go out of the loop (tranfer finished).
         */
        if (((void *) td) == xfer->hcd_td_last) {
            td = NULL;
            break;
        }

        /*
         * if we have a condition flag other than USB_OHCI_STATUS_OK then
         * we treat this as completed and go out of the loop.
         * (transfer finished).
         */
        if (cc) {
            td = NULL;
            break;
        }

        /*
         * we encountered a short transfer, we have to check now if
         * short transfers are acceptable or not by checking the
         * short_frames_ok flag. If this is the case, we follow the
         * alternative next pointer to the TD filling up the remainder of
         * this frame. If short frames are not acceptable this means
         * we have reached the end of the transfer
         */
        if (phy_start) {
            if (xfer->flags_internal.short_frames_ok) {
                td = td->alt_next;
            } else {
                td = NULL;
            }
            break;
        }

        // take the next TD in the list
        td = td->obj_next;

        /*
         * we have a new alternative next pointer this belongs
         * to another transfer thus this transfer is complete
         */
        if (td->alt_next != td_alt_next) {
            break;
        }
    }

    /* update transfer cache */

    xfer->hcd_td_cache = td;

    return ((cc == USB_OHCI_STATUS_OK) ? USB_ERR_OK :
            (cc == USB_OHCI_STATUS_STALL) ? USB_ERR_STALLED : USB_ERR_IOERROR);
}


/**
 * \brief   this function handles the completion of a transfer
 *
 * \param   xfer    the usb transfer to handle completion
 */
void usb_ohci_xfer_done(struct usb_xfer *xfer)
{
    usb_error_t err = USB_ERR_OK;

    /*
     * go over the td list and handle the competition
     */
    xfer->hcd_td_cache = xfer->hcd_td_first;

    /*
     * check if it is a control transfer. If this is the case
     * we may need to do some extra work, because control headers
     * could be requested
     */
    if (xfer->flags_internal.ctrl_xfer) {
        if (xfer->flags_internal.ctrl_header) {
            err = usb_ohci_xfer_update_frame_lengths(xfer);
        }
        xfer->actual_frames = 1;

        if (xfer->hcd_td_cache == NULL) {
            usb_ohci_xfer_remove(xfer, err);
            return;
        }
    }

    /*
     * process the remaining frames till all is complete
     */
    while (xfer->actual_frames < xfer->num_frames) {
        err = usb_ohci_xfer_update_frame_lengths(xfer);
        xfer->actual_frames++;

        if (xfer->hcd_td_cache == NULL) {
            usb_ohci_xfer_remove(xfer, err);
            return;
        }
    }

    /*
     * check if there is a control transfer active at the moment
     */
    if (xfer->flags_internal.ctrl_xfer && !xfer->flags_internal.ctrl_active) {
        err = usb_ohci_xfer_update_frame_lengths(xfer);
    }

    usb_ohci_xfer_remove(xfer, err);

}


/**
 * \brief
 *
 * \param   xfer    the usb transfer to check for completion
 *
 * \return  0       if the usb transfer is not finished
 *          else    if the usb transfer is finished
 */
void usb_ohci_xfer_done_isoc(struct usb_xfer *xfer)
{
   assert(!"NYI: need to check isochronus transfer competition");
}

/**
 * \brief This function is called by the specific pipe functions when a
 *        usb transfer is finished. The transfer descriptors are removed
 *        from the endpoint lists.
 *
 * \param xfer  the usb transfer request that is finished
 * \param error status code of the finished transfer request
 */
void usb_ohci_xfer_remove(struct usb_xfer *xfer, usb_error_t error)
{
    usb_ohci_ed_t *ed;

    // get the host controller
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) xfer->host_controller->hc_control;

    // get the endpoint associated with the usb transfer
    ed = xfer->hcd_qh_start[xfer->flags_internal.curr_dma_set];

    // todo: invalidate page cache of endpoint

    switch (xfer->type) {
        case USB_XFER_TYPE_ISOC:
            usb_ohci_remove_qh(ed, hc->qh_isoc_last);
            break;
        case USB_XFER_TYPE_INTR:
            usb_ohci_remove_qh(ed, hc->qh_intr_last[xfer->intr_qh_pos]);
            break;
        case USB_XFER_TYPE_CTRL:
            usb_ohci_remove_qh(ed, hc->qh_ctrl_last);
            break;
        case USB_XFER_TYPE_BULK:
            usb_ohci_remove_qh(ed, hc->qh_bulk_last);
            break;
        default:
            assert(!"Invalid Transfer Type");
            break;
    }

    // set the transfer descriptor pointers to null
    xfer->hcd_td_first = NULL;
    xfer->hcd_td_last = NULL;

    /*
     * remove from interrupt queue and enqueue to done queue
     */
    usb_xfer_done(xfer, error);
}


/*
 * \brief enqueues the transfer on the controller's interrupt queue to
 *        handle the completed transfers
 *
 * \param xfer  the transfer to enqueue
 */
void usb_ohci_xfer_enqueue(struct usb_xfer *xfer)
{
    /* check for early completion */
    if (usb_ohci_xfer_is_finished(xfer)) {
        return;
    }
    /* put transfer on interrupt queue */
    usb_xfer_enqueue(xfer->host_controller->intr_queue, xfer);

    /* start timeout, if any */
    /* TODO: handle time out
    if (xfer->timeout != 0) {
        usbd_transfer_timeout_ms(xfer, &ohci_timeout, xfer->timeout);
    }*/
}

struct usb_ohci_setup_td
{
    struct usb_page_cache *pc;
    usb_ohci_td_t *td;
    usb_ohci_td_t *td_next;
    uint32_t average;
    usb_ohci_td_ctrl_t td_flags;
    uint32_t len;
    uint16_t max_frame_size;
    uint8_t shortpkt;
    uint8_t setup_alt_next;
    uint8_t last_frame;
};


static void
usb_ohci_xfer_setup_td(struct usb_ohci_setup_td *temp)
{
    struct usb_page_search buf_res;
    usb_ohci_td_t *td;
    usb_ohci_td_t *td_next;
    usb_ohci_td_t *td_alt_next;
    uint32_t buf_offset;
    uint32_t average;
    uint32_t len_old;
    uint8_t shortpkt_old;
    uint8_t precompute;

    td_alt_next = NULL;
    buf_offset = 0;
    shortpkt_old = temp->shortpkt;
    len_old = temp->len;
    precompute = 1;

    /* software is used to detect short incoming transfers */

    if ((temp->td_flags & htole32(OHCI_TD_DP_MASK)) == htole32(OHCI_TD_IN)) {
        temp->td_flags |= htole32(OHCI_TD_R);
    } else {
        temp->td_flags &= ~htole32(OHCI_TD_R);
    }

    restart:

    td = temp->td;
    td_next = temp->td_next;

    while (1) {

        if (temp->len == 0) {

            if (temp->shortpkt) {
                break;
            }
            /* send a Zero Length Packet, ZLP, last */

            temp->shortpkt = 1;
            average = 0;

        } else {

            average = temp->average;

            if (temp->len < average) {
                if (temp->len % temp->max_frame_size) {
                    temp->shortpkt = 1;
                }
                average = temp->len;
            }
        }

        if (td_next == NULL) {
            panic("%s: out of OHCI transfer descriptors!", __FUNCTION__);
        }
        /* get next TD */

        td = td_next;
        td_next = td->obj_next;

        /* check if we are pre-computing */

        if (precompute) {

            /* update remaining length */

            temp->len -= average;

            continue;
        }
        /* fill out current TD */
        td->td_flags = temp->td_flags;

        /* the next TD uses TOGGLE_CARRY */
        temp->td_flags &= ~htole32(OHCI_TD_TOGGLE_MASK);

        if (average == 0) {
            /*
             * The buffer start and end phys addresses should be
             * 0x0 for a zero length packet.
             */
            td->td_cbp = 0;
            td->td_be = 0;
            td->len = 0;

        } else {

            usbd_get_page(temp->pc, buf_offset, &buf_res);
            td->td_cbp = htole32(buf_res.physaddr);
            buf_offset += (average - 1);

            usbd_get_page(temp->pc, buf_offset, &buf_res);
            td->td_be = htole32(buf_res.physaddr);
            buf_offset++;

            td->len = average;

            /* update remaining length */

            temp->len -= average;
        }

        if ((td_next == td_alt_next) && temp->setup_alt_next) {
            /* we need to receive these frames one by one ! */
            td->td_flags &= htole32(~OHCI_TD_INTR_MASK);
            td->td_flags |= htole32(OHCI_TD_SET_DI(1));
            td->td_next = htole32(OHCI_TD_NEXT_END);
        } else {
            if (td_next) {
                /* link the current TD with the next one */
                td->td_next = td_next->td_self;
            }
        }

        td->alt_next = td_alt_next;

        usb_pc_cpu_flush(td->page_cache);
    }

    if (precompute) {
        precompute = 0;

        /* setup alt next pointer, if any */
        if (temp->last_frame) {
            /* no alternate next */
            td_alt_next = NULL;
        } else {
            /* we use this field internally */
            td_alt_next = td_next;
        }

        /* restore */
        temp->shortpkt = shortpkt_old;
        temp->len = len_old;
        goto restart;
    }
    temp->td = td;
    temp->td_next = td_next;
}





void
usb_ohci_xfer_setup(struct usb_xfer *xfer, usb_ohci_ed_t **ed_last)
{
    struct usb_ohci_setup_td temp;
    struct usb_hcdi_pipe_fn *pipe_fn;
    usb_ohci_ed_t *ed;
    usb_ohci_td_t *td;
    uint32_t ed_flags;
    uint32_t x;


    temp.average = xfer->max_frame_size;
    temp.max_frame_size = xfer->max_frame_size;

    /* toggle the DMA set we are using */
    xfer->flags_internal.curr_dma_set ^= 1;

    /* get next DMA set */
    td = xfer->hcd_td_start[xfer->flags_internal.curr_dma_set];

    xfer->hcd_td_first = td;
    xfer->hcd_td_cache = td;

    temp.td = NULL;
    temp.td_next = td;
    temp.last_frame = 0;
    temp.setup_alt_next = xfer->flags_internal.short_frames_ok;

    pipe_fn = xfer->endpoint->pipe_fn;



    /*
     * Control transfer may need a setup packet for certain requests
     * so check if we need such a setup packet and generate one in the
     * very first transfer descriptor
     */
    if (xfer->flags_internal.ctrl_xfer) {
        if (xfer->flags_internal.ctrl_header) {
            memset(&temp.td_flags, 0, sizeof(temp.td_flags))
            temp.td_flags.data_toggle = 0;
            temp.td_flags.direction_pid = USB_OHCI_PID_SETUP;
            temp.td_flags.delay_interrupt = USB_OHCI_TD_DISABLE_IRQ;
            temp.td_flags.condition_code = 0;

            temp.td_flags = htole32(OHCI_TD_SETUP | OHCI_TD_NOCC |
                OHCI_TD_TOGGLE_0 | OHCI_TD_NOINTR);

            temp.len = xfer->frame_lengths[0];
            temp.pc = xfer->frame_buffers + 0;
            temp.shortpkt = temp.len ? 1 : 0;
            /* check for last frame */
            if (xfer->num_frames == 1) {
                /* no STATUS stage yet, SETUP is last */
                if (xfer->flags_int.control_act) {
                    temp.last_frame = 1;
                    temp.setup_alt_next = 0;
                }
            }
            ohci_setup_standard_chain_sub(&temp);

            /*
             * XXX assume that the setup message is
             * contained within one USB packet:
             */
            xfer->endpoint->toggle_next = 1;
        }
        x = 1;
    } else {
        x = 0;
    }
    temp.td_flags = htole32(OHCI_TD_NOCC | OHCI_TD_NOINTR);

    /* set data toggle */

    if (xfer->endpoint->toggle_next) {
        temp.td_flags |= htole32(OHCI_TD_TOGGLE_1);
    } else {
        temp.td_flags |= htole32(OHCI_TD_TOGGLE_0);
    }

    /* set endpoint direction */

    if (UE_GET_DIR(xfer->endpointno) == UE_DIR_IN) {
        temp.td_flags |= htole32(OHCI_TD_IN);
    } else {
        temp.td_flags |= htole32(OHCI_TD_OUT);
    }

    while (x != xfer->nframes) {

        /* DATA0 / DATA1 message */

        temp.len = xfer->frlengths[x];
        temp.pc = xfer->frbuffers + x;

        x++;

        if (x == xfer->nframes) {
            if (xfer->flags_int.control_xfr) {
                /* no STATUS stage yet, DATA is last */
                if (xfer->flags_int.control_act) {
                    temp.last_frame = 1;
                    temp.setup_alt_next = 0;
                }
            } else {
                temp.last_frame = 1;
                temp.setup_alt_next = 0;
            }
        }
        if (temp.len == 0) {

            /* make sure that we send an USB packet */

            temp.shortpkt = 0;

        } else {

            /* regular data transfer */

            temp.shortpkt = (xfer->flags.force_short_xfer) ? 0 : 1;
        }

        ohci_setup_standard_chain_sub(&temp);
    }

    /* check if we should append a status stage */

    if (xfer->flags_int.control_xfr &&
        !xfer->flags_int.control_act) {

        /*
         * Send a DATA1 message and invert the current endpoint
         * direction.
         */

        /* set endpoint direction and data toggle */

        if (UE_GET_DIR(xfer->endpointno) == UE_DIR_IN) {
            temp.td_flags = htole32(OHCI_TD_OUT |
                OHCI_TD_NOCC | OHCI_TD_TOGGLE_1 | OHCI_TD_SET_DI(1));
        } else {
            temp.td_flags = htole32(OHCI_TD_IN |
                OHCI_TD_NOCC | OHCI_TD_TOGGLE_1 | OHCI_TD_SET_DI(1));
        }

        temp.len = 0;
        temp.pc = NULL;
        temp.shortpkt = 0;
        temp.last_frame = 1;
        temp.setup_alt_next = 0;

        ohci_setup_standard_chain_sub(&temp);
    }
    td = temp.td;

    /* Ensure that last TD is terminating: */
    td->td_next = htole32(OHCI_TD_NEXT_END);
    td->td_flags &= ~htole32(OHCI_TD_INTR_MASK);
    td->td_flags |= htole32(OHCI_TD_SET_DI(1));

    usb_pc_cpu_flush(td->page_cache);

    /* must have at least one frame! */

    xfer->td_transfer_last = td;

#ifdef USB_DEBUG
    if (ohcidebug > 8) {
        DPRINTF("nexttog=%d; data before transfer:\n",
            xfer->endpoint->toggle_next);
        ohci_dump_tds(xfer->td_transfer_first);
    }
#endif

    ed = xfer->qh_start[xfer->flags_int.curr_dma_set];

    ed_flags = (OHCI_ED_SET_FA(xfer->address) |
        OHCI_ED_SET_EN(UE_GET_ADDR(xfer->endpointno)) |
        OHCI_ED_SET_MAXP(xfer->max_frame_size));

    ed_flags |= (OHCI_ED_FORMAT_GEN | OHCI_ED_DIR_TD);

    if (xfer->xroot->udev->speed == USB_SPEED_LOW) {
        ed_flags |= OHCI_ED_SPEED;
    }
    ed->ed_flags = htole32(ed_flags);

    td = xfer->td_transfer_first;

    ed->ed_headp = td->td_self;

    if (xfer->xroot->udev->flags.self_suspended == 0) {
        /* the append function will flush the endpoint descriptor */
        OHCI_APPEND_QH(ed, *ed_last);

        if (methods == &ohci_device_bulk_methods) {
            ohci_softc_t *sc = OHCI_BUS2SC(xfer->xroot->bus);

            OWRITE4(sc, OHCI_COMMAND_STATUS, OHCI_BLF);
        }
        if (methods == &ohci_device_ctrl_methods) {
            ohci_softc_t *sc = OHCI_BUS2SC(xfer->xroot->bus);

            OWRITE4(sc, OHCI_COMMAND_STATUS, OHCI_CLF);
        }
    } else {
        usb_pc_cpu_flush(ed->page_cache);
    }
}


