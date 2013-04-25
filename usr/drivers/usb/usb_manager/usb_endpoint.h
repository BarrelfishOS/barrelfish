/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_ENDPOINT_H_
#define _USB_ENDPOINT_H_

#include "usb_controller.h"
#include "usb_xfer.h"

/*
 * ------------------------------------------------------------------------
 * USB Endpoint
 * ------------------------------------------------------------------------
 * This data structure defines an USB tendpoint reflecting the state on a
 * physical endpoint on the device.
 *
 * Fields:
 *  - transfers         queue of usb_xfers of this endpoint
 *  - descriptor        the endpoint descriptor
 *  - pipe_fn           pointer to the pipe functions (set by HC driver)
 *  - isoc_next         the next isochronus transfer
 *  - toggle_next       the next data toggle value
 *  - is_stalled        set if the endpoint is stalled
 *  - is_sync           is set if the data structure is in sync
 *  - unused            unused
 *  - iface             pointer to the interface
 *  - iface_index       the index of the interface
 *  - endpoint_address  the address of this endpoint
 *  - ref_allocation    reference count allocation
 *  - ref_bandwidth     reference count bandwidth
 *  - hs_start          high speed resource allocation start mask
 *  - hs_complete       high speed resource allocation complete mask
 *  - hs_uframe;        high speed micro frame
 *  - max_packet_size   the maximum packet size to be used for this endponit
 *  - status            the status uf this endpoint
 */
struct usb_endpoint
{
    struct usb_xfer_queue transfers;

    struct usb_endpoint_descriptor *descriptor;

    struct usb_hcdi_pipe_fn *pipe_fn;

    uint16_t isoc_next;

    uint8_t data_toggle : 1;
    uint8_t is_stalled : 1;
    uint8_t is_sync : 1;
    uint8_t unused : 5;

    struct usb_interface *iface;
    uint8_t iface_index;
    uint8_t endpoint_address;

    uint8_t ref_allocation;
    uint8_t ref_bandwidth;
    uint8_t hs_start;
    uint8_t hs_complete;
    uint8_t hs_uframe;

    uint16_t max_packet_size;

    uint32_t status;
};

// endpoint status flag for usb_status_t
#define USB_ENDPOINT_STATUS_HALT 0x0001

// the USB control endpoint
#define USB_ENDPOINT_CONTROL

// the maximum number of endpoints
#define USB_ENDPOINT_MAX 32
#endif
