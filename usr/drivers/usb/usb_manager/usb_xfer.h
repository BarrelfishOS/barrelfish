/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_XFER_H_
#define _USB_XFER_H_

#include "usb_controller.h"

enum usb_xfer_type {
    USB_XFER_TYPE_ISOC = 0,
    USB_XFER_TYPE_INTR,
    USB_XFER_TYPE_CTRL,
    USB_XFER_TYPE_BULK
};

/*
 * ------------------------------------------------------------------------
 * USB Transfer Flags (Internal)
 * ------------------------------------------------------------------------
 * This data structure represents a set of flags that are used internally
 * for keeping track of the transfer state
 *
 * Fields:
 *  - usb_mode              the host controller mode
 *  - remaining_bytes       remaining bytes of the transfer
 *  - pipe_open             the pipe used for the transfer is opened
 *  - transferring          transfer is currently in progress
 *  - dma_wait              indicates that we are waiting for HW DMA
 *  - transfer_closed       indicates that we have closed the transfer
 *  - draining              indicates that we are draining the transfer
 *  - started               transfer is started / stopped
 *  - bandwidth_reclaimed   set if the bandwidth has been reclaimed
 *  - ctrl_xfer             set if this transfer is a control transfer
 *  - ctrl_header           set if the ctrl header needs to be sent
 *  - ctrl_active           set if the ctrl transfer is active
 *  - ctrl_stall            set if the ctrl transfer should be stalled
 *  - short_frames_ok       allows non full frames
 *  - short_transfer_ok     allows non full transfers
 *  - isoc_xfer             set if this transfer is isochronus
 *  - curr_dma_set          is used by the host controller driver
 *  - cancellable           set if this transfer can immediately be cancelled
 *  - notify                set if the device driver is being notified
 */
struct usb_xfer_flags_internal {
    enum usb_controller_mode usb_mode;

    uint16_t remaining_bytes;

    uint8_t pipe_open :1;
    uint8_t transferring :1;

    uint8_t dma_wait :1;
    uint8_t transfer_closed :1;

    uint8_t draining :1;
    uint8_t started :1;

    uint8_t bandwidth_reclaimed :1;

    uint8_t ctrl_xfer :1;
    uint8_t ctrl_header :1;
    uint8_t ctrl_active :1;
    uint8_t ctrl_stall;

    uint8_t short_frames_ok :1;
    uint8_t short_transfer_ok :1;

    /* TODO: BUS DMA? */

    uint8_t isoc_xfer :1;
    uint8_t curr_dma_set :1;
    uint8_t cancellable :1;
    uint8_t notify :1;
};

/*
 * ------------------------------------------------------------------------
 * USB Transfer Flags
 * ------------------------------------------------------------------------
 * This data structure represents a set of flags that are used internally
 * for keeping track of the transfer state
 *
 * Fields:
 *  - short_xfer_forced     force a short transmit transfer on last packet
 *  - short_xfer_ok         allow short transfers (small packets)
 *  - short_frames_ok       allow short frames
 *  - pipe_on_falure        block pipe had a failure condition
 *  - buf_size_frame        buffer size shall be multiple of frame size
 *  - ext_buffer            use external DMA buffer
 *  - manual_status         disables automatic status stage on ctrl transfers
 *  - pipe_none_ok          ingore USB_ERR_NO_PIPE errors
 *  - pipe_stalled          stall the endpoint before starting the transfer
 *  - prescale              prescale to frames for isochr transfers
 */
struct usb_xfer_flags {
    uint8_t short_xfer_forced :1;
    uint8_t short_xfer_ok :1;
    uint8_t short_frames_ok :1;
    uint8_t pipe_on_falure :1;
    uint8_t buf_size_frame :1;
    uint8_t ext_buffer :1;
    uint8_t manual_status :1;
    uint8_t pipe_none_ok :1;
    uint8_t pipe_stalled :1;
    uint8_t prescale :1;
};

/*
 * ------------------------------------------------------------------------
 * USB Transfer Queue
 * ------------------------------------------------------------------------
 * This data structure is a commonly used structure for transfer queues
 *
 * Fields:
 *  - head:     struct containing pointers to the list elements
 *      - first:    the head of the queue (first element)
 *      - last:     the address of last next element
 *  - current:  the current USB transfer being processed
 *  - command:  a function pointer to a command to execute
 *  - recurse:
 */
struct usb_xfer_queue {
    struct {
        struct usb_xfer *first;
        struct usb_xfer **last_next;
    } head;
    struct usb_xfer *current;
    void
    (*command)(struct usb_xfer_queue *pq);
    uint8_t recurse_1 :1;
    uint8_t recurse_2 :1;
};

/*
 * ------------------------------------------------------------------------
 * USB Transfer
 * ------------------------------------------------------------------------
 * This data structure defines an USB transfer
 *
 * Fields:
 *  -
 */
struct usb_xfer {
    uint32_t device_driver_binding;  // flounder ref
    struct {
        struct usb_xfer *next; /* next element */
        struct usb_xfer **prev_next; /* address of previous next element */
    } wait_entry;

    struct usb_xfer_queue *wait_queue;

    enum usb_xfer_type type;
    uint16_t intr_qh_pos;            // position in the queue head intr list

    struct usb_endpoint *endpoint;
    void *hcd_qh_start[2];
    void *hcd_td_start[2];
    void *hcd_td_first;  // first td in the list
    void *hcd_td_last;  // last td in the list
    void *hcd_td_cache;  // a field used to swap the tds

    uint32_t max_data_length;
    uint32_t sum_bytes;
    uint32_t actual_bytes;

    uint32_t num_frames;
    uint32_t actual_frames;
    uint32_t *frame_lengths;    // array of frame lengths
    uint8_t frame_shift;
    struct usb_dma_page *frame_buffers;  // TODO: not really needed?
    struct usb_dma_page dma_page;

    uint32_t max_packet_count;
    uint16_t max_packet_size;
    uint16_t max_frame_size;    // maximum size of a frame
    uint16_t max_hc_frame_size;
    uint32_t max_data_length; max_hc_frame_size;
    uint32_t max_frame_count;   // size of array frame_length

    struct usb_host_controller *host_controller;
    struct usb_device *device;
    uint8_t device_address;
    uint8_t endpoint_number;
    uint8_t usb_state;
    uint8_t ed_direction;

    uint16_t isoc_time_complete;
    uint16_t interval;          // for isoc and intr
    uint16_t timeout;
    usb_error_t error;

    struct usb_xfer_flags flags;
    struct usb_xfer_flags_internal flags_internal;

};

/*
 * ------------------------------------------------------------------------
 * USB Transfer Setup Parameters
 * ------------------------------------------------------------------------
 * This data structure contains all the relevant information needed to
 * setup a new USB transfer
 *
 * Fields:
 *  -
 */
struct usb_xfer_setup_params {
    struct usb_device *device;
    struct usb_xfer *curr_xfer;
    const struct usb_xfer_config *xfer_setup;
    const struct usb_hcdi_pipe_fn *pipe_fn;
    enum usb_xfer_type type;
    void *buf;
    uint32_t *xfer_length_ptr;

    uint32_t size[7];
    uint32_t bufsize;
    uint32_t bufsize_max;
    struct usb_dma_page dma_page;

    uint32_t hc_max_frame_size;
    uint16_t hc_max_packet_size;
    uint8_t hc_max_packet_count;
    enum usb_dev_speed speed;
    usb_error_t err;
};

/**
 * ------------------------------------------------------------------------
 * USB Transfer Configuration
 * ------------------------------------------------------------------------
 * This data structure contains an USB configuration used for setting up
 * new USB transfers.
 *
 * Fields:
 *  - bufsize   total pipe buffer size in bytes
 *  - frames    maximum number of USB frames to be used
 *  - interval  interval in milliseconds
 *  - timeout   transfer timeout in milliseconds
 *  - flags     transfer flags
 *  - usb_mode  host or device mode
 *  - type      transfer type
 *  - endpoint  endpoint number to be used
 *  - direction the direction of the transfer
 *  - ep_index  the endpoint index to be used
 *  - if_index  the interface index to be used
 */
struct usb_xfer_config {
    uint32_t bufsize;
    uint32_t frames;
    uint32_t interval;
    uint32_t timeout;
    struct usb_xfer_flags flags;
    enum usb_controller_mode usb_mode;
    uint8_t type;
    uint8_t endpoint;
    uint8_t direction;
    uint8_t ep_index;
    uint8_t if_index;
};

void usb_xfer_enqueue(struct usb_xfer_queue *queue, struct usb_xfer *xfer);
void usb_xfer_dequeue(struct usb_xfer *xfer);
void usb_xfer_done(struct usb_xfer *xfer, usb_error_t err);
void usb_xfer_setup_struct(struct usb_xfer_setup_params *param);
#endif
