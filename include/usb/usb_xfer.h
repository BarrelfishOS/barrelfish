/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_XFER_H_
#define LIBUSB_XFER_H_

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
    uint8_t _unused : 6;            ///< unused bits to fill up the 2 bytes
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



#endif
