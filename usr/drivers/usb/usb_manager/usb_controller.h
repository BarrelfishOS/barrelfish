/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * ==========================================================================
 * USB Host Controller Driver Interface
 * ==========================================================================
 */

typedef enum {
    USB_CONTROLLER_TYPE_UHCI,
    USB_CONTROLLER_TYPE_OHCI,
    USB_CONTROLLER_TYPE_EHCI,
    USB_CONTROLLER_TYPE_XHCI
} usb_controller_type_t;


enum usb_controller_mode {
    USB_CONTROLLER_MODE_HOST,      /* initiates transfers */
    USB_CONTROLLER_MODE_HOST,      /* bus transfer target */
    USB_CONTROLLER_MODE_HOST       /* can be host or device */
};
#define USB_MODE_MAX    (USB_MODE_DUAL+1)

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Interface for USB Bus
 * ------------------------------------------------------------------------
 * This data structure stores pointers to host controller specific
 * functions that are used to control the USB bus.
 *
 * Fields:
 *  - open_pipe
 *  - sw_irq
 *  - poll
 *  - mem_alloc
 *  - mem_free
 *  - xfer_alloc
 *  - xfer_free
 */
struct usb_hcdi_bus_fn {

};

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Interface for USB pipes
 * ------------------------------------------------------------------------
 * This data structure contains pointers to host controller specific
 * functions that are used to control an already existing pipe
 *
 * Fields:
 *  - open:     open a pipe
 *  - close:    close a pipe
 *  - enter:    enters
 *  - start:    starts
 *  - info:     get information about the pipe
 */
struct usb_hcdi_pipe_fn {
    void (*open)(struct usb_xfer *);
    void (*close)(struct usb_xfer *);
    void (*enter)(struct usb_xfer *);
    void (*start)(struct usb_xfer *);
    // optional functions
    void *info;
 };




 #define USB_HOST_UHCI 0x10
 #define USB_HOST_OHCI 0x11
 #define USB_HOST_EHCI 0x20
 #define USB_HOST_XHCI 0x30

 typedef struct usb_host_controller {
    usb_controller_type_t hc_type;    // the type of the host controller
    void *hc_control;   // pointer to the host specific controller

    struct usb_xfer_queue intr_queue;
    struct usb_xfer_queue done_queue;

 } usb_host_controller_t;


usb_error_t controller_init();
