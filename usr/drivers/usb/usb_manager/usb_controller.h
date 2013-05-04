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

#ifndef USB_CONTROLLER_H
#define USB_CONTROLLER_H

#include <usb/usb_xfer.h> /* usb_xfer_done_queue */

/* prototypes */
struct usb_device;
struct usb_device_request;
struct usb_endpoint;
struct usb_xfer_queue;
struct usb_xfer;
struct usb_host_controller;
struct usb_hw_ep_profile;



/*
 * flags representing USB power states
 */
#define USB_HW_POWER_CONTROL    0x01
#define USB_HW_POWER_BULK   0x02
#define USB_HW_POWER_INTERRUPT  0x04
#define USB_HW_POWER_ISOC   0x08
#define USB_HW_POWER_NON_ROOT_HUB 0x10
#define USB_HW_POWER_SUSPEND 0x20
#define USB_HW_POWER_RESUME 0x40
#define USB_HW_POWER_SHUTDOWN 0x60

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





/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Driver Interface for USB Bus
 * ------------------------------------------------------------------------
 * This data structure stores pointers to host controller specific
 * functions that are used to control the USB bus.
 *
 * Fields
 *  - endpoint_init
 *  - xfer_setup
 *  - xfer_unsetup
 *  - get_dma_delay
 *  - device_suspend
 *  - device_resume
 *  - set_hw_power
 *  - set_hw_power_sleep
 *  - get_hw_ep_profile
 *  - set_stall
 *  - clear_stall
 *  - xfer_poll
 *  - get_power_mode
 *  - endpoint_uninit
 *  - device_init
 *  - device_uninit
 *  - start_dma_delay
 *  - device_state_change
 *  - set_address
 */
struct usb_hcdi_bus_fn {
    usb_error_t (*roothub_exec)(struct usb_device *, struct usb_device_request *,
            const void **, uint16_t *);
    void (*endpoint_init)(struct usb_device *, struct usb_endpoint_descriptor *,
            struct usb_endpoint *);
    void (*xfer_setup)(struct usb_xfer_setup_params *parm);
    void (*xfer_unsetup)(struct usb_xfer *);
    void (*get_dma_delay)(struct usb_device *, uint32_t *);
    void (*device_suspend)(struct usb_device *);
    void (*device_resume)(struct usb_device *);
    void (*set_hw_power)(struct usb_host_controller *);
    void (*set_hw_power_sleep)(struct usb_host_controller *, uint32_t);
    void (*xfer_poll)(struct usb_host_controller *hc);
    void (*get_hw_ep_profile)(struct usb_device *udev,
            const struct usb_hw_ep_profile **ppf, uint8_t ep_addr);
    void (*set_stall)(struct usb_device *udev, struct usb_xfer *xfer,
            struct usb_endpoint *ep, uint8_t *did_stall);
    void (*clear_stall)(struct usb_device *udev, struct usb_endpoint *ep);

    void (*get_power_mode)(struct usb_device *udev, int8_t *pmode);
    void (*endpoint_uninit)(struct usb_device *, struct usb_endpoint *);
    usb_error_t (*device_init)(struct usb_device *);
    void (*device_uninit)(struct usb_device *);
    void (*start_dma_delay)(struct usb_xfer *);
    void (*device_state_change)(struct usb_device *);
    usb_error_t (*set_address)(struct usb_device *, uint16_t);
};




typedef struct usb_host_controller {
    usb_hc_version_t hc_type;    // the type of the host controller
    void *hc_control;   // pointer to the host specific controller

    struct usb_hcdi_bus_fn *hcdi_bus_fn;

    enum usb_revision usb_revision;

    struct usb_xfer_queue intr_queue;
    struct usb_xfer_queue done_queue;

    struct usb_device **devices;
    uint8_t devices_max;

    struct usb_device *root_hub;

} usb_host_controller_t;

usb_error_t usb_hc_init(usb_host_controller_t *hc);

#endif /* _USB_CONTROLLER_H_ */
