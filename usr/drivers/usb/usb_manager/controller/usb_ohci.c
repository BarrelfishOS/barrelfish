/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "usb_ohci.h"
#include "usb_ohci_pipe.h"
#include "usb_ohci_root.h"

#define USB_OHCI_INTERRUPT_ENDPOINT 1


 /*
 * ------------------------------------------------------------------------
 * Function Prototypes
 * ------------------------------------------------------------------------
 */
static void             ohci_do_poll(struct usb_bus *bus);
static void             ohci_device_done(struct usb_xfer *xfer, usb_error_t error);
static void             ohci_timeout(void *arg);
static uint8_t          ohci_check_transfer(struct usb_xfer *xfer);
static void             ohci_root_intr(ohci_softc_t *sc);
static struct ohci_hcca *ohci_get_hcca(ohci_softc_t *sc)
void                    ohci_iterate_hw_softc(struct usb_bus *bus, usb_bus_mem_sub_cb_t *cb)
static usb_error_t      ohci_controller_init(ohci_softc_t *sc, int do_suspend)
static struct ohci_ed   *ohci_init_ed(struct usb_page_cache *pc)
static void
ohci_suspend(ohci_softc_t *sc)
static void
ohci_resume(ohci_softc_t *sc)
static void
ohci_transfer_intr_enqueue(struct usb_xfer *xfer)
static ohci_ed_t *
_ohci_append_qh(ohci_ed_t *sed, ohci_ed_t *last)
static ohci_ed_t *
_ohci_remove_qh(ohci_ed_t *sed, ohci_ed_t *last)
static void
ohci_isoc_done(struct usb_xfer *xfer)
static usb_error_t
ohci_non_isoc_done_sub(struct usb_xfer *xfer)
static void
ohci_non_isoc_done(struct usb_xfer *xfer)
static void
ohci_check_transfer_sub(struct usb_xfer *xfer)
static uint8_t
ohci_check_transfer(struct usb_xfer *xfer)
static void
ohci_rhsc_enable(ohci_softc_t *sc)
static void
ohci_interrupt_poll(ohci_softc_t *sc)
static void
ohci_setup_standard_chain_sub(struct ohci_std_temp *temp)
static void
ohci_setup_standard_chain(struct usb_xfer *xfer, ohci_ed_t **ed_last)

/*
 * ------------------------------------------------------------------------
 * Exported Functions
 * ------------------------------------------------------------------------
 */

usb_error_t ohci_init(ohci_host_controller_t *sc);



void	    ohci_detach(struct ohci_host_controller *sc);




void	    ohci_interrupt(ohci_host_controller_t *sc);
