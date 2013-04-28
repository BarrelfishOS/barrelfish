/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_ROOT_HUB_H_
#define _USB_OHCI_ROOT_HUB_H_
/*
 * =======================================================================
 * This file contains the declarations for the OHCI root hub
 * =======================================================================
 */

#include <usb/usb_error.h>
#include "usb_ohci.h"

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor.
 *
 * Fields:
 *  - status:  configuration descriptor
 *  - port_status:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */
 union ohci_hub_desc {
	struct usb_status status;
	struct usb_hub_port_status port_status;
	struct usb_hub_class_descriptor hub_descriptor;
	uint8_t	temp[128];
};



usb_error_t ohci_roothub_exec(struct usb_device *device,
          struct usb_device_request *req, const void **pptr, uint16_t *plength);
static void ohci_root_intr(usb_ohci_hc_t *hc);
#endif
