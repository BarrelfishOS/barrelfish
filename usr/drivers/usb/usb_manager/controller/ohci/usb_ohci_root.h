/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
 
/*
 * =======================================================================
 * This file contains the declarations for the OHCI root hub
 * =======================================================================
 */
 
/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor. 
 * 
 * Fields:
 *  - confg_desc:  configuration descriptor
 *  - iface_desc:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */ 
 union ohci_hub_desc {
	struct usb_status stat;
	struct usb_port_status ps;
	struct usb_hub_descriptor hubd;
	uint8_t	temp[128];
};