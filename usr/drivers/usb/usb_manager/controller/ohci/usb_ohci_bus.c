/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * ------------------------------------------------------------------------
 * OHCI Bus Functions
 * ------------------------------------------------------------------------
 */

struct usb_bus_methods *usb_ohci_get_bus_fn()
{


}








 struct usb_bus_methods ohci_bus_methods =
{
	.endpoint_init = ohci_ep_init,
	.xfer_setup = ohci_xfer_setup,
	.xfer_unsetup = ohci_xfer_unsetup,
	.get_dma_delay = ohci_get_dma_delay,
	.device_resume = ohci_device_resume,
	.device_suspend = ohci_device_suspend,
	.set_hw_power = ohci_set_hw_power,
	.set_hw_power_sleep = ohci_set_hw_power_sleep,
	.roothub_exec = ohci_roothub_exec,
	.xfer_poll = ohci_do_poll,
};
