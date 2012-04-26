/**
 * \file Fake PCI host bridge
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_hdr0_mem_dev.h"

#include <pci/pci.h>
#include <arch/x86/barrelfish/iocap_arch.h>

#define INVALID         0xffffffff

#define PCI_CONFIG_ADDRESS_PORT 0x0cf8
#define PCI_CONFIG_DATA_PORT    0x0cfc

#define PCI_ETHERNET_IRQ 11

struct pci_ethernet {
    pci_hdr0_mem_t      ph;
    uint32_t            pci_header[0x40];
};

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
    if(addr.d.fnct_nr != 0) {
        return;
    }
    
    if(addr.d.doubleword < 0x40) {
        errval_t r = pci_write_conf_header(addr.d.doubleword, val);
        if(err_is_fail(r)) {
            DEBUG_ERR(r, "Writing conf header failed\n");
        } else {
            VMKIT_PCI_DEBUG("Written to conf header at %u: %x\n", addr.d.doubleword, val);
        }
    }
}

struct pci_address {
    uint8_t bus;
    uint8_t device;
    uint8_t function;
};

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
        return;
    }

    if(addr.d.doubleword < 0x40) {
        errval_t r = pci_read_conf_header(addr.d.doubleword,val);
        if(err_is_fail(r)) {
            DEBUG_ERR(r,"Reading conf header failed\n");
        } else {
            VMKIT_PCI_DEBUG("Read from conf header at %u: %x\n",addr.d.doubleword, *val);
        }
    } else {
        *val = INVALID;
    }
}

static void e1000_init(void *bar_info, int nr_allocated_bars)
{
	printf("e1000_init)!\n");
	printf("TODO: STUFF vm...\n");
    
    printf("nr_allocated_bars: %d\n", nr_allocated_bars);
    int i;
    
    struct device_mem *bar = (struct device_mem *)bar_info;
    for(i = 0; i < nr_allocated_bars; i++) {
        printf("type: %d\n", bar[i].type);
    }
}

static void e1000_interrupt_handler(void *arg)
{
    // Read & acknowledge interrupt cause(s)
    printf("e1000n: packet interrupt\n");
    //TODO trigger interrupt in VM
    /*
    struct pci_device *dev = (struct pci_device *)arg;
    
    lpc_pic_assert_irq(dev->lpc, dev->irq);
     */
}


static uint32_t function = PCI_DONT_CARE;
static uint32_t deviceid = 0x10fb; //PCI_DONT_CARE;

struct pci_device *pci_ethernet_new(struct lpc *lpc)
{
    struct pci_device *dev = calloc(1, sizeof(struct pci_device));
    struct pci_ethernet *host = calloc(1, sizeof(struct pci_ethernet));

    //initialize device
    dev->confspace_write = confspace_write;
    dev->confspace_read = confspace_read;
    dev->state = host;
    dev->irq = PCI_ETHERNET_IRQ;
    dev->lpc = lpc;
    
    //Connect to pci server
	errval_t r = pci_client_connect();
	assert(err_is_ok(r));
	printf("vmkitmon: connected to pci\n");
    
    //Register as driver
	r = pci_register_driver_irq((pci_driver_init_fn)e1000_init,
                                PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
								PCI_VENDOR_INTEL, deviceid,
								PCI_DONT_CARE, PCI_DONT_CARE, function,
								e1000_interrupt_handler, dev);
    
	if(err_is_fail(r)) {
		DEBUG_ERR(r, "ERROR: vmkitmon: pci_register_driver");
	}
	assert(err_is_ok(r));
	printf("vmkitmon: registered driver, waiting for init..\n");

    return dev;
}
