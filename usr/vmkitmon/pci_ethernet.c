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
#include "pci_ethernet.h"

#include <pci/pci.h>
#include <arch/x86/barrelfish/iocap_arch.h>

#define INVALID         0xffffffff

#define PCI_CONFIG_ADDRESS_PORT 0x0cf8
#define PCI_CONFIG_DATA_PORT    0x0cfc

#define PCI_ETHERNET_IRQ 11



static struct guest *guest_info;
static struct pci_ethernet *pci_ethernet_unique;

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

/*
static void iommu_init(void *bar_info, int nr_allocated_bars)
{
	printf("IOMMU init!\n");
} */

static genpaddr_t eth_base_paddr = 0;

static void e1000_init(void *bar_info, int nr_allocated_bars)
{
	printf("e1000_init)!\n");
	printf("TODO: STUFF vm...\n");
    
    struct device_mem *bar = (struct device_mem *)bar_info;
    
    eth_base_paddr = bar[0].paddr;

    struct pci_ethernet * eth = pci_ethernet_unique;
    eth->phys_base_addr = bar[0].paddr;

    errval_t err = map_device(&bar[0]);
    if(err_is_fail(err)){
		printf("guest_vspace_map_wrapper failed\n");
	}
    printf("e1000_init: map_device successful vaddr: 0x%lx...\n", (uint64_t)bar[0].vaddr);
    eth->virt_base_addr = bar[0].vaddr;

    err = guest_vspace_map_wrapper(&guest_info->vspace, bar[0].paddr, bar[0].frame_cap[0], bar[0].bytes);
    if(err_is_fail(err)){
    	printf("guest_vspace_map_wrapper failed\n");
    }

}

/*
static void iommu_interrupt_handler(void *arg)
{
    printf("iommu: interrupt\n");
} */

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

struct pci_device *pci_ethernet_new(struct lpc *lpc, struct guest *g)
{
    struct pci_device *dev = calloc(1, sizeof(struct pci_device));
    struct pci_ethernet *host = calloc(1, sizeof(struct pci_ethernet));
    
    pci_ethernet_unique = host;
    guest_info = g;

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
	printf("vmkitmon: registered ethernet driver, waiting for init..\n");

	/*
	r = pci_register_driver_irq((pci_driver_init_fn)iommu_init,
									PCI_CLASS_SYSTEMPERIPHERAL,
									PCI_SUB_IOMMU, PCI_IF_IOMMU,
									PCI_VENDOR_AMD, deviceid,
									PCI_DONT_CARE, PCI_DONT_CARE, function,
									iommu_interrupt_handler, dev);

	printf("vmkitmon: registered iommu driver, waiting for init..\n");
	 */

    return dev;
}
