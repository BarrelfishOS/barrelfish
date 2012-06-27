/*
 * pci_vmio.h
 *
 * Virtual Input Output device for Host/Guest communication
 *
 *  Created on: May 13, 2012
 *      Author: luki
 */

#ifndef PCI_ETHERNET_H_
#define PCI_ETHERNET_H_

#include "pci.h"
#include "pci_hdr0_mem_dev.h"

struct pci_vmio {
	pci_hdr0_mem_t      ph;
    uint32_t            pci_header[0x40];
    uint64_t            phys_base_addr; //host physical device memory base address
    void *              virt_base_addr; //vmkitmon virtual adress
	size_t              bytes;
	struct pci_device *pci_device;
};

#endif
