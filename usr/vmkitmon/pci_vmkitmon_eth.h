/*
 * pci_vmio.h
 *
 * Virtual Input Output device for Host/Guest communication
 *
 *  Created on: May 13, 2012
 *      Author: luki
 */

#ifndef PCI_VMKITMON_ETH_H_
#define PCI_VMKITMON_ETH_H_

#include "pci.h"
#include "pci_hdr0_mem_dev.h"

// control register
#define PCI_VMKITMON_ETH_RSTIRQ 1

// status register
#define PCI_VMKITMON_ETH_IRQST 1

enum pci_vmkitmon_registers {
	PCI_VMKITMON_ETH_STATUS,
	PCI_VMKITMON_ETH_CONTROL
};

struct pci_vmkitmon_eth {
	pci_hdr0_mem_t      ph;
    uint32_t            pci_header[0x40];
    uint32_t			mmio_register[1];
    uint32_t            mem_guest_paddr; //guest physical base address of memory register
	struct pci_device *pci_device;
};

#endif
