/*
 * pci_vmio.h
 *
 * Virtual Network device for Host/Guest communication
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
#define PCI_VMKITMON_ETH_TXMIT 2

// status register
#define PCI_VMKITMON_ETH_IRQST 1

enum pci_vmkitmon_registers {
	PCI_VMKITMON_ETH_STATUS,
	PCI_VMKITMON_ETH_CONTROL,
	PCI_VMKITMON_ETH_MAC_LOW,
	PCI_VMKITMON_ETH_MAC_HIGH,
	PCI_VMKITMON_ETH_TX_ADR,	//Guest Physical ptr to one packet
	PCI_VMKITMON_ETH_TX_LEN,	//Packet length
	PCI_VMKITMON_ETH_RX_ADR, 	//Guest Physical ptr to receive buffer
	PCI_VMKITMON_ETH_RX_SIZE	//Size of the receive buffer
};

struct pci_vmkitmon_eth {
	pci_hdr0_mem_t      ph;
    uint32_t            pci_header[0x40];
    uint32_t			mmio_register[6];
    uint32_t            mem_guest_paddr; //guest physical base address of memory register
	struct pci_device *pci_device;
};

#endif
