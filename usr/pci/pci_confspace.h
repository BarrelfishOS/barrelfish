/**
 * \file
 * \brief PCI configuration space access.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_CONFSPACE_H
#define PCI_CONFSPACE_H

struct pci_address {
    uint8_t bus;
    uint8_t device;
    uint8_t function;
};

errval_t pcie_setup_confspace(void);

uint32_t pci_read_conf_header(struct pci_address *address, uint64_t dword);
void pci_write_conf_header(struct pci_address *address, uint64_t dword,
                           uint32_t data);

int pcie_confspace_init(lpaddr_t pbase, uint16_t segment, uint8_t startbus,
                        uint8_t endbus);
lvaddr_t pcie_confspace_access(struct pci_address addr);

void pcie_enable(void);
void pcie_disable(void);

struct pci_device_info *pci_get_device(uint64_t class_code,
   uint64_t sub_class, uint64_t prog_if, uint64_t vendor_id, uint64_t device_id,
   uint64_t bus, uint64_t device, uint64_t function);

#endif
