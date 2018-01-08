/**
 * \file
 * \brief Common PCI data types
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2018 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_TYPES_H
#define PCI_TYPES_H

struct pci_addr {
    uint8_t bus;
    uint8_t device;
    uint8_t function;
};

struct pci_id {
    uint16_t device;
    uint16_t vendor;
};

#endif
