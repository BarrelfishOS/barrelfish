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

/* Most of the members are smaller, but to allow PCI_DONT_CARE, everything
 * is expressed as uint32_t */

struct pci_addr {
    uint32_t bus;        
    uint32_t device;
    uint32_t function;
};

struct pci_id {
    uint32_t device;
    uint32_t vendor;
};

struct pci_class {
    uint32_t class_code;
    uint32_t subclass;
    uint32_t prog_if; 
};

#endif
