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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_TYPES_H
#define PCI_TYPES_H

#include <stdint.h>
#include <stdio.h>
#include <errors/errno.h>

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

#define PCI_OCTET_LEN (8*4+8)

static inline void pci_serialize_octet(
        struct pci_addr addr,
        struct pci_id id,
        struct pci_class cls,
        char *out)
{
    snprintf(out, 8*4+8, "%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x",
           addr.bus, addr.device, addr.function,
           id.device, id.vendor,
           cls.class_code, cls.subclass, cls.prog_if);
};

static inline errval_t pci_deserialize_octet(
        char *in,
        struct pci_addr* addr,
        struct pci_id* id,
        struct pci_class* cls
        )
{
    int scn = sscanf(in, "%x:%x:%x:%x:%x:%x:%x:%x",
           &addr->bus, &addr->device, &addr->function,
           &id->device, &id->vendor,
           &cls->class_code, &cls->subclass, &cls->prog_if);

    return scn == 8 ? SYS_ERR_OK : PCI_ERR_ARG_PARSE;
};


#endif
