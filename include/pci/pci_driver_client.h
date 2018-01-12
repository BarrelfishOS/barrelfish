/**
 * \file
 * \brief Library to be used in PCI drivers to use PCI functionality.
 */

/*
 * Copyright (c) 2018 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef PCI_DRIVER_CLIENT_H
#define PCI_DRIVER_CLIENT_H

#include <pci/pci_types.h>
#include <barrelfish/caddr.h>
#include <int_route/int_model.h>

struct waitset;

typedef void (*interrupt_handler_fn)(void *);

struct pcid {
    // Derived from initialization
    struct waitset * ws;
    struct pci_addr addr;
    struct pci_id id;
    struct pci_class cls;
    struct int_startup_argument int_arg;

    // 
    struct cnoderef arg_cnode; // CNode (in local cspace) containing the passed caps
};

/**
 * initialize the pci driver client. 
 * \param ws Used for the pci_driver_client binding and interrupts
 * */
errval_t pcid_init(
            struct pcid* pdc,
            struct capref* caps,
            size_t caps_len,
            char** args,
            size_t args_len,
            struct waitset *ws);


/**
 * Memory mapped BAR interface
 */

/**
 * Generic data about any (I/O, mapped or unmapped...) bar
 */
struct pcid_bar_info {
    uint8_t type;             // 0 = memory BAR, 1 = IO BAR
    struct  capref phys_cap;  // phys caps 
    struct  capref frame_cap; // frame caps
    struct  capref io_cap;    // IO cap (only valid if type == 1)
    uint8_t bar_nr;           // BAR number
};
/**
 * Contains information about a mapped bar
 */
struct pcid_mapped_bar_info {
    struct pcid_bar_info info;
    void *vaddr;        // assigned by the device driver when calling map_device()
    genpaddr_t paddr;   // physical base address of device
    uint8_t bits;    // size of a single cap in bits
    size_t bytes;    // size of entire region in bytes
    struct memobj *memobj;   
    struct vregion *vregion; 
};

size_t pcid_get_bar_num(struct pcid* pdc);
errval_t pcid_get_bar_info(struct pcid* pdc, int bar_index, struct pcid_bar_info *ret);
errval_t pcid_map_bar(struct pcid* pdc, int bar_index, struct pcid_mapped_bar_info *ret);


/**
 * Interrupt interface
 */
size_t pcid_get_int_num(struct pcid* pdc);
errval_t pcid_connect_int(struct pcid* pdc, int int_index,
        interrupt_handler_fn handler, void *st);
errval_t pcid_enable_msix(int *num_vectors);

#endif
