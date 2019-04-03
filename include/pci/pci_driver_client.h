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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
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
    size_t num_bars;

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
 * Getting arguments that were passed by kaluga
 * \param ws Used for the pci_driver_client binding and interrupts
 * */
errval_t pcid_get_interrupt_cap(struct pcid* pdc, struct capref *ret);
errval_t pcid_get_devid_cap(struct pcid* pdc, struct capref *ret);
errval_t pcid_get_bar_cap(struct pcid* pdc, int bar_index, struct capref *ret);
size_t pcid_get_bar_num(struct pcid* pdc);
/**
 * Interrupt interface
 */
size_t pcid_get_int_num(struct pcid* pdc);
errval_t pcid_connect_int(struct pcid* pdc, int int_index,
                          interrupt_handler_fn handler, void *st);
errval_t pcid_connect_int_with_cap(struct capref src_int, int int_index,
                                   interrupt_handler_fn handler, void *st);
errval_t pcid_enable_msix(int *num_vectors);

#endif
