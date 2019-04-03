/**
 * \file
 * \brief PCI configuration library.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_H
#define PCI_H

#include <barrelfish/inthandler.h>
#include <pci/mem.h>
#include <pci/devids.h>
#include <pci/pci_types.h>

/**
 * Kaluga passes a CNode with capabilities to the pci driver. The offset
 * in this CNode are defined here
 */
#define PCIARG_SLOT_DEVID  0
#define PCIARG_SLOT_INT    1
#define PCIARG_SLOT_BAR0   2
#define PCIARG_SLOT_BAR1   3
#define PCIARG_SLOT_BAR2   4
#define PCIARG_SLOT_BAR3   5
#define PCIARG_SLOT_BAR4   6
#define PCIARG_SLOT_BAR5   7
#define PCIARG_SLOT_BAR6   8
#define PCIARG_SLOT_IO     9


typedef void (*pci_driver_init_fn)(void *user_state, struct device_mem *bar_info,
                                   int nr_mapped_bars);
typedef void (*legacy_driver_init_fn)(void);


errval_t pci_get_bar_caps_for_device(
        struct pci_addr addr,
        struct device_mem **bars_out,
        size_t *bars_len
        );

// Virtual Functions
errval_t pci_sriov_get_vf_bar_cap(uint32_t vf_num, uint8_t bar_num,
                                  struct capref* bar);

// Gets the "Starting package" for a VF which would normally be handed over from Kaluga
// Required to start Arrakis style Applications that use VFs
errval_t pci_sriov_get_vf_resources(uint32_t vf_num, struct capref* regs, struct capref* irq,
                                    struct capref* iommu_ep, struct capref* pci_ep);


errval_t pci_parse_int_arg(int argc, char ** argv);

errval_t pci_reregister_irq_for_device(uint32_t class, uint32_t subclass, uint32_t prog_if,
                                       uint32_t vendor, uint32_t device,
                                       uint32_t bus, uint32_t dev, uint32_t fun,
                                       interrupt_handler_fn handler,
                                       void *handler_arg,
                                       interrupt_handler_fn reloc_handler,
                                       void *reloc_handler_arg);

errval_t pci_register_driver_noirq(pci_driver_init_fn init_func,
                                   void *user_state, uint32_t class,
                                   uint32_t subclass, uint32_t prog_if,
                                   uint32_t vendor, uint32_t device,
                                   uint32_t bus, uint32_t dev, uint32_t fun);

errval_t pci_register_driver_movable_irq(pci_driver_init_fn init_func,
                                         void *user_state, uint32_t class,
                                         uint32_t subclass, uint32_t prog_if,
                                         uint32_t vendor, uint32_t device,
                                         uint32_t bus, uint32_t dev, uint32_t fun,
                                         interrupt_handler_fn handler,
                                         void *handler_arg,
                                         interrupt_handler_fn reloc_handler,
                                         void *reloc_handler_arg);

errval_t pci_register_driver_irq(pci_driver_init_fn init_func,
                                 void *user_state, uint32_t class,
                                 uint32_t subclass, uint32_t prog_if,
                                 uint32_t vendor, uint32_t device,
                                 uint32_t bus, uint32_t dev, uint32_t fun,
                                 interrupt_handler_fn handler, void *handler_arg);

/**
 * Setup interrupt routing manually. If interrupt handler
 * function is passed to register_driver it will be called called from there.
 * Use this in your init function (or any later point) if you want to:
 *  * MSIx
 *  * Unusual interrupt routing.
 *  * Activate interrupts later
 */
errval_t pci_setup_int_routing(int irq_idx, interrupt_handler_fn handler,
                                         void *handler_arg,
                                         interrupt_handler_fn reloc_handler,
                                         void *reloc_handler_arg);

/**
 * Deprecated. Use pci_register_legacy_driver_irq_cap.
 */
errval_t pci_register_legacy_driver_irq(legacy_driver_init_fn init_func,
                                        uint16_t iomin, uint16_t iomax, int irq,
                                        interrupt_handler_fn handler,
                                        void *handler_arg);

/**
 * Register a legacy device driver. The driver can specify a range of IO ports
 * that he wants to access.
 *
 * @param init_func          Callback function that will be called once pci_client is initialized.
 * @param iomin              I/O range minimum
 * @param iomax              I/O range maximum
 * @param irq_idx            Interrupt cap index
 * @param interrupt_handler  The handler function when a interrupt is triggered.
 * @param handler_arg        Argument for interrupt_handler
 *
 */
errval_t pci_register_legacy_driver_irq_cap(legacy_driver_init_fn init_func,
                                        uint16_t iomin, uint16_t iomax, int irq_idx,
                                        interrupt_handler_fn handler,
                                        void *handler_arg);

errval_t pci_setup_inthandler(interrupt_handler_fn handler, void *handler_arg,
                              uint8_t *ret_vector);
/**
 * Enable a Virtual function of a device
 *
 * @param vf_num        VF number
 *
 */
errval_t pci_sriov_enable_vf(uint32_t vf_num);

errval_t pci_read_conf_header(uint32_t dword, uint32_t *val);

errval_t pci_write_conf_header(uint32_t dword, uint32_t val);

errval_t pci_client_connect(void);


/**
 * Connect to PCI service using endpoint cap
 * @param ep        Endpoint cap to PCI
 */
errval_t pci_client_connect_ep(struct capref ep);

/**
 * Enable MSI-X for the PCI device
 * @param count Memory location where the number of supported vectors is written
 */
errval_t pci_msix_enable(uint16_t *count);

/**
 * \brief enables MSI-X interupts for a given device
 *
 * \param addr  PCI address of the device to activate or NULL if don't care
 * \param count returns the number of supported MSI-X interrupts
 *
 * \returns SYS_ERR_OK on success
 *          errval on FAILURE
 */
errval_t pci_msix_enable_addr(struct pci_addr *addr, uint16_t *count);

/**
 * Configure an MSI-X vector
 * @param index       MSI-X Vector index
 * @param destination Destination APIC where the interrupt should be sent
 * @param vector      Interrupt vector to send
 */
errval_t pci_msix_vector_init(uint16_t index, uint8_t destination,
                              uint8_t vector);
/**
 * Configure an MSI-X vector
 * \param addr  PCI address of the device to activate or NULL if don't care
 * \param index       MSI-X Vector index
 * \param destination Destination APIC where the interrupt should be sent
 * \param vector      Interrupt vector to send
 */
errval_t pci_msix_vector_init_addr(struct pci_addr *addr, uint16_t index,
                                   uint8_t destination, uint8_t vector);

errval_t pci_setup_int_routing_with_cap(int irq_idx, 
                                        struct capref irq_src_cap,
                                        interrupt_handler_fn handler,
                                        void *handler_arg,
                                        interrupt_handler_fn reloc_handler,
                                        void *reloc_handler_arg);

/**
 * Get info of PCI device
 * \param addr       PCI address of the device must not be null.
 * \param id         PCI id of the device must not be null.
 */
errval_t pci_get_device_info(struct pci_addr* addr, struct pci_id* id);
#endif
