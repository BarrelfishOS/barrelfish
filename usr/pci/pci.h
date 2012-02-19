/**
 * \file
 * \brief Header file for the PCI driver
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_H_
#define PCI_H_

#include <acpi.h>

#include "pci_confspace.h"
#include "mackerelpci.h"
#include "pci_hdr0_dev.h"
#include "pci_hdr1_dev.h"

/// BIOS area is 1MB in size
#define BIOS_BITS       20

#define PCI_NBUSES     256  ///< Maximum number of PCI buses
#define PCI_NDEVICES    32  ///< Maximum number of PCI devices on a bus
#define PCI_NFUNCTIONS   8  ///< Maximum number of PCI functions on a device
#define PCI_NBARS        6  ///< Maximum number of BARs per function
#define PCI_NINTPINS     4  ///< Number of PCI wired interrupt pins (INTA-INTD)

// XXX: this enum defines region types that must not overlap
// with the KPI-defined enum region_type.
enum user_region_type {
    RegionType_LocalAPIC = RegionType_Max,  ///< local APIC start address
    RegionType_IOAPIC                       ///< I/O APIC start address
};

#if 0

struct pci_device_info {
    struct pci_address addr;
    struct bus *bus;
    uint16_t vendor_id;
    uint16_t device_id;
    pci_hdr0_class_code_t classcode;
    bool pcie;
    uint32_t irq;
    struct device_mem *bar_info;
    int nr_allocated_bars;
    bool driver_loaded;
#if 0
    void *lowlevel_representation; /**< representation of the hardware
                                       access to the upper part
                                      of the per core instance of the driver.
                                      not used by applications (or libraries) */
    void *logical_representation; /**< representation of the device to
                                       libraries/applications (such as
                                       representing "eth0") */
#endif
};
#endif

errval_t pci_setup_root_complex(void);
void pci_add_root(struct pci_address addr, uint8_t maxchild, char* handle);
void pci_program_bridges(void);
void pci_init(void);
void pci_init_datastructures(void);
errval_t device_init(bool enable_irq, uint8_t coreid, int vector,
                     uint32_t class_code, uint32_t sub_class, uint32_t prog_if,
                     uint32_t vendor_id, uint32_t device_id,
                     uint32_t *bus, uint32_t *dev,uint32_t *fun,
                     int *nr_allocated_bars);
int pci_get_nr_caps_for_bar(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t index);
struct capref pci_get_cap_for_device(uint8_t bus, uint8_t dev, uint8_t fun,
                                     uint8_t index, int cap_nr);
uint8_t pci_get_cap_type_for_device(uint8_t bus, uint8_t dev, uint8_t fun,
                                    uint8_t index);
void pci_enable_interrupt_for_device(uint32_t bus, uint32_t dev, uint32_t fun,
                                    bool pcie);

/* interrupts.c */
int init_all_apics(void);
errval_t enable_and_route_interrupt(int gsi, coreid_t dest, int vector);

//void pci_set_lowlevel_representation(struct pci_address *address,void *representation);
//void *pci_get_lowlevel_representation(struct pci_address *address);
//void *pci_get_logical_representation(struct pci_address *address);
//void pci_set_logical_representation(struct pci_address *address,void *representation);

extern struct capref biosmem;

// Memory allocator instance for physical address regions and platform memory
extern struct mm pci_mm_physaddr;

// The APIC ID of the core the PCI domain is running on
extern uintptr_t my_apic_id;

#endif // PCI_H_
