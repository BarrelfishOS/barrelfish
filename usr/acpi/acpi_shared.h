/**
 * \file
 * \brief ACPI daemon Flounder handler functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ACPI_SHARED_H_
#define ACPI_SHARED_H_

#include <acpi.h>
#include <if/acpi_defs.h>
#include <errors/errno.h>
#include <pci/confspace/pci_confspace.h>

#define BIOS_BITS       20
extern struct capref my_devframes_cnode;
extern struct mm pci_mm_physaddr;

extern uintptr_t my_hw_id;

errval_t find_all_apics(void);

int init_all_interrupt_sources(void);
errval_t enable_and_route_interrupt(int gsi, coreid_t dest, int vector);
errval_t set_device_irq(const char* device, uint32_t irq);

int init_acpi(void);
int acpi_arch_init(void);
ACPI_STATUS acpi_eval_integer(ACPI_HANDLE handle, const char *name,
                              ACPI_INTEGER *ret);
errval_t acpi_get_irqtable_device(ACPI_HANDLE parent, acpi_pci_address_t device,
        ACPI_HANDLE *child, uint8_t bus);

void video_init(void);
void buttons_init(void);
void ec_probe_ecdt(void);
void ec_init(void);

void start_service(void);
void acpi_service_arch_init(struct acpi_rx_vtbl *acpi_rx_vtbl);
extern bool vtd_force_off;

void
AcpiOsSetRootPointer (
    ACPI_PHYSICAL_ADDRESS physaddr);


errval_t acpi_arch_copy_bios_mem(void);
errval_t acpi_arch_load_irq_routing_new(void);
void acpi_arch_video_init(void);
errval_t acpi_arch_skb_set_info(void);
errval_t acpi_interrupts_arch_setup(void);

#endif /* ACPI_SHARED_H_ */
