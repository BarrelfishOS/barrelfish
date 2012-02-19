/**
 * \file
 * \brief ACPI management
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ACPI_H
#define ACPI_H

#include <acpi.h>

#include <if/acpi_defs.h>

int init_acpi(void);
ACPI_STATUS acpi_eval_integer(ACPI_HANDLE handle, char *name, ACPI_INTEGER *ret);
void acpi_get_irqtable_device(ACPI_HANDLE parent, acpi_pci_address_t device,
        ACPI_HANDLE *child, uint8_t bus);
void video_init(void);
void buttons_init(void);
void ec_probe_ecdt(void);
void ec_init(void);

#endif
