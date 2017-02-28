/**
 * \file
 * \brief ACPI RPC Client header file
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ACPI_CLIENT_H_
#define ACPI_CLIENT_H_

#include <errors/errno.h>

/* forward declaration */
struct acpi_binding;

typedef uint64_t acpi_device_handle_t;


struct acpi_binding* get_acpi_binding(void);
errval_t connect_to_acpi(void);

errval_t acpi_client_get_device_handle(const char *dev_id,
                                       acpi_device_handle_t *ret_handle);
errval_t acpi_client_eval_integer(acpi_device_handle_t handle,
                                  const char *path, uint64_t *data);

errval_t acpi_reset(void);
errval_t acpi_sleep(int state);
errval_t acpi_get_vbe_bios_cap(struct capref *retcap, size_t *retsize);

errval_t vtd_create_domain(struct capref pml4);
errval_t vtd_delete_domain(struct capref pml4);
errval_t vtd_domain_add_device(int seg, int bus, int dev, int funct, struct capref pt_addr);
errval_t vtd_domain_remove_device(int seg, int bus, int dev, int funct, struct capref pt_addr);
errval_t vtd_add_devices(void);

#endif /* ACPI_CLIENT_H_ */
