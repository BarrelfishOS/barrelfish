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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
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

#endif /* ACPI_CLIENT_H_ */
