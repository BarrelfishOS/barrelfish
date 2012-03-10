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
#include <if/acpi_rpcclient_defs.h>

struct acpi_rpc_client* get_acpi_rpc_client(void);
errval_t connect_to_acpi(void);

errval_t acpi_reset(void);
errval_t acpi_sleep(int state);
errval_t acpi_get_vbe_bios_cap(struct capref *retcap, size_t *retsize);


#endif /* ACPI_CLIENT_H_ */
