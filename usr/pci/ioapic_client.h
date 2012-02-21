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

#ifndef IOAPIC_CLIENT_H_
#define IOAPIC_CLIENT_H_

#include <errors/errno.h>
#include <if/ioapic_rpcclient_defs.h>

struct ioapic_rpc_client* get_ioapic_rpc_client(void);
errval_t connect_to_ioapic(void);

#endif /* IOAPIC_CLIENT_H_ */
