/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains functions to set and retrive context 
 * of service clients. 
 * The set context saves the information into 
 * the I/O request wrapper 
 */

#ifndef __EHCI_CONTEXT_H
#define __EHCI_CONTEXT_H

#include <if/ehci_defs.h>

void set_context(struct ehci_service_response *rsp);
struct ehci_service_response *get_context(void);

#endif                          // __EHCI_CONTEXT_H
