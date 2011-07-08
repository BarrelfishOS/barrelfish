/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PS_H
#define PS_H

#include <stdbool.h>
#include <barrelfish_kpi/types.h>

#define MAX_DOMAINS     256

struct ps_entry {
    char        *name;
};

void ps_add(domainid_t domain_id, const char *name);
void ps_remove(domainid_t domain_id);
bool ps_exists(domainid_t domain_id);
struct ps_entry *ps_get(domainid_t domain_id);

#endif
