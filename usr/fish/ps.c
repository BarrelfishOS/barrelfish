/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ps.h"

static struct ps_entry entries[MAX_DOMAINS];

void ps_add(domainid_t domain_id, const char *name)
{
    assert(domain_id < MAX_DOMAINS);
    struct ps_entry *e = &entries[domain_id];
    e->name = malloc(strlen(name) + 1);
    strcpy(e->name, name);
}

void ps_remove(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    struct ps_entry *e = &entries[domain_id];
    free(e->name);
    e->name = NULL;
}

bool ps_exists(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    struct ps_entry *e = &entries[domain_id];
    return e->name != NULL;
}

struct ps_entry *ps_get(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    return &entries[domain_id];
}
