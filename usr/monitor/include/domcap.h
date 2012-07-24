/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef GENCAP_H
#define GENCAP_H

#include <barrelfish/caddr.h>
#include <stdint.h>

struct domcapref {
    struct capref croot;
    capaddr_t cptr;
    uint8_t bits;
};

static inline struct domcapref
get_cap_domref(struct capref cap)
{
    uint8_t bits = get_cap_valid_bits(cap);
    return (struct domcapref) {
        .croot = cap_root,
        .cptr = get_cap_addr(cap) >> (CPTR_BITS - bits),
        .bits = bits,
    };
}

#endif
