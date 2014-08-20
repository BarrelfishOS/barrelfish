/**
 * \file
 * \brief Proxy for connecting bulk transfer channels over a network connection
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_NET_E10K_H
#define BULK_NET_E10K_H

#include <barrelfish/barrelfish.h>
#include <bulk_transfer/bulk_transfer.h>

#include <dev/e10k_dev.h>

#if 0

struct bulk_net_msgdesc;
struct e10k_binding;
struct e10k_queue;
struct bulk_e10k {
    bool                    ready;
    size_t                  buffer_size;
    size_t                  ring_size;
    uint8_t                 qi;
    struct e10k_binding    *binding;
    e10k_t                  d;
    struct e10k_queue      *q;
    uint64_t                mac;
    struct capref           rxframe;
    struct capref           txframe;
    void                  (*received)(struct bulk_e10k *,
                                      struct bulk_net_msgdesc *);
    void                  (*transmitted)(struct bulk_e10k *,
                                         void *);

    void                   *opaque;
};
#endif

#endif
