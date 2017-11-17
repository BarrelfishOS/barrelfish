/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MLX4_DEVIF_QUEUE_H
#define MLX4_DEVIF_QUEUE_H

#include "../../../queue_interface_internal.h"

struct mlx4_priv;

typedef struct mlx4_queue {
    struct devq q;
    struct mlx4_priv *priv;

    uint32_t pci_vendor, pci_deviceid, pci_bus, pci_device, pci_function;
    char *name;
    uint64_t mac_address;

    regionid_t region_id;
    genpaddr_t region_base;
    gensize_t  region_size;

    unsigned interrupt_mode;
    void (*isr)(void *);
} mlx4_queue_t;

#endif
