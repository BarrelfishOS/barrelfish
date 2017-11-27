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

#include <devif/queue_interface_backend.h>

struct mlx4_en_priv;

typedef struct mlx4_queue {
    struct devq q;
    // struct mlx4_dev *dev;
    struct mlx4_en_priv *priv;

    uint32_t pci_vendor, pci_deviceid, pci_bus, pci_device, pci_function;
    char *name;
    uint64_t mac_address;

    regionid_t region_id;
    genpaddr_t region_base;
    gensize_t  region_size;
    void *region_mapped;
    
    unsigned interrupt_mode;
    void (*isr)(void *);
} mlx4_queue_t;

int mlx4_en_xmit(struct mlx4_en_priv *priv, int tx_ind, genpaddr_t buffer_data, size_t length);
void mlx4_en_xmit_poll(struct mlx4_en_priv *priv, int tx_ind);

errval_t mlx4_en_enqueue_rx(mlx4_queue_t *queue, regionid_t rid,
                            genoffset_t offset, genoffset_t length,
                            genoffset_t valid_data, genoffset_t valid_length,
                            uint64_t flags);
errval_t mlx4_en_dequeue_rx(mlx4_queue_t *queue, regionid_t* rid, genoffset_t* offset,
                            genoffset_t* length, genoffset_t* valid_data,
                            genoffset_t* valid_length, uint64_t* flags);
errval_t mlx4_en_enqueue_tx(mlx4_queue_t *queue, regionid_t rid,
                            genoffset_t offset, genoffset_t length,
                            genoffset_t valid_data, genoffset_t valid_length,
                            uint64_t flags);
errval_t mlx4_en_dequeue_tx(mlx4_queue_t *queue, regionid_t* rid, genoffset_t* offset,
                            genoffset_t* length, genoffset_t* valid_data,
                            genoffset_t* valid_length, uint64_t* flags);

#endif
