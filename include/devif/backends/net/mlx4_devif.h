/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
 
#ifndef MLX4_DEVIF_H
#define MLX4_DEVIF_H

struct mlx4_queue;

// interrupt_mode: 0 - none, 1 - normal
errval_t mlx4_queue_create(struct mlx4_queue** q, uint32_t vendor, uint32_t deviceid,
    uint32_t bus, uint32_t device, uint32_t function, unsigned interrupt_mode,
    void (*isr)(void *));

#endif
