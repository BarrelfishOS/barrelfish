/**
 * \file
 * \brief Beehive message transport kernel component
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_BEEHIVE_BMP_H
#define KERNEL_ARCH_BEEHIVE_BMP_H

#include <stdlib.h> // uint32_t
#include <barrelfish_kpi/legacy_idc_buffer.h> // struct idc_recv_msg
#include <barrelfish_kpi/syscalls.h> // struct sysret
#include <corearea.h> // struct corearea

void bmp_init(void);

void bmp_pump(void);

// This returns when the messenger is empty and either a message
// arrived at some point, or a timer interrupt is pending.
void bmp_pump_until_timer(struct corearea *corearea);

void bmp_send(int core, int assoc, int size, uint32_t *words);

struct sysret bmp_table_set(struct capability *to, struct idc_recv_msg *msg);
struct sysret bmp_table_delete(struct capability *to, struct idc_recv_msg *msg);

#endif // KERNEL_ARCH_BEEHIVE_BMP_H
