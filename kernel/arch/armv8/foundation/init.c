/*
 * Copyright (c) 2016, ETH Zurich.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <serial.h>
#include <stdio.h>

#include <arch/armv8/init.h>

void
arch_init(uint32_t magic, void *pointer) {
    serial_early_init(serial_console_port);
    serial_console_init(true);
    printf("Serial initialised.\n");

    while(1);
}
