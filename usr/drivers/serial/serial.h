/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef SERIAL_H
#define SERIAL_H

#include <barrelfish/barrelfish.h>

struct serial_buffer {
    char *buf;
    size_t len;
};

#define SERIAL_PORTBASE_INVALID 0xffffffff
#define SERIAL_IRQ_INVALID      0xffffffff
#define SERIAL_MEMBASE_INVALID  0xffffffffffffffffULL

struct serial_params {
    uint32_t portbase;
    uint32_t irq;
    uint64_t membase;
};

typedef void serial_input_fn_t(char *data, size_t length);

void serial_write(const char *c, size_t len);
errval_t serial_init(struct serial_params *params);
void start_service(void);
void start_basic_service(char *driver_name);
void start_terminal_service(char *driver_name);
void serial_input(char *data, size_t length);
void set_new_input_consumer(serial_input_fn_t fn);

#endif
