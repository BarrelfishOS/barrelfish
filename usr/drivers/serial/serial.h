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
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
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

struct serial_common;

typedef void (*serial_input_fn_t)
    (void *arg, char *data, size_t length);
typedef void (*serial_output_fn_t)
    (void * arg, const char *c, size_t length);

// Returned from init functions
struct serial_common {
    uint32_t irq;
    uint64_t membase;
    char *driver_name;
    struct serial_buffer buffer;

    // Handler for input from the serial line. Set in main.
    // Module should call serial_input(..) to perform buffering.
    serial_input_fn_t input_consumer;      
    void *input_consumer_arg;      

    // Output on the serial line. Set in module, called from main 
    serial_output_fn_t output;     
    void *output_arg;      
};

errval_t init_serial_common(struct serial_common *m);

// Generic interface
void serial_input(struct serial_common *main, char *data, size_t length);
void serial_set_new_input_consumer(struct serial_common *main,
        serial_input_fn_t fn, void *fn_arg);

// Service interface
void start_service(struct serial_common *main);
void start_basic_service(struct serial_common *main);
void start_terminal_service(struct serial_common *main);

#endif
