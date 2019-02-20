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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include "serial.h"
#include "serial_debug.h"


void serial_input(struct serial_main *main, char *data, size_t length)
{
    if (main->input_consumer != NULL) {
        // There is a consumer (client) attached to either the basic service
        // interface or the terminal service interface. Direct input directly
        // to service.
        main->input_consumer(main->input_consumer_arg, data, length);
    } else {
        // No consumer (client) attached. Buffer input.
        if (main->buffer.buf == NULL) {
            // Allocate a new buffer.
            main->buffer.buf = (char *) malloc(length);
            assert(main->buffer.buf != NULL);
            memcpy(main->buffer.buf, data, length);
            main->buffer.len = length;
        } else {
            // Append new data to existing buffer.
            main->buffer.buf = realloc(main->buffer.buf, main->buffer.len + length);
            assert(main->buffer.buf != NULL);
            memcpy(main->buffer.buf + main->buffer.len, data, length);
            main->buffer.len += length;
        }
    }
}

void serial_set_new_input_consumer(struct serial_main *main,
                                   serial_input_fn_t fn, void *fn_arg)
{
    SERIAL_DEBUG("New input consumer set. main=%p\n", main);
    main->input_consumer = fn;
    main->input_consumer_arg = fn_arg;

    // Send previously buffered input to newly attached consumer.
    if (main->buffer.buf != NULL) {
        SERIAL_DEBUG("Previously buffered input sent to newly attached "
                     "consumer.\n");
        main->input_consumer(main->input_consumer_arg, main->buffer.buf,
                main->buffer.len);
        free(main->buffer.buf);
        main->buffer.buf = NULL;
    }
}

void start_service(struct serial_main *m)
{
    SERIAL_DEBUG("Starting services.\n");
    start_terminal_service(m);
    start_basic_service(m);
}

errval_t init_serial_main(struct serial_main *m, int argc, char **argv)
{
    // defaults
    m->driver_name = "serial0";       
    m->input_consumer = NULL;
    m->input_consumer_arg = NULL;
    m->buffer.buf = NULL;
    m->buffer.len = 0;
    m->portbase       = SERIAL_PORTBASE_INVALID;
    m->irq            = SERIAL_IRQ_INVALID;
    m->membase        = SERIAL_MEMBASE_INVALID;

    // Parse args
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "portbase=", sizeof("portbase=") - 1) == 0) {
            uint32_t x=
                strtoul(argv[i] + sizeof("portbase=") - 1, NULL, 0);
            if (x == 0 || x > 65535) {
                fprintf(stderr, "Error: invalid portbase 0x%"PRIx32"\n", x);
                goto usage;
            }
            m->portbase = x;
        } else if (strncmp(argv[i], "irq=", sizeof("irq=") - 1) == 0) {
             uint32_t x=
                 strtoul(argv[i] + sizeof("irq=") - 1, NULL, 0);
             if (x == 0) {
                fprintf(stderr, "Error: invalid IRQ %"PRIu32"\n", x);
                goto usage;
            }
            m->irq = x;
        } else if (strncmp(argv[i], "membase=", sizeof("membase=") - 1) == 0) {
            uint64_t x=
                strtoull(argv[i] + sizeof("membase=") - 1, NULL, 0);
            m->membase = x;
        } else if (strncmp(argv[i], "name=", sizeof("name=") - 1) == 0) {
             m->driver_name = argv[i] + sizeof("name=") - 1;
        } else if (strncmp(argv[i], "auto", 4) == 0) {
            // do nothing, means we are being started through kaluga
        } else if (strncmp(argv[i], "int_model=", sizeof("int_model=") - 1) == 0) {
            // ignore. x86 just assumes that legacy interrupts are used.
        } else {
            fprintf(stderr, "Error: unknown option %s\n", argv[i]);
            goto usage;
        }
    }

    return SYS_ERR_OK;

usage:
    fprintf(stderr, "Usage: %s [portbase=PORT] [irq=IRQ] [name=NAME]\n"
                    "    [membase=ADDR] [kernel]\n", argv[0]);
    return SYS_ERR_IRQ_INVALID;
}
