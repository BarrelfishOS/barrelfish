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

static char *driver_name = "serial0";       // default driver name

static struct serial_buffer buffer;

static serial_input_fn_t *consumer_serial_input = NULL;

void serial_input(char *data, size_t length)
{
    if (consumer_serial_input != NULL) {
        // There is a consumer (client) attached to either the basic service
        // interface or the terminal service interface. Direct input directly
        // to service.
        consumer_serial_input(data, length);
    } else {
        // No consumer (client) attached. Buffer input.
        if (buffer.buf == NULL) {
            // Allocate a new buffer.
            buffer.buf = (char *) malloc(length);
            assert(buffer.buf != NULL);
            memcpy(buffer.buf, data, length);
            buffer.len = length;
        } else {
            // Append new data to existing buffer.
            buffer.buf = realloc(buffer.buf, buffer.len + length);
            assert(buffer.buf != NULL);
            memcpy(buffer.buf + buffer.len, data, length);
            buffer.len += length;
        }
    }
}

void set_new_input_consumer(serial_input_fn_t fn)
{
    SERIAL_DEBUG("New input consumer set.\n");
    consumer_serial_input = fn;

    // Send previously buffered input to newly attached consumer.
    if (buffer.buf != NULL) {
        SERIAL_DEBUG("Previously buffered input sent to newly attached "
                     "consumer.\n");
        consumer_serial_input(buffer.buf, buffer.len);
        free(buffer.buf);
        buffer.buf = NULL;
    }
}

void start_service(void)
{
    SERIAL_DEBUG("Starting services.\n");
    start_terminal_service(driver_name);
    start_basic_service(driver_name);
}

int main(int argc, char *argv[])
{
    errval_t err;

    struct serial_params params = {
        .portbase       = SERIAL_PORTBASE_INVALID,
        .irq            = SERIAL_IRQ_INVALID,
        .membase        = SERIAL_MEMBASE_INVALID,
    };

    // Parse args
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "portbase=", sizeof("portbase=") - 1) == 0) {
            uint32_t x=
                strtoul(argv[i] + sizeof("portbase=") - 1, NULL, 0);
            if (x == 0 || x > 65535) {
                fprintf(stderr, "Error: invalid portbase 0x%"PRIx32"\n", x);
                goto usage;
            }
            params.portbase = x;
        } else if (strncmp(argv[i], "irq=", sizeof("irq=") - 1) == 0) {
             uint32_t x=
                 strtoul(argv[i] + sizeof("irq=") - 1, NULL, 0);
             if (x == 0) {
                fprintf(stderr, "Error: invalid IRQ %"PRIu32"\n", x);
                goto usage;
            }
            params.irq = x;
        } else if (strncmp(argv[i], "membase=", sizeof("membase=") - 1) == 0) {
            uint64_t x=
                strtoull(argv[i] + sizeof("membase=") - 1, NULL, 0);
            params.membase = x;
        } else if (strncmp(argv[i], "name=", sizeof("name=") - 1) == 0) {
             driver_name = argv[i] + sizeof("name=") - 1;
        } else if (strncmp(argv[i], "auto", 4) == 0) {
            // do nothing, means we are being started through kaluga
        } else if (strncmp(argv[i], "int_model=", sizeof("int_model=") - 1) == 0) {
            // ignore. x86 just assumes that legacy interrupts are used.
        } else {
            fprintf(stderr, "Error: unknown option %s\n", argv[i]);
            goto usage;
        }
    }

    // Initialize serial driver
    err = serial_init(&params);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error initializing serial driver.");
    }

    SERIAL_DEBUG("Serial driver initialized.\n"
                 "Using driver name %s.\n", driver_name);

    // Stick around waiting for input
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Error dispatching events.");
        }
    }

    return EXIT_SUCCESS;

usage:
    fprintf(stderr, "Usage: %s [portbase=PORT] [irq=IRQ] [name=NAME]\n"
                    "    [membase=ADDR] [kernel]\n", argv[0]);
    return 1;
}
