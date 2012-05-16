/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/serial_defs.h>
#include "serial.h"

#define DEFAULT_PORTBASE    0x3f8   //< COM1 port
#define DEFAULT_IRQ         4       //< COM1 IRQ

static const char *service_name = "serial"; // default service name

/// current consumer of input
static struct serial_binding *terminal;

/// input buffers, double-buffered for safety with async IDC sends
static struct {
    char *buf;
    size_t len;
} inbuf[2];
static int ninbuf;

static void tx_handler(void *arg)
{
    struct serial_binding *b = arg;
    errval_t err;

    // free previously-sent buffer, if there is one
    if (inbuf[!ninbuf].buf != NULL) {
        free(inbuf[!ninbuf].buf);
        inbuf[!ninbuf].buf = NULL;
    }

    // do we have something to send? if not, bail out
    if (inbuf[ninbuf].buf == NULL) {
        return;
    }

    // try to send
    err = b->tx_vtbl.input(b, MKCONT(tx_handler,b), inbuf[ninbuf].buf,
                           inbuf[ninbuf].len);
    if (err_is_ok(err)) {
        // swing buffer pointer
        ninbuf = !ninbuf;
        assert(inbuf[ninbuf].buf == NULL);
    } else if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending serial input to terminal");
    }
}

void serial_input(char *data, size_t length)
{
    if (inbuf[ninbuf].buf == NULL) { // allocate a buffer
        inbuf[ninbuf].buf = malloc(length);
        assert(inbuf[ninbuf].buf != NULL);
        memcpy(inbuf[ninbuf].buf, data, length);
        inbuf[ninbuf].len = length;
    } else { // append new data to existing buffer
        inbuf[ninbuf].buf = realloc(inbuf[ninbuf].buf, inbuf[ninbuf].len + length);
        assert(inbuf[ninbuf].buf != NULL);
        memcpy(inbuf[ninbuf].buf + inbuf[ninbuf].len, data, length);
        inbuf[ninbuf].len += length;
    }

    // try to send something, if we're not already doing so
    if (terminal != NULL && inbuf[!ninbuf].buf == NULL) {
        tx_handler(terminal);
    }
}

static void output_handler(struct serial_binding *b, char *c, size_t len)
{
    serial_write(c, len);
    free(c);
}

static void associate_stdin_handler(struct serial_binding *b)
{
    terminal = b;
    // try to send something, if we have it ready
    if (inbuf[ninbuf].buf != NULL) {
        tx_handler(b);
    }
}

static struct serial_rx_vtbl serial_rx_vtbl = {
    .output = output_handler,
    .associate_stdin = associate_stdin_handler,
};

static errval_t connect_cb(void *st, struct serial_binding *b)
{
    b->rx_vtbl = serial_rx_vtbl;
    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "serial export failed");
        abort();
    }

    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice register failed");
        abort();
    }
}

void start_service(void)
{
    errval_t err;
    err = serial_export(NULL, export_cb, connect_cb, get_default_waitset(),
                        IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));
}


int main(int argc, char *argv[])
{
    int r;

    uint16_t portbase = DEFAULT_PORTBASE;
    int irq = DEFAULT_IRQ;

    // Parse args
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "portbase=", sizeof("portbase=") - 1) == 0) {
            unsigned long x = strtoul(argv[i] + sizeof("portbase=") - 1, NULL, 0);
            if (x == 0 || x > 65535) {
                fprintf(stderr, "Error: invalid portbase 0x%lx\n", x);
                goto usage;
            }
            portbase = (uint16_t) x;
        } else if (strncmp(argv[i], "irq=", sizeof("irq=") - 1) == 0) {
             unsigned long x = strtoul(argv[i] + sizeof("irq=") - 1, NULL, 0);
             if (irq == 0 || irq > 255) {
                fprintf(stderr, "Error: invalid IRQ %lu\n", x);
                goto usage;
            }
            irq = (int) x;
        } else if (strncmp(argv[i], "name=", sizeof("name=") - 1) == 0) {
             service_name = argv[i] + sizeof("name=") - 1;
        } else {
            fprintf(stderr, "Error: unknown option %s\n", argv[i]);
            goto usage;
        }
    }

    if (argc > 1) {
        printf("%s: using port base 0x%x and IRQ %d. Registering as '%s'.\n",
               argv[0], portbase, irq, service_name);
    }

    // Initialize serial driver
    r = serial_init(portbase, irq);
    assert(r == 0);

    // Stick around waiting for input
    messages_handler_loop();

    return 0;

usage:
    fprintf(stderr, "Usage: %s [portbase=PORT] [irq=IRQ] [name=NAME]\n", argv[0]);
    return 1;
}
