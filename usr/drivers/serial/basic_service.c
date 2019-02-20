/**
 * \file
 * \brief Basic/raw interface for serial driver.
 */

/*
 * Copyright (c) 2007, 2008, 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/serial_defs.h>

#include "serial.h"
#include "serial_debug.h"

#define SERVICE_SUFFIX "raw"

struct bs_state {
    struct serial_main *serial;
    struct term_server *server;

    /// input buffers, double-buffered for safety with async IDC sends
    struct serial_buffer inbuf[2];
    int ninbuf;

    /// current consumer of input
    struct serial_binding *terminal;
};


static void tx_handler(void *arg)
{
    struct bs_state * bs = arg;
    struct serial_binding *b = bs->terminal;
    errval_t err;

    // free previously-sent buffer, if there is one
    if (bs->inbuf[!bs->ninbuf].buf != NULL) {
        free(bs->inbuf[!bs->ninbuf].buf);
        bs->inbuf[!bs->ninbuf].buf = NULL;
    }

    // do we have something to send? if not, bail out
    if (bs->inbuf[bs->ninbuf].buf == NULL) {
        return;
    }

    // try to send
    err = b->tx_vtbl.input(b, MKCONT(tx_handler,b), bs->inbuf[bs->ninbuf].buf,
                           bs->inbuf[bs->ninbuf].len);
    if (err_is_ok(err)) {
        // swing buffer pointer
        bs->ninbuf = !bs->ninbuf;
        assert(bs->inbuf[bs->ninbuf].buf == NULL);
    } else if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending serial input to terminal");
    }
}

static void basic_serial_input(void *st, char *data, size_t length)
{
    struct bs_state *bs = st;
    struct serial_buffer *in = &bs->inbuf[bs->ninbuf]; 

    if (in->buf == NULL) { // allocate a buffer
        in->buf = malloc(length);
        assert(in->buf != NULL);
        memcpy(in->buf, data, length);
        in->len = length;
    } else { // append new data to existing buffer
        in->buf = realloc(in->buf, in->len + length);
        assert(in->buf != NULL);
        memcpy(in->buf + in->len, data, length);
        in->len += length;
    }

    // try to send something, if we're not already doing so
    if (bs->terminal != NULL && bs->inbuf[!bs->ninbuf].buf == NULL) {
        tx_handler(bs);
    }
}

static void output_handler(struct serial_binding *b, const char *c, size_t len)
{
    struct bs_state *bs = b->st;
    bs->serial->output(bs->serial, c, len);
}

static void associate_stdin_handler(struct serial_binding *b)
{
    SERIAL_DEBUG("associate_stdin called on basic interface\n");
    struct bs_state *bs = b->st;

    bs->terminal = b;
    serial_set_new_input_consumer(bs->serial, basic_serial_input, bs);
    // try to send something, if we have it ready
    if (bs->inbuf[bs->ninbuf].buf != NULL) {
        tx_handler(bs);
    }
}

static struct serial_rx_vtbl serial_rx_vtbl = {
    .output = output_handler,
    .associate_stdin = associate_stdin_handler,
};

static errval_t connect_cb(void *st, struct serial_binding *b)
{
    struct bs_state *bs = st;
    SERIAL_DEBUG("Client connected to basic interface.\n");

    b->st = bs;
    b->rx_vtbl = serial_rx_vtbl;
    bs->terminal = b;
    serial_set_new_input_consumer(bs->serial, basic_serial_input, bs);
    // try to send something, if we have it ready
    if (bs->inbuf[bs->ninbuf].buf != NULL) {
        tx_handler(bs);
    }

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    struct bs_state *bs = st;
    size_t size = 0;
    char *service_name = NULL;

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Exporting basic interface failed.\n");
    }

    // build service name as driver_name.SERVICE_SUFFIX
    size = snprintf(NULL, 0, "%s.%s", bs->serial->driver_name, SERVICE_SUFFIX);
    service_name = (char *) malloc(size + 1);
    if (service_name == NULL) {
        USER_PANIC("Error allocating memory.");
    }
    snprintf(service_name, size + 1, "%s.%s", bs->serial->driver_name, SERVICE_SUFFIX);

    SERIAL_DEBUG("About to register basic interface '%s' at nameservice.\n",
                 service_name);

    // register basic serial driver service at nameservice
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Registering basic interface at "
                       "nameserver failed.");
    }

    free(service_name);
}

void start_basic_service(struct serial_main *serial)
{
    errval_t err;
    struct bs_state *bs = malloc(sizeof(struct bs_state));
    bs->serial = serial;
    err = serial_export(bs, export_cb, connect_cb,
                        get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Preparing basic interface for export failed.");
    }

    SERIAL_DEBUG("Exporting basic interface.\n");
}
