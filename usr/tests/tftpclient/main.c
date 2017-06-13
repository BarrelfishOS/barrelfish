/**
 * \file main.c
 * \brief 
 */


/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <tftp/tftp.h>

static char *file = "test";
static char *server = "127.0.0.1";
uint16_t port = 69;

#define TFTP_BUF_SIZE (1<<20)

char buffer[TFTP_BUF_SIZE];

static void parse_uri(char *uri)
{
    debug_printf("TFTP PARSE URI '%s'\n", uri);
    if (uri == NULL) {
        return;
    }

    if (strncmp(uri, "tftp://", 7)) {
        return;
    }

    uri += 7;

    /* format: tftp://10.110.4.4:69 */
    char *del = strchr(uri, ':');
    if (del != NULL) {
        port = atoi(del + 1);
        *del = 0;
    }

    server = uri;
}

int main(int argc, char *argv[])
{
    errval_t err;

    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "--server=", 9) == 0) {
            parse_uri(argv[i] + 9);
        } else if (strncmp(argv[i], "--file=", 7) == 0) {
            file = argv[i] + 7;
        } else {
            debug_printf("TFTP WARNING unknown argument '%s'\n", argv[i]);
        }
    }

    debug_printf("TFTP SERVER: %s:%u\n", server, port);
    err = tftp_client_connect(server, port);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "TFTP ERROR Could not connect to the tftp service");
    }

    debug_printf("TFTP READFILE: %s\n", file);

    size_t size;
    err = tftp_client_read_file(file, buffer, TFTP_BUF_SIZE, &size);
    if (err_is_fail(err)) {
        USER_PANIC("TFTP ERRO: reading tftp file");
    }

    debug_printf("TFTP READFILE: %zu bytes\n", size);

    debug_printf("TFTP FILE CONTENTS: %s\n", buffer);

    debug_printf("TFTP TEST DONE. \n");

    // prevent main exit since we do not have
    // graceful flounder channel teardown
    while(1) {
        event_dispatch(get_default_waitset());
    }
}
