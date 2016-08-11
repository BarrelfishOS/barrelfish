/**
 * \file
 * \brief Device manager for Barrelfish.
 *
 * Interacts with the SKB / PCI to start cores, drivers etc.
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <errors/errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h>

int main(int argc, char** argv)
{
    errval_t err;

    printf("Starting listfacts\n");
#ifndef __ARCH_ARM_7A__
    iref_t iref;
    nameservice_blocking_lookup("pci_discovery_done", &iref);
#endif

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }
    printf("Connected to SKB\n");

    err = skb_execute("listing.");
    if (err_is_fail(err)) {
        USER_PANIC_SKB_ERR(err, "Can not execute listing command?");
    }

    printf("SKB FACTS LISTING START\n");
    printf("%s\n", skb_get_output());
    printf("SKB FACTS LISTING END\n");

    //free(result);
    //free(error);

    return EXIT_SUCCESS;
}
