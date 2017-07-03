/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <driverkit/driverkit.h>

#include "kaluga.h"

#define DRIVER_DOMAIN_NAME "driverdomain"

// Add an argument to argc/argv pair. argv must be mallocd!
static void argv_push(int * argc, char *** argv, char * new_arg){
    int new_size = *argc + 1;
    *argv = realloc(*argv, (new_size+1) * sizeof(char*)); // +1 for last NULL entry.
    if(*argv == NULL){
        USER_PANIC("Could not allocate argv memory");
    }
    *argc = new_size;
    (*argv)[new_size-1] = new_arg;
    (*argv)[new_size] = NULL;
}

static errval_t launch_driver_domain(coreid_t where, size_t did, struct module_info* ddomain)
{
    assert(ddomain != NULL);
    errval_t err = SYS_ERR_OK;

    char **argv = NULL;
    int argc = ddomain->argc;
    argv = malloc((argc+1) * sizeof(char *)); // +1 for trailing NULL
    assert(argv != NULL);
    memcpy(argv, ddomain->argv, (argc+1) * sizeof(char *));
    assert(argv[argc] == NULL);

    char* did_str = malloc(26);
    assert(did_str != NULL);
    snprintf(did_str, 26, "%"PRIx64"", did);
    argv_push(&argc, &argv, did_str);

    err = spawn_program_with_caps(where, ddomain->path, argv,
                                  environ, NULL_CAP, NULL_CAP,
                                  0, get_did_ptr(ddomain));
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", ddomain->path);
    }

    free(argv);
    return err;
}

struct domain_instance* instantiate_driver_domain(coreid_t where) {
    static uint64_t did = 1;

    errval_t err = launch_driver_domain(0, did, find_module(DRIVER_DOMAIN_NAME));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
    struct domain_instance* di = ddomain_create_domain_instance(did);
    did++;

    return di;
}
