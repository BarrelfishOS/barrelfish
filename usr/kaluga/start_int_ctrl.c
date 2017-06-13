/**
 * \file
 * \brief Code responsible for booting application cores
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
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <if/octopus_defs.h>
#include <if/octopus_thc.h>

#include <octopus/octopus.h>
#include <octopus/trigger.h>

#include <skb/skb.h>
#include <thc/thc.h>

#include "kaluga.h"


/** Turn a line of comma seperated values into a a number of arguments
 * and a pointer array of values (a la argc/argv).
 * string pointed to by in _will be modified_
 * argv will be null terminated
 */

static void csv_to_argv(char * in, int * argc, char *** argv) {
    // Determine argc
    *argc = 0;
    for(char * pos = in; *pos; pos++) *argc += *pos == ',' ? 1 : 0;

    char ** argv_out = (char **)malloc(sizeof(char *) * (*argc) + 1);
    *argv = argv_out;
    assert(argv_out != NULL);

    //Replace , with \0 and generate argv
    int argv_idx = 0;
    argv_out[argv_idx] = in;
    argv_idx++;
    for(char * pos = in; *pos; pos++) {
        if(*pos == ',') {
            argv_out[argv_idx] = pos+1;
            argv_idx++;
            *pos = '\0';
        }
    }
    argv_out[argv_idx] = NULL;
}

static void int_controller_change_event(octopus_mode_t mode,
                                        const char* device_record, void* st)
{
    KALUGA_DEBUG("int_controller_change_event!\n");

    errval_t err;
    char ** argv = NULL;

    if (mode & OCT_ON_SET) {
        char * label;
        char * class;
        KALUGA_DEBUG("Device record: %s\n", device_record);
        err = oct_read(device_record, "_ { label: %s, class: %s }",
                &label, &class);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Got malformed int controller device record?");
            goto out;
        }

        KALUGA_DEBUG("Int Controller appeared, Label: %s\n", label);
        // Ask the SKB which binary and where to start it...
        static char* query = "find_int_controller_driver(%s).";
        err = skb_execute_query(query, label);
        if (err_no(err) == SKB_ERR_EXECUTION) {
            KALUGA_DEBUG("No int controller driver found for: label=%s,class=%s\n",
                    label, class);
            goto out;
        }
        else if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to query SKB.\n");
            goto out;
        }

        char * out = skb_get_output();
        int argc;
        csv_to_argv(out, &argc, &argv);

        // TODO: specify core somehow. If core != my_core_id
        // We must make sure that spawnd is up (see start_pci.c)
        coreid_t core = get_my_core_id();
        KALUGA_DEBUG("Starting int controller binary: %s\n", argv[0]);

        err = spawn_program(core, argv[0], argv+1,
                        environ, 0, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Spawning %s failed.", argv[0]);
        }
    }

out:
    free(argv);
}

errval_t watch_for_int_controller(void) {
    KALUGA_DEBUG("watch_for_int_controller\n");
    static char* int_controller_device  = "r'hw\\.int\\.controller\\.[0-9]+' { "
                               " label: _, class: _ }";
    octopus_trigger_id_t tid;
    return oct_trigger_existing_and_watch(int_controller_device, int_controller_change_event, NULL, &tid);
}
