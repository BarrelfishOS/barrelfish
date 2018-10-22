/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/deferred.h>

static void usage(char *progname)
{
    printf("Usage: %s <prog-to-run> <run-count> <run-rounds>\n", progname);
}

int main(int argc, char *argv[])
{
    if (argc < 4) {
        usage(argv[0]);
        return 1;
    }
    char *prog_to_run = argv[1];
    int run_count = atoi(argv[2]);
    int run_rounds = atoi(argv[3]);
    char *prog_argv[2] = { prog_to_run, NULL };

    printf("frequency_bench starting, printing mdb counters\n");
    sys_debug_print_mdb_counters();

    errval_t err;
    coreid_t my_core_id = disp_get_current_core_id();
    for (int r = 0; r < run_rounds; r++) {
        domainid_t domids[run_count];
        for (int c = 0; c < run_count; c++) {
            err = spawn_program(my_core_id, prog_to_run, prog_argv, NULL,
                    SPAWN_FLAGS_DEFAULT, &domids[c]);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "spawn_program");
            }
        }
        for (int c = 0; c < run_count; c++) {
            uint8_t exitcode;
            err = spawn_wait(domids[c], &exitcode, false);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "spawn_wait");
            }
            if (exitcode != 0) {
                debug_printf("program %d exited with %d\n", c, exitcode);
            }
        }
        // sleep 5s between rounds to give cleanup time to settle
        barrelfish_usleep(5000000);
    }

    // sleep for 5s, to make sure all cleanup has happened
    barrelfish_usleep(5000000);

    printf("frequency_bench finished, printing mdb counters\n");
    sys_debug_print_mdb_counters();
    printf("frequency_bench done\n");
    return 0;
}
