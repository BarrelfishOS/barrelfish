/**
 * \file
 * \brief Core boot benchmark driver
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <octopus/octopus.h>
#include <stdio.h>
#include <string.h>

static int execute_program(coreid_t coreid, int argc, char *argv[],
                           domainid_t *retdomainid)
{
    errval_t err;

    // if the name contains a directory separator, assume it is relative to PWD
    char *prog = argv[0];
    assert(retdomainid != NULL);

    argv[argc] = NULL;
    err = spawn_program(coreid, prog, argv, NULL, SPAWN_NEW_DOMAIN,
                        retdomainid);

    if (prog != argv[0]) {
        free(prog);
    }

    if (err_is_fail(err)) {
        printf("%s: error spawning: %s\n", argv[0], err_getstring(err));
        DEBUG_ERR(err, "Spawning Error\n");
        return -1;
    }

    return 0;
}

static uint8_t wait_domain_id(domainid_t domainid)
{
    uint8_t exitcode;
    errval_t err = spawn_wait(domainid, &exitcode, false);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn_wait");
    }
    return exitcode;
}


#define LOAD_GEN_DOMAIN "mem_kpi_loadgen"
int main(int argc, char *argv[])
{
    errval_t err;
    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Octopus initialization failed.");
    }

    char** names;
    size_t len;
    err = oct_get_names(&names, &len, "r'hw\\.processor\\.[0-9]+'");
    assert(err_is_ok(err));
    assert(len > 0);
    domainid_t *load_dom_ids = calloc(len, sizeof(domainid_t));

    // start load generating domain on all cores
    for (size_t i=0; i<len; i++) {
        char* record;
        err = oct_get(&record, names[i]);
        assert(err_is_ok(err));

        uint64_t barrelfish_id, apic_id, processor_id, enabled;
        err = oct_read(record, "_ { barrelfish_id: %d, apic_id: %d, processor_id: %d, enabled: %d }",
                       &barrelfish_id, &apic_id, &processor_id, &enabled);
        assert(err_is_ok(err));

        if (enabled) {
            printf("spawning load generator on apic_id %"PRIx64"\n", apic_id);
            char *load_argv[1] = { LOAD_GEN_DOMAIN };
            execute_program(apic_id, 1, load_argv, &load_dom_ids[barrelfish_id]);
        }
    }

    oct_free_names(names, len);

    // run benchmark
    domainid_t x86id;
#ifndef BENCH
#error need to define a benchmark
#else
    char *bench_argv[1] = { BENCH };
    printf("bench_argv[0] = %s\n", bench_argv[0]);
    execute_program(disp_get_core_id(), 1, bench_argv, &x86id);
#endif
    wait_domain_id(x86id);

    // kill load gens
    for (size_t i = 0; i < len; i++) {
        if (load_dom_ids[i] != 0) {
            spawn_kill(load_dom_ids[i]);
        }
    }
}
