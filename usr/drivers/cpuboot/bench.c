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
#include <barrelfish/deferred.h>
#include <octopus/octopus.h>
#include <stdio.h>
#include <string.h>

void set_flag(void *arg);
void set_flag(void *arg) {
    bool *flag = arg;
    *flag = true;
}

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

    char **names;
    char **n2;
    size_t len;
    err = oct_get_names(&names, &len, "r'hw\\.processor\\.[0-9]+'");
    assert(err_is_ok(err));
    assert(len > 0);
    assert(names);
    n2 = names;
    printf("%p, %ld\n", names, len);
    domainid_t *load_dom_ids = calloc(len, sizeof(domainid_t));
    printf("%p, %ld\n", names, len);
    int my_bf_id = -1;
    printf("%p, %ld\n", names, len);
    char *load_argv[1] = { LOAD_GEN_DOMAIN };
    printf("%p, %ld\n", names, len);

    int load_count = 0;
    // start load generating domain on all cores
    for (size_t i=0; i<len; i++) {
        load_count++;
        if (load_count > 8) {
            break;
        }
        if (!names) {
            names = n2;
        }
        printf("%ld: %p, %ld\n", i, names, len);
        char* record;
        err = oct_get(&record, names[i]);
        assert(err_is_ok(err));

        uint64_t barrelfish_id, apic_id, processor_id, enabled;
        err = oct_read(record, "_ { barrelfish_id: %d, apic_id: %d, processor_id: %d, enabled: %d }",
                       &barrelfish_id, &apic_id, &processor_id, &enabled);
        assert(err_is_ok(err));

        if (enabled && apic_id != disp_get_core_id()) {
            printf("spawning load generator on apic_id %"PRIx64"\n", apic_id);
            execute_program(apic_id, 1, load_argv, &load_dom_ids[barrelfish_id]);
        } else if (apic_id == disp_get_core_id()) {
            my_bf_id = barrelfish_id;
        }
    }
    if (my_bf_id > -1) {
        execute_program(disp_get_core_id(), 1, load_argv, &load_dom_ids[my_bf_id]);
    }

    if (!names) { names = n2; }
    assert(names);
    oct_free_names(names, len);

    // wait a bit
    static struct deferred_event myevent;
    static const int wait_usec = 5e6;
    bool flag = false;
    deferred_event_register(&myevent, get_default_waitset(),
            wait_usec, MKCLOSURE(set_flag, &flag));
    while(!flag) {
        event_dispatch(get_default_waitset());
    }
    printf("starting benchmark\n");
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

#if 0
    // kill load gens
    for (size_t i = 0; i < len; i++) {
        if (load_dom_ids[i] != 0) {
            spawn_kill(load_dom_ids[i]);
        }
    }
#endif
}
