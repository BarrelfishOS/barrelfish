#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/deferred.h>
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
    err = spawn_program(coreid, prog, argv, NULL, SPAWN_FLAGS_NEW_DOMAIN,
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

static struct deferred_event myevent;
static coreid_t loopy_core = 3;
static coreid_t core_inc = 1;
static coreid_t next_core = 0;
#define ROUNDS 2
static int rounds = 0;

static const int wait_usec = 5e6;

static void restart_core(void *arg)
{
    char corestr[3] = { 0 };
    snprintf(corestr, 2, "%x", next_core);
    next_core += core_inc;
    if (next_core > loopy_core) {
        next_core = core_inc;
        rounds++;
    }
    if (rounds > ROUNDS) {
        exit(0);
    }

    // restart a core
    char *argv[] = {
        "corectrl",
        "update",
        corestr,
    };
    domainid_t x86id;
    debug_printf("restarting core %d\n", next_core);
    execute_program(disp_get_core_id(), 3, argv, &x86id);
    wait_domain_id(x86id);

    // wait a bit
    deferred_event_register(&myevent, get_default_waitset(),
            wait_usec, MKCLOSURE(restart_core, NULL));
}
int main(int argc, char *argv[])
{
    if (argc == 3) {
        loopy_core = atoi(argv[1]);
        core_inc = atoi(argv[2]);
    }
    next_core = core_inc;
    deferred_event_init(&myevent);
    // spawn loopy on fixed core
    char *loopy_argv[1] = { "loopy" };
    domainid_t retid;
    execute_program(loopy_core, 1, loopy_argv, &retid);

    // 2: wait a bit
    deferred_event_register(&myevent, get_default_waitset(),
            wait_usec, MKCLOSURE(restart_core, NULL));

    while(true) {
        event_dispatch(get_default_waitset());
    }
    return 0;
}
