// for num_spawnds_online()
#include <if/octopus_rpcclient_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER
#include "monitor.h"
#include "internal.h"

struct bind_state {
    bool done;
    errval_t err;
};

static void error_handler(struct octopus_binding *b, errval_t err)
{
    USER_PANIC_ERR(err, "asynchronous error in nameservice binding");
}

static void bind_continuation(void *st_arg, errval_t err,
                              struct octopus_binding *b)
{
    struct bind_state *st = st_arg;

    if (err_is_ok(err)) {
        b->error_handler = error_handler;

        struct octopus_rpc_client *r;
        r = malloc(sizeof(struct octopus_rpc_client));
        assert(r != NULL);
        err = octopus_rpc_client_init(r, b);
        if (err_is_fail(err)) {
            free(r);
            USER_PANIC_ERR(err, "error in nameservice_rpc_client_init");
        } else {
            set_octopus_rpc_client(r);
        }
    }

    st->err = err;
    st->done = true;
}

static errval_t bind_to_octopus(void)
{
    errval_t err;
    struct bind_state st = { .done = false };
    err = octopus_bind(name_serv_iref, bind_continuation, &st,
            get_default_waitset(), IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    while (!st.done) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ev_disp in bind_to_octopus");
        }
    }

    return st.err;
}

size_t num_monitors_online(void)
{
    errval_t err;
    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        err = bind_to_octopus();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "bind_to_octopus");
            debug_printf("no connection to octopus, num_monitors=1\n");
            return 1;
        }
        r = get_octopus_rpc_client();
    }
    assert(r != NULL);

    char* buffer = NULL;
    errval_t error_code;
    octopus_trigger_id_t tid;

    char** names = NULL;
    size_t count = 0;

    static char* spawnds = "r'spawn.[0-9]+' { iref: _ }";
        err = r->vtbl.get_names(r, spawnds, NOP_TRIGGER, &buffer, &tid, &error_code);
    if (err_is_fail(err) || err_is_fail(error_code)) {
        err = err_push(err, SPAWN_ERR_FIND_SPAWNDS);
        goto out;
    }

    err = oct_parse_names(buffer, &names, &count);
    if (err_is_fail(err)) {
        goto out;
    }

out:
    free(buffer);
    oct_free_names(names, count);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "num_spawnds_online");
        debug_printf("error in octopus, setting num_monitors=1\n");
        return 1;
    }
    return count;
}

