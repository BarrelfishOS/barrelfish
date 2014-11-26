// for num_spawnds_online()
#include <if/octopus_rpcclient_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER
#include "internal.h"

size_t num_monitors_online(void)
{
    errval_t err;
    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

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
        return 0;
    }
    return count;
}

