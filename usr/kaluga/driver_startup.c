#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include "kaluga.h"

extern char **environ;

errval_t default_start_function(coreid_t where, struct module_info* mi,
        char* record)
{
    assert(mi != NULL);
    errval_t err = SYS_ERR_OK;

    if (is_started(mi)) {
        return KALUGA_ERR_DRIVER_ALREADY_STARTED;
    }

    if (!is_auto_driver(mi)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    err = spawn_program(where, mi->path, mi->argv+1,
            environ, 0, &mi->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", mi->path);
    }

    return err;
}

errval_t start_networking(coreid_t core, struct module_info* driver,
        char* record)
{
    assert(driver != NULL);
    errval_t err = SYS_ERR_OK;

    if (is_started(driver)) {
        return KALUGA_ERR_DRIVER_ALREADY_STARTED;
    }

    if (!is_auto_driver(driver)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    struct module_info* netd = find_module("netd");
    if (netd == NULL || !is_auto_driver(netd)) {
        KALUGA_DEBUG("netd not found or not declared as auto.");
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    err = spawn_program(core, driver->path, driver->argv+1,
            environ, 0, &driver->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", driver->path);
        return err;
    }

    // XXX: Manually add cardname (overwrite first (auto) argument)
    size_t name_len = strlen("cardname=")+strlen(driver->binary)+1;
    char* cardname = malloc(name_len);
    sprintf(cardname, "cardname=%s", driver->binary);
    netd->argv[0] = cardname;
    err = spawn_program(core, netd->path, netd->argv,
            environ, 0, &netd->did);
    free(cardname);

    return err;

}
