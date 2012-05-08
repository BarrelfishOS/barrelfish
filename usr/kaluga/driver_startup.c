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

    struct module_info* ngd_mng = find_module("NGD_mng");
    if (ngd_mng == NULL || !is_auto_driver(ngd_mng)) {
        KALUGA_DEBUG("NGD_mng not found or not declared as auto.");
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    err = spawn_program(core, driver->path, driver->argv+1,
            environ, 0, &driver->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", driver->path);
        return err;
    }

    // XXX: Manually add cardname (overwrite first (auto) argument)
    // +Weird convention, e1000n binary but cardname=e1000
    char* cardname = strcmp(driver->binary, "e1000n") == 0 ?
            "e1000" :  driver->binary;

    size_t name_len = strlen("cardname=")+strlen(cardname)+1;
    char* card_argument = malloc(name_len);
    sprintf(card_argument, "cardname=%s", cardname);

    // Spawn netd and ngd_mng
    netd->argv[0] = card_argument;
    err = spawn_program(core, netd->path, netd->argv,
            environ, 0, &netd->did);

    ngd_mng->argv[0] = card_argument;
    err = spawn_program(core, ngd_mng->path, ngd_mng->argv,
            environ, 0, &ngd_mng->did);

    free(card_argument);
    return err;
}
