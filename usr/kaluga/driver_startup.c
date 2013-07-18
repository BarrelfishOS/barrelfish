#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#ifdef __arm__
#include <if/monitor_blocking_rpcclient_defs.h>
#endif

#include "kaluga.h"

extern char **environ;

#ifdef __x86__
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

    // Construct additional command line arguments containing pci-id.
    // We need one extra entry for the new argument.
    uint64_t vendor_id, device_id;
    char **argv = mi->argv;
    bool cleanup = false;
    err = oct_read(record, "_ { vendor: %d, device_id: %d }",
            &vendor_id, &device_id);
    if (err_is_ok(err)) {
        // We assume that we're starting a device if the query above succeeds
        // and therefore append the pci vendor and device id to the argument
        // list.
        argv = malloc((mi->argc+1) * sizeof(char *));
        memcpy(argv, mi->argv, mi->argc * sizeof(char *));
        char *pci_id  = malloc(10);
        // Make sure pci vendor and device id fit into our argument
        assert(vendor_id < 0x9999 && device_id < 0x9999);
        snprintf(pci_id, 10, "%04"PRIx64":%04"PRIx64, vendor_id, device_id);
        argv[mi->argc] = pci_id;
        mi->argc += 1;
        argv[mi->argc] = NULL;
        cleanup = true;
    }
    err = spawn_program(where, mi->path, argv,
            environ, 0, &mi->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", mi->path);
    }
    if (cleanup) {
        // alloc'd string is the last of our array
        free(argv[mi->argc-1]);
        free(argv);
    }

    return err;
}
#endif

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

    err = spawn_program(core, driver->path, driver->argv + 1, environ, 0,
            &driver->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", driver->path);
        return err;
    }

    // XXX: Manually add cardname (overwrite first (auto) argument)
    // +Weird convention, e1000n binary but cardname=e1000
    char* cardname =
            strcmp(driver->binary, "e1000n") == 0 ? "e1000" : driver->binary;

    size_t name_len = strlen("cardname=") + strlen(cardname) + 1;
    char* card_argument = malloc(name_len);
    sprintf(card_argument, "cardname=%s", cardname);

    // Spawn netd and ngd_mng
    netd->argv[0] = card_argument;
    err = spawn_program(core, netd->path, netd->argv, environ, 0, &netd->did);

    ngd_mng->argv[0] = card_argument;
    err = spawn_program(core, ngd_mng->path, ngd_mng->argv, environ, 0,
            &ngd_mng->did);

    free(card_argument);
    return err;
}
