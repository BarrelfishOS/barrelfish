/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>
#include <barrelfish/nameservice_client.h>

#include <dist2/init.h>
#include <dist2/trigger.h>
#include <dist2/getset.h>

#ifndef USE_CHIPS_NS
errval_t nameservice_lookup(const char *iface, iref_t *retiref)
{
    errval_t err;

    char* record;
    err = dist_get(&record, iface);
    if (err_no(err) == DIST2_ERR_NO_RECORD) {
        return err_push(err, LIB_ERR_NAMESERVICE_UNKNOWN_NAME);
    }

    // XXX: numbers from records are 64bit, irefs are 32
    uint64_t iref_number = 0;
    err = dist_read(record, "_ { iref: %d }", &iref_number);
    *retiref = iref_number;

    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_NAMESERVICE_INVALID_NAME);
    }

    return SYS_ERR_OK;
}

errval_t nameservice_blocking_lookup(const char *iface, iref_t *retiref)
{
    errval_t err;
    struct dist2_rpc_client *r = get_dist_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    char* record = NULL;
    errval_t error_code;
    err = r->vtbl.wait_for(r, iface, &record, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    if (err_is_ok(err)) {
        assert(record != NULL);
        // XXX: numbers from records are 64bit, irefs are 32
        uint64_t iref_number = 0;
        if (retiref != NULL) {
            err = dist_read(record, "_ { iref: %d }", &iref_number);
            *retiref = (iref_t) iref_number;
        }
    }

    return err;
}

errval_t nameservice_register(const char *iface, iref_t iref)
{
    return dist_set("%s { iref: %d }", iface, iref);
}
#endif
