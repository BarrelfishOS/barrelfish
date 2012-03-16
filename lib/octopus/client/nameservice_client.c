/**
 * \brief Nameservice Implementation
 *
 * NS Implementation using THC. Currently not included in libbarrelfish
 * due to limitations in memory allocation: THC allocates a large stack
 * and during NS initialization only allocation of one page is allowed.
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#if 0
#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>
#include <barrelfish/nameservice_client.h>

#include <dist2/init.h>
#include <dist2/trigger.h>
#include <dist2/getset.h>

errval_t nameservice_lookup(const char *iface, iref_t *retiref)
{
    errval_t err;

    char* record = NULL;
    err = dist_get(&record, iface);
    if (err_no(err) == DIST2_ERR_NO_RECORD) {
        return err_push(err, LIB_ERR_NAMESERVICE_UNKNOWN_NAME);
    }

    // XXX: numbers from records are 64bit, irefs are 32
    uint64_t iref_number = 0;
    err = dist_read(record, "_ { iref: %d }", &iref_number);
    *retiref = iref_number;

    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_NAMESERVICE_INVALID_NAME);
    }

    free(record);
    return SYS_ERR_OK;
}

errval_t nameservice_blocking_lookup(const char *iface, iref_t *retiref)
{
    errval_t err;
    errval_t error_code;
    char* record = NULL;
    dist2_mode_t mode;
    uint64_t state;
    uint64_t fn;
    uint64_t iref_number = 0;

    struct dist2_thc_client_binding_t *cl = dist_get_thc_client();
    if (cl == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    dist2_trigger_t t = dist_mktrigger(DIST2_ERR_NO_RECORD, DIST_ON_SET, 0, 0);
    err = cl->call_seq.get(cl, iface, &record, t, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    if (err_no(err) == DIST2_ERR_NO_RECORD) {
        assert(record == NULL);
        cl->recv.trigger(cl, &mode, &fn, &state, &record);
        err = SYS_ERR_OK;
    }

    if (err_is_ok(err)) {
        assert(record != NULL);
        // XXX: numbers from records are 64bit, irefs are 32
        if (retiref != NULL) {
            err = dist_read(record, "_ { iref: %d }", &iref_number);
            *retiref = (iref_t) iref_number;
        }
        free(record);
    }

    return err;
}

errval_t nameservice_register(const char *iface, iref_t iref)
{
    return dist_set("%s { iref: %d }", iface, iref);
}
#endif
