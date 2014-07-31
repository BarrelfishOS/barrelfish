/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <if/octopus_defs.h>
#include <if/octopus_rpcclient_defs.h>
#include <if/monitor_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER

#include "xeon_phi_internal.h"
#include "domain.h"

/**
 * \brief Non-blocking name service lookup
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t domain_lookup(const char *iface,
                       xphi_dom_id_t *retdomid)
{
    errval_t err;

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    char* record = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->vtbl.get(r, iface, NOP_TRIGGER
    ,
                      &record, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;
    if (err_is_fail(err)) {
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            err = err_push(err, LIB_ERR_NAMESERVICE_UNKNOWN_NAME);
        }
        goto out;
    }

    xphi_dom_id_t domid = 0;
    err = oct_read(record, "_ { domid: %d }", &domid);
    if (err_is_fail(err) || domid == 0) {
        err = err_push(err, LIB_ERR_NAMESERVICE_INVALID_NAME);
        goto out;
    }

    if (retdomid != NULL) {
        *retdomid = domid;
    }

    out: free(record);
    return err;
}

/**
 * \brief looks up the name and registers a callback
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t domain_wait(const char *iface,
                     struct xnode *node,
                     void *state)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief Register with name service
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t domain_register(const char *iface,
                         xphi_dom_id_t domid)
{
    errval_t err = SYS_ERR_OK;

    struct octopus_rpc_client *r = get_octopus_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    // Format record
    static const char* format = "%s { domid: %"PRIu64" }";
    size_t len = snprintf(NULL, 0, format, iface, domid);
    char* record = malloc(len + 1);
    if (record == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    snprintf(record, len + 1, format, iface, domid);

    debug_printf("record: [[%s]]\n", record);

    char* ret = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->vtbl.set(r, record, 0, NOP_TRIGGER
    ,
                      0, &ret, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

    out: free(record);
    return err;
}

