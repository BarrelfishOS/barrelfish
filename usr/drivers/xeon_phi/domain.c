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
#include <if/octopus_thc.h>

#include <octopus/octopus.h>
#include <octopus/trigger.h>

#include "xeon_phi_internal.h"
#include "domain.h"
#include "interphi.h"

struct wait_state
{
    void *usr_state;
    struct xnode *node;
    octopus_trigger_id_t tid;
};

static void domain_wait_trigger_handler(octopus_mode_t mode,
                                        const char* record,
                                        void* state)
{
    errval_t err;

    struct wait_state *ws = state;

    oct_remove_trigger(ws->tid);

    xphi_dom_id_t domid = 0;
    err = oct_read(record, "_ { domid: %d }", &domid);
    if (err_is_fail(err) || domid == 0) {
        err = err_push(err, XEON_PHI_ERR_CLIENT_DOMAIN_VOID);
    }

    interphi_domain_wait_reply(ws->node, err, ws->usr_state, domid);

    free(state);
}

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

    struct octopus_binding *r = get_octopus_binding();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    struct octopus_get_response__rx_args reply;
    err = r->rpc_tx_vtbl.get(r, iface, NOP_TRIGGER, reply.output, &reply.tid,
                      &reply.error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = reply.error_code;
    if (err_is_fail(err)) {
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            err = err_push(err, XEON_PHI_ERR_CLIENT_DOMAIN_VOID);
        }
        goto out;
    }

    xphi_dom_id_t domid = 0;
    err = oct_read(reply.output, "_ { domid: %d }", &domid);
    if (err_is_fail(err) || domid == 0) {
        err = err_push(err, XEON_PHI_ERR_CLIENT_DOMAIN_VOID);
        goto out;
    }

    if (retdomid != NULL) {
        *retdomid = domid;
    }

    out:
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
                     void *state,
                     xphi_dom_id_t *retdom)
{
    errval_t err;

    struct octopus_thc_client_binding_t* c = oct_get_thc_client();
    if (c == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    struct wait_state *ws = malloc(sizeof(*ws));
    if (ws == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    ws->usr_state = state;
    ws->node = node;

    octopus_mode_t m = OCT_ON_SET;
    octopus_trigger_t iface_set_trigger = oct_mktrigger(
                    OCT_ERR_NO_RECORD, octopus_BINDING_EVENT, m,
                    domain_wait_trigger_handler, ws);

    struct octopus_get_response__rx_args reply;

    assert(!"FIXME");
    err = c->call_seq.get(c, iface, iface_set_trigger, NULL, &ws->tid,
                      &reply.error_code);

    if (err_is_fail(err)) {
        return err;
    }

    if (err_is_fail(reply.error_code)) {
        return reply.error_code;
    }

    free(ws);

    xphi_dom_id_t domid = 0;
    err = oct_read(reply.output, "_ { domid: %d }", &domid);
    if (err_is_fail(err) || domid == 0) {
        err = err_push(err, XEON_PHI_ERR_CLIENT_DOMAIN_VOID);
        return err;
    }

    if (retdom) {
        *retdom = domid;
    }

    return err;
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

    struct octopus_binding *r = get_octopus_binding();
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

    octopus_trigger_id_t tid;
    errval_t error_code;
    err = r->rpc_tx_vtbl.set(r, record, 0, NOP_TRIGGER, 0, NULL, &tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

    out: free(record);

    return err;
}
