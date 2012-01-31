/**
 * \file
 * \brief Client for interacting with the name service (chips)
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/nameservice_defs.h>
#include <if/nameservice_rpcclient_defs.h>
#include <if/monitor_defs.h>

/**
 * \brief Non-blocking name service lookup
 *
 * \param iface Name of interface for which to query name server
 * \param retiref Returns pointer to IREF on success
 */
errval_t nameservice_lookup(const char *iface, iref_t *retiref)
{
    nameservice_srvref_t ref;
    errval_t err;

    struct nameservice_rpc_client *r = get_nameservice_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    err = r->vtbl.get_service_reference(r, iface, &ref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_REFERENCE);
    }

    if (ref == 0) {
        return CHIPS_ERR_UNKNOWN_NAME;
    }

    err = r->vtbl.get_service(r, ref, retiref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_IREF);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Blocking name service lookup
 *
 * \param iface Name of interface for which to query name server
 * \param retiref Returns pointer to IREF on success
 */
errval_t nameservice_blocking_lookup(const char *iface, iref_t *retiref)
{
    nameservice_srvref_t ref;
    iref_t iref;
    errval_t err;

    struct nameservice_rpc_client *r = get_nameservice_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    err = r->vtbl.wait_for_service_reference(r, iface, &ref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_REFERENCE);
    }

    if (ref == 0) {
        return CHIPS_ERR_GET_SERVICE_REFERENCE;
    }

    err = r->vtbl.get_service(r, ref, &iref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_IREF);
    }

    if (retiref != NULL) {
        *retiref = iref;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Register with name service
 *
 * \param iface Name of interface to register
 * \param iref IREF to register
 */
errval_t nameservice_register(const char *iface, iref_t iref)
{
    nameservice_srvref_t ref;

    struct nameservice_rpc_client *r = get_nameservice_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    return r->vtbl.register_service(r, iref, iface, &ref);
}

/**
 * \brief Get a capability from the capability store.
 *
 * \param key           String that identifies the capability
 * \param retcap        Pointer to structure holding capability
 */
#include <stdio.h>
errval_t nameservice_get_capability(const char *key, struct capref *retcap)
{
    errval_t reterr;
    struct nameservice_rpc_client *r = get_nameservice_rpc_client();
    if (r == NULL) {
	printf("nameservice not found\n");
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }
    printf("get cap %s\n", key);
    errval_t err = r->vtbl.get_cap(r, key, retcap, &reterr);
    if(err_is_fail(err)) {
	printf("ERROR!\n");
        return err_push(err, CHIPS_ERR_GET_CAP);
    }
    printf("nameservice_get_capability: about to return\n");
    return reterr;
}

/**
 * \brief Put a capability to the capability store.
 *
 * \param key           String that identifies the capability
 * \param cap           The capability to store
 */
errval_t nameservice_put_capability(const char *key, struct capref cap)
{
    errval_t reterr;
    struct nameservice_rpc_client *r = get_nameservice_rpc_client();
    if (r == NULL) {
        return LIB_ERR_NAMESERVICE_NOT_BOUND;
    }

    errval_t err = r->vtbl.put_cap(r, key, cap, &reterr);
    if(err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_PUT_CAP);
    }

    return reterr;
}

/* ----------------------- BIND/INIT CODE FOLLOWS ----------------------- */


static void error_handler(struct nameservice_binding *b, errval_t err)
{
    USER_PANIC_ERR(err, "asynchronous error in nameservice binding");
}

struct bind_state {
    bool done;
    errval_t err;
};

static void bind_continuation(void *st_arg, errval_t err,
                              struct nameservice_binding *b)
{
    struct bind_state *st = st_arg;

    if (err_is_ok(err)) {
        b->error_handler = error_handler;

        struct nameservice_rpc_client *r;
        r = malloc(sizeof(struct nameservice_rpc_client));
        assert(r != NULL);
        err = nameservice_rpc_client_init(r, b);
        if (err_is_fail(err)) {
            free(r);
            USER_PANIC_ERR(err, "error in nameservice_rpc_client_init");
        } else {
            set_nameservice_rpc_client(r);
        }
    }

    st->err = err;
    st->done = true;
}

static void get_name_iref_reply(struct monitor_binding *mb, iref_t iref,
                                uintptr_t st_arg)
{
    struct bind_state *st = (void *)st_arg;
    errval_t err;

    if (iref == 0) {
        err = LIB_ERR_GET_NAME_IREF;
    } else {
        err = nameservice_bind(iref, bind_continuation, st,
                               get_default_waitset(), IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    }

    if (err_is_fail(err)) {
        st->err = err;
        st->done = true;
    }
}

/**
 * \brief Blocking bind to the name service
 *
 * Should be called once only at init time on each dispatcher.
 */
errval_t nameservice_client_blocking_bind(void)
{
    errval_t err;

    struct bind_state st = { .done = false };

    /* fire off a request for the iref for the name service */
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.get_name_iref_reply = get_name_iref_reply;
    err = mb->tx_vtbl.get_name_iref_request(mb, NOP_CONT, (uintptr_t)&st);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_GET_NAME_IREF);
    }

    /* block on the default waitset until we're bound */
    struct waitset *ws = get_default_waitset();
    while (!st.done) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_EVENT_DISPATCH);
        }
    }

    return st.err;
}
