#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <dist2/dist2.h>

#include "common.h"

static struct dist_state {
    struct dist_rpc_client* rpc_client;
    errval_t err;
    bool is_done;
} dist_connection;


static struct dist_event_state {
	struct dist_event_binding* binding;
    errval_t err;
	bool is_done;
} dist_event_connection;


struct dist_event_rx_vtbl rx_vtbl = {
		.identify = NULL,
		.subscribed_message = subscribed_message_handler,
};


struct dist_event_binding* get_dist_event_binding(void)
{
	return dist_event_connection.binding;
}


struct dist_rpc_client* get_dist_rpc_client(void)
{
    assert(dist_connection.is_done && dist_connection.rpc_client != NULL);
    return dist_connection.rpc_client;
}


static void event_bind_cb(void *st, errval_t err, struct dist_event_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "dist_event bind failed");
        goto out;
    }

    struct dist_rpc_client* dist_rpc = get_dist_rpc_client();
    uint64_t id = 0;
    dist_event_connection.binding = b;

    err = dist_rpc->vtbl.get_identifier(dist_rpc, &id);
    if(err_is_fail(err)) {
        goto out;
    }

    // Send Message from both bindings to ensure proper identification
    err = dist_event_connection.binding->tx_vtbl.identify(
                        dist_event_connection.binding, NOP_CONT, id);
    if(err_is_fail(err)) {
        goto out;
    }

    err = dist_rpc->vtbl.identify(dist_rpc, id);
    if(err_is_fail(err)) {
        goto out;
    }

    dist_event_connection.binding->rx_vtbl = rx_vtbl;

out:
    assert(!dist_event_connection.is_done);
    dist_event_connection.is_done = true;
    dist_event_connection.err = err;
}


static errval_t dist_event_binding_init(void)
{
    errval_t err = SYS_ERR_OK;
    iref_t event_iref = 0;

    err = nameservice_blocking_lookup("dist2_events", &event_iref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_REFERENCE);
    }

    dist_event_connection.is_done = false;
    err = dist_event_bind(event_iref, event_bind_cb, NULL, get_default_waitset(),
                          IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for connection to complete
    while (!dist_event_connection.is_done) {
        messages_wait_and_handle_next();
    }

    return dist_event_connection.err;
}


static void rpc_bind_cb(void *st, errval_t err, struct dist_binding* b)
{
    if (err_is_ok(err)) {
        dist_connection.rpc_client = malloc(sizeof(struct dist_rpc_client));
        assert(dist_connection.rpc_client != NULL);

        err = dist_rpc_client_init(dist_connection.rpc_client, b);
        if (err_is_fail(err)) {
            free(dist_connection.rpc_client);
        }
    } // else: Do nothing

    assert(!dist_connection.is_done);
    dist_connection.is_done = true;
    dist_connection.err = err;
}


static errval_t dist_binding_init(void)
{
    errval_t err = SYS_ERR_OK;
    iref_t rpc_iref = 0;

    err = nameservice_blocking_lookup("dist2_rpc", &rpc_iref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_REFERENCE);
    }

    dist_connection.is_done = false;
    err = dist_bind(rpc_iref, rpc_bind_cb, NULL, get_default_waitset(),
                    IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for connection to complete
    while (dist_connection.is_done) {
        messages_wait_and_handle_next();
    }

    return dist_connection.err;
}


errval_t dist_init(void)
{
    errval_t err = SYS_ERR_OK;

    err = dist_binding_init();
    if(err_is_ok(err)) {
        err = dist_event_binding_init();
    }

    return err;
}
