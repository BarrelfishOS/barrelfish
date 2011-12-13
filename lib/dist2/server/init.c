#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/dist2_defs.h>
#include <if/dist_event_defs.h>

#include <dist2_server/init.h>
#include <dist2_server/service.h>

#define DIST2_EVENT_SERVICE_NAME "dist2_events"
#define DIST2_RPC_SERVICE_NAME "dist2_rpc"


static struct export_state {
    bool is_done;
    errval_t err;
} rpc_export, event_export;


struct dist2_rx_vtbl rpc_rx_vtbl = {
    .get_names_call = get_names_handler,
    .get_call = get_handler,
    .set_call = set_handler,
    .del_call = del_handler,
    .exists_call = exists_handler,
    .exists_not_call = exists_not_handler,
    .watch_call = watch_handler,
    .subscribe_call = subscribe_handler,
    .publish_call = publish_handler,
    .identify_call = identify_rpc_binding,
    .get_identifier_call = get_identifier,
    .unsubscribe_call = unsubscribe_handler,
    //.lock_call = lock_handler,
    //.unlock_call = unlock_handler
};


struct dist_event_rx_vtbl event_rx_vtbl = {
        .identify = identify_events_binding,
};


static void events_export_cb(void *st, errval_t err, iref_t iref)
{
    event_export.is_done = true;
    event_export.err = err;

    if(err_is_ok(err)) {
        // register this iref with the name service
        event_export.err = nameservice_register(DIST2_EVENT_SERVICE_NAME, iref);
    }
}


static errval_t events_connect_cb(void *st, struct dist_event_binding *b)
{
    // Set up continuation queue
    b->st = NULL;

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = event_rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}


errval_t event_server_init(void)
{
    event_export.err = SYS_ERR_OK;
    event_export.is_done = false;

    errval_t err = dist2_export(&event_export, events_export_cb, events_connect_cb,
                            get_default_waitset(),
                            IDC_EXPORT_FLAGS_DEFAULT);
    if(err_is_fail(err)) {
        return err;
    }

    while(!event_export.is_done) {
        messages_wait_and_handle_next();
    }

    return event_export.err;
}


static void rpc_export_cb(void *st, errval_t err, iref_t iref)
{
    rpc_export.is_done = true;
    rpc_export.err = err;

    if(err_is_ok(err)) {
        // register this iref with the name service
        rpc_export.err = nameservice_register(DIST2_RPC_SERVICE_NAME, iref);
    }
}


static errval_t rpc_connect_cb(void *st, struct dist2_binding *b)
{
    // Set up continuation queue
    b->st = NULL;

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rpc_rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}


errval_t rpc_server_init(void)
{
    rpc_export.err = SYS_ERR_OK;
    rpc_export.is_done = false;

    errval_t err = dist2_export(&rpc_export, rpc_export_cb, rpc_connect_cb,
                            get_default_waitset(),
                            IDC_EXPORT_FLAGS_DEFAULT);

    if(err_is_fail(err)) {
        return err;
    }

    while(!rpc_export.is_done) {
        messages_wait_and_handle_next();
    }

    return rpc_export.err;
}


errval_t dist_server_init(void)
{
    errval_t err = event_server_init();
    if(err_is_fail(err)) {
        return err;
    }

    err = rpc_server_init();

    return err;
}
