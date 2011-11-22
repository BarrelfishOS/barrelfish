#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/skb_events_defs.h>
#include <if/skb_rpcclient_defs.h>

#include <dist2/dist2.h>


static struct skb_events_state {
	struct skb_events_binding* binding;
	bool request_done;
} skb_events_state;


struct skb_events_rx_vtbl rx_vtbl = {
		.identify = NULL,
		.subscribed_message = subscribed_message_handler,
};


static struct skb_events_state* get_skb_events_binding(void)
{
	return &skb_events_state;
}


static void bind_cb(void *st, errval_t err, struct skb_events_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }

    struct skb_state* skb_rpc = get_skb_state();
    struct skb_events_state* skb_events = get_skb_events_binding();
    skb_events->binding = b;

    uint64_t id = 0;
    errval_t err2 = skb_rpc->skb->vtbl.get_identifier(skb_rpc->skb, &id);
    assert(err_is_ok(err2));

    // Send SKB Pointer from both bindings to ensure proper identification
    err2 = skb_events->binding->tx_vtbl.identify(
    		                 skb_events->binding, NOP_CONT,
    		                 id);
    assert(err_is_ok(err2));
    // Should not fail since this is the only message
    // we ever send on this interface

    // avoid race condition and wait until skb_events identify is handled
    err2 = skb_rpc->skb->vtbl.identify(skb_rpc->skb, id);
    assert(err_is_ok(err2));

    skb_events->binding->rx_vtbl = rx_vtbl;
    assert(!skb_events->request_done);
    skb_events->request_done = true;
}


errval_t dist_init(void)
{
    errval_t err;
    iref_t iref;
    struct skb_events_state* events_state = get_skb_events_binding();

    err = nameservice_blocking_lookup("skb_events", &iref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_REFERENCE);
    }

    events_state->request_done = false;
    err = skb_events_bind(iref, bind_cb, NULL, get_default_waitset(),
                          IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    /* XXX: wait for connection to complete */
    while (!events_state->request_done) {
        messages_wait_and_handle_next();
    }

    return err;
}
