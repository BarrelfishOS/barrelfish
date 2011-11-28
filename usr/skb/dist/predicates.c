#include <stdio.h>
#include <string.h>

#include <eclipse.h>
#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>
#include <include/skb_server.h>
#include <include/skb_debug.h>
#include <include/queue.h>

#include <dist2_server/service.h>
#include "predicates.h"

static void identification_complete_reply(struct skb_binding* b, struct skb_reply_state* srs) {
    errval_t err;

    err = b->tx_vtbl.identify_response(b, MKCONT(free_reply_state, srs));
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }

}


int p_identification_complete(void)         /* identification_complete(+Integer) */
{
	long int id;
    ec_get_long(ec_arg(1), &id);

    struct skb_reply_state* srs = NULL;
    errval_t err = new_reply_state(&srs, identification_complete_reply);

    assert(err_is_ok(err)); // TODO

    struct skb_binding* skb = (struct skb_binding*) id;
    identification_complete_reply(skb, srs);

    return PSUCCEED;
}


int p_notify_client(void)         /* p_notify_client(+String, ReplyState) */
{
    struct dist_reply_state* drs;
    char* str = NULL;

    ec_get_string(ec_arg(1), &str); // TODO do we need to free this?
    ec_get_long(ec_arg(2), (long int*) &drs); // TODO conversion to pointer?
    assert(strlen(str)+1 < BUFFER_SIZE); // TODO

    strcpy(drs->skb.output_buffer, str);
    debug_printf("p_notify_client: %s\n", drs->skb.output_buffer);

    drs->error = SYS_ERR_OK;
    drs->rpc_reply(drs->binding, drs);

    return PSUCCEED;
}
