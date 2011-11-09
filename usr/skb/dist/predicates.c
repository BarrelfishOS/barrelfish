#include <eclipse.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>

#include "predicates.h"
#include "../queue.h"



static void id_complete_handler(struct skb_binding *b,
                                struct skb_msg_queue_elem *e);


struct identify_request_state {
    struct skb_msg_queue_elem elem;
    //struct skb_identify_response__args args;
};


static void id_complete_cont(struct skb_binding* skb_closure)
{
    errval_t err;

    err = skb_closure->tx_vtbl.identify_response(skb_closure, NOP_CONT);

    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct identify_request_state *me =
                malloc(sizeof(struct identify_request_state));
            struct skb_queue_state *ist = skb_closure->st;
            me->elem.cont = id_complete_handler;

            err = skb_enqueue_send(skb_closure, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending get_object response failed!");
    }
}


static void id_complete_handler(struct skb_binding *b,
                                struct skb_msg_queue_elem *e)
{
    /*struct identify_request_state *st =
    		(struct identify_request_state *)e;*/
    id_complete_cont(b);
    free(e);
}


int p_identification_complete(void)         /* identification_complete(+Integer) */
{

    long int id;
    ec_get_long(ec_arg(1), &id);

    struct skb_binding* skb = (struct skb_binding*) id;

    printf("p_identification_complete: %ld\n", id);
    id_complete_cont(skb);

    return PSUCCEED;
}
