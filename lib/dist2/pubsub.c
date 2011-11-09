#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <dist2/pubsub.h>
#include <if/skb_rpcclient_defs.h>

#include "common.h"

static subscription_handler_fn subscriber_table[MAX_SUBSCRIPTIONS] = { NULL };

static errval_t get_free_slot(subscription_t* slot)
{
	assert(slot != NULL);

	subscription_t idx = 0;
	for(; idx < MAX_SUBSCRIPTIONS; idx++) {
		if(subscriber_table[idx] == NULL) {
			*slot = idx;
			return SYS_ERR_OK;
		}
	}

	return SYS_ERR_BMP_INVALID; // TODO proper error code
}


errval_t dist_subscribe(subscription_handler_fn function, subscription_t* id, char* query, ...)
{
	assert(function != NULL);
	assert(query != NULL);
	assert(id != NULL);
	va_list  args;
	errval_t err = SYS_ERR_OK;
	char* buf = NULL;

	err = get_free_slot(id);
	if(err_is_fail(err)) {
		return err;
	}

	va_start(args, query);
	err = format_object(&buf, query, args);
	assert(err_is_ok(err)); // TODO
	va_end(args);

	// send to skb
	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	errval_t error_code = SYS_ERR_OK;
	err = skb_state->skb->vtbl.subscribe(skb_state->skb, buf, *id, &error_code);

	debug_printf("error code returned was: %lu\n", error_code);

	// TODO check error_code
	if(err_is_ok(err)) {
		subscriber_table[*id] = function;
	}

	free(buf);
	return err;
}


errval_t dist_unsubscribe(subscription_t id)
{


	return SYS_ERR_OK;
}


errval_t dist_publish(char* object, ...)
{
	assert(object != NULL);

	va_list  args;
	errval_t err = SYS_ERR_OK;
	char* buf = NULL;

	va_start(args, object);
	err = format_object(&buf, object, args);
	assert(err_is_ok(err)); // TODO
	va_end(args);

	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	errval_t error_code = SYS_ERR_OK;
	err = skb_state->skb->vtbl.publish(skb_state->skb, buf, &error_code);
	// TODO check error_code
	debug_printf("dist_publish completed!\n");

	free(buf);
	return err;
}

