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

void subscribed_message_handler(struct skb_events_binding* b,
		                        subscription_t id, char* object)
{
	assert(subscriber_table[id] != NULL);

	subscriber_table[id](id, object);
}


errval_t dist_subscribe(subscription_handler_fn function, subscription_t* id, char* object, ...)
{
	assert(function != NULL);
	assert(object != NULL);
	assert(id != NULL);
	va_list  args;
	errval_t err = SYS_ERR_OK;

	err = get_free_slot(id);
	if(err_is_fail(err)) {
		return err;
	}

	size_t length = 0;
	char* buf = NULL;
	va_start(args, object);
	err = allocate_string(object, args, &length, &buf);
	va_end(args);
	if(err_is_fail(err)) {
		return err;
	}

	va_start(args, object);
	size_t bytes_written = vsnprintf(buf, length+1, object, args);
	va_end(args);
	assert(bytes_written == length);

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
	assert(id < MAX_SUBSCRIPTIONS);

	// send to skb
	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	errval_t error_code = SYS_ERR_OK;
	errval_t err = skb_state->skb->vtbl.unsubscribe(skb_state->skb, id, &error_code);

	if(err_is_ok(err)) { // TODO check error_code
		subscriber_table[id] = NULL;
	}

	return err;
}


errval_t dist_publish(char* object, ...)
{
	assert(object != NULL);

	va_list  args;
	errval_t err = SYS_ERR_OK;

	size_t length = 0;
	char* buf = NULL;
	va_start(args, object);
	err = allocate_string(object, args, &length, &buf);
	va_end(args);
	if(err_is_fail(err)) {
		return err;
	}

	va_start(args, object);
	size_t bytes_written = vsnprintf(buf, length+1, object, args);
	va_end(args);
	assert(bytes_written == length);


	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	errval_t error_code = SYS_ERR_OK;
	err = skb_state->skb->vtbl.publish(skb_state->skb, buf, &error_code);
	// TODO check error_code
	debug_printf("dist_publish completed!\n");

	free(buf);
	return err;
}

