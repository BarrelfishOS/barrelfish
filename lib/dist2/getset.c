#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include <skb/skb.h>
#include <if/skb_rpcclient_defs.h>
#include <dist2/getset.h>

#include "common.h"




/**
 * nice to have would be:
 * get("object { weight: %d }", &weight);
 *
 */

/*
static errval_t dist_del(char* name)
{

}
*/

/**
 * gets all objects satisfying query
 */
errval_t dist_get_all(char* query, char** data)
{
	assert(!"TODO");
	return SYS_ERR_OK;
}


/**
 * Gets one (the first?) found
 * returns error if ambigous query
 */
errval_t dist_get(char* query, char** data)
{
	assert(query != NULL);

	char* error = NULL;
	errval_t error_code;
	errval_t err = SYS_ERR_OK;

	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	err = skb_state->skb->vtbl.get(skb_state->skb, query, data, &error, &error_code);
	if(err_is_ok(err)) {
		err = error_code;
	}

	debug_printf("dist_get: skberror %lu: %s\n", error_code, error);
	debug_printf("dist_get: data: %s\n", *data);

	free(error); // TODO can this be NULL?
	// free(*data) on error? can be NULL?

	return err;
}


/**
 * sets one object
 */
errval_t dist_set(char* object, ...)
{
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;
	va_list  args;

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

	// Send to SKB
	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);
	char* error = NULL;
	errval_t error_code;
	err = skb_state->skb->vtbl.set(skb_state->skb, buf, &error, &error_code);
	// TODO check error_code

	debug_printf("dist_set: skberror %lu: %s\n", error_code, error);

	free(buf);
	free(error);
	return err;
}


errval_t dist_del(char* object, ...)
{
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;
	va_list  args;

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
	errval_t error_code;
	err = skb_state->skb->vtbl.del(skb_state->skb, buf, &error_code);

	assert(err_is_ok(err) && err_is_ok(error_code)); // TODO check error_code

	free(buf);
	return err;
}
