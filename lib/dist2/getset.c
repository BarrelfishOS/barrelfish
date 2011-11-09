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
	int error_code;
	errval_t err = SYS_ERR_OK;

	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);
	err = skb_state->skb->vtbl.get(skb_state->skb, query, data, &error, &error_code);
	// TODO check error_code

	debug_printf("dist_get: skberror %d: %s\n", error_code, error);
	debug_printf("dist_get: data: %s\n", *data);


	free(error);
	// free(*data) on error

	return err;
}


/**
 * sets one object
 */
errval_t dist_set(char* object, ...)
{
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;
	char* buf;

	va_list  args;
	va_start(args, object);
	err = format_object(&buf, object, args);
	assert(err_is_ok(err));
	va_end(args);

	// Send to SKB
	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);
	char* error = NULL;
	int error_code;
	err = skb_state->skb->vtbl.set(skb_state->skb, buf, &error, &error_code);
	// TODO check error_code

	debug_printf("dist_set: skberror %d: %s\n", error_code, error);

	free(buf);
	free(error);
	return err;
}
