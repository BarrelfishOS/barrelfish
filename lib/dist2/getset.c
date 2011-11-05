#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include <skb/skb.h>
#include <if/skb_defs.h>
#include <if/skb_rpcclient_defs.h>
#include <dist2/getset.h>


#define MAX_OBJECT_LENGTH (5*1024)


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
	va_list  args;
	errval_t err = SYS_ERR_OK;

	// Construct query
	char* buf = malloc(MAX_OBJECT_LENGTH);
	assert(buf != NULL); // TODO error
	va_start(args, object);
	size_t bytes_written = vsnprintf(buf, MAX_OBJECT_LENGTH, object, args);
	va_end(args);

	if(bytes_written >= MAX_OBJECT_LENGTH) {
		// TODO return error!
		assert(!"Object string too big, return error!");
	}

	// Send to SKB
	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);
	char* error = NULL;
	int error_code;
	err = skb_state->skb->vtbl.set(skb_state->skb, buf, &error, &error_code);

	debug_printf("dist_set: skberror %d: %s\n", error_code, error);

	free(buf);
	free(error);
	return err;
}
