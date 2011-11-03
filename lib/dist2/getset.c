#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include <skb/skb.h>
#include <if/skb_defs.h>
#include <if/skb_rpcclient_defs.h>
#include <dist2/getset.h>


/**
 *
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
 *
 */
errval_t dist_set(char* object)
{
	assert(object != NULL);
	char* error = NULL;
	int error_code;
	errval_t err = SYS_ERR_OK;

	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);
	err = skb_state->skb->vtbl.set(skb_state->skb, object, &error, &error_code);

	debug_printf("dist_set: skberror %d: %s\n", error_code, error);


	return err;
}
