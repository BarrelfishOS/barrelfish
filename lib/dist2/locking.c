#include <barrelfish/barrelfish.h>
#include <dist2/lock.h>
#include <if/skb_rpcclient_defs.h>

/**
 * Locking using constraints?
 *
 * lock( name { physical_start: 100, physical_end: 110, physical_start: >= 100, physical_end: <= 110 }):
 * 	if not exists(lock_name) and not exists(obj with given constraints)
 * 		set(lock_name { owner: &binding, physical_start: 100, physical_end: 110 }
 * 	else:
 * 		set lock_name-{count} { physical_start: >= 100, physical_end: <= 110 }
 *
 * 	unlock(name)
 *
 * 	-----
 *
 * 	lock(name { physStart: 0x000, physEnd: 0x1000 })
 * 	is_locked( _ { physStart: >= 0x000, physEnd: <= 0x200 }) ???
 *
 * 	in_range(0x000, 0x1000): a >= 0x000 and a <= 0x1000
 *
 */
errval_t dist_lock(char* object) {
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;

	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	errval_t error_code = SYS_ERR_OK;
	err = skb_state->skb->vtbl.lock(skb_state->skb, object, &error_code);

	assert(err_is_ok(err) && err_is_ok(error_code)); // TODO

	return err;
}


errval_t dist_unlock(char* object) {
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;

	struct skb_state* skb_state  = get_skb_state();
	assert(skb_state != NULL);

	errval_t error_code = SYS_ERR_OK;
	err = skb_state->skb->vtbl.unlock(skb_state->skb, object, &error_code);

	assert(err_is_ok(err) && err_is_ok(error_code)); // TODO

	return err;
}
