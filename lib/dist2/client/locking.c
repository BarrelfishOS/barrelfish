#include <barrelfish/barrelfish.h>

#include <dist2/lock.h>
#include <dist2/getset.h>

#include "common.h"

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

extern int id;

errval_t dist_lock(char* ressource, char** lock_record)
{
	assert(ressource != NULL);
	errval_t err = SYS_ERR_OK;
	char* name = NULL;

    err = dist_set_get(SET_SEQUENTIAL, lock_record, "%s_ { lock: '%s' }", ressource, ressource, id);
    assert(err_is_ok(err));
    debug_printf("id:%d set lock: %s\n", id, *lock_record);

    err = dist_read(*lock_record, "%s", &name);
    assert(err_is_ok(err));
    assert(name != NULL);

    char** names = NULL;
    size_t len = 0;

    while(true) {
        err = dist_get_names(&names, &len, "_ { lock: '%s' }", ressource, id);
        assert(err_is_ok(err));

        debug_printf("id:%d lock queue:\n", id);
        size_t i = 0;
        bool found = false;
        for(; i < len; i++) {
            debug_printf("id:%d %s\n", id, names[i]);
            if(strcmp(names[i], name) == 0) {
                found = true;
                break;
            }
        }
        assert(found);

        if(i == 0) {
            // We are the lock owner
            break;
        }
        else {
            debug_printf("id:%d waiting for deletion: %s\n", id, names[i-1]);
            err = dist_exists_not(true, "%s { lock: '%s' }", names[i-1], ressource, id);
            assert(err_is_ok(err));
        }

        dist_free_names(names, len);
    }

    debug_printf("id:%d locked: %s\n", id, *lock_record);

    free(name);
	return err;
}


errval_t dist_unlock(char* lock_record)
{
	assert(lock_record != NULL);
	errval_t err = SYS_ERR_OK;



	char* name;
    err = dist_read(lock_record, "%s", &name);
    debug_printf("id:%d unlocking: %s\n", id, name);
    assert(err_is_ok(err));

	err = dist_del(name);

	free(name);
	return err;
}


/*
errval_t dist_lock2(char* object)
{
    assert(object != NULL);
    errval_t err = SYS_ERR_OK;

    struct dist_rpc_client* rpc_client = get_dist_rpc_client();


    errval_t error_code = SYS_ERR_OK;
    err = rpc_client->vtbl.lock(rpc_client, object, &error_code);

    assert(err_is_ok(err) && err_is_ok(error_code)); // TODO

    return error_code;
}


errval_t dist_unlock(char* object)
{
    assert(object != NULL);
    errval_t err = SYS_ERR_OK;

    struct dist_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code = SYS_ERR_OK;
    err = rpc_client->vtbl.unlock(rpc_client, object, &error_code);

    assert(err_is_ok(err) && err_is_ok(error_code)); // TODO

    return err;
}
*/


/*
errval_t dist_lock_async(char* object, lock_handler_fn lock_function)
{
    return SYS_ERR_OK;
}


// TODO just use dist_unlock()?
errval_t dist_unlock_nb()
{
}

*/

