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

    err = dist_set_get(SET_SEQUENTIAL, lock_record, "%s_ { lock: '%s' }", ressource, ressource);
    assert(err_is_ok(err));
    debug_printf("id:%d dist_lock: %s\n", id, *lock_record);

    err = dist_read(*lock_record, "%s", &name);
    assert(err_is_ok(err));

    char** names = NULL;
    size_t len = 0;

    while(true) {
        err = dist_get_names(&names, &len, "_ { lock: '%s' }", ressource);
        assert(err_is_ok(err));

        size_t i = 0;
        bool found = false;
        for(; i < len; i++) {
            //printf("names[%lu] = %s\n", i, names[i]);
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
            printf("id:%d before exists not: %s\n", id, names[i-1]);
            err = dist_exists_not(true, "%s { lock: '%s' }", names[i-1], ressource);
            assert(err_is_ok(err));
        }

        dist_free_names(names, len);
    }

    free(name);
	return err;
}


errval_t dist_unlock(char* lock_record)
{
	assert(lock_record != NULL);
	errval_t err = SYS_ERR_OK;

	err = dist_del(lock_record);

	return err;
}


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

