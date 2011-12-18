#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>

#include <dist2/lock.h>
#include <dist2/getset.h>

#include "common.h"
#include "trigger.h"


static void trigger_lock_deleted(char* object, void* st)
{
    struct thread_sem* ts = (struct thread_sem*) st;
    debug_printf("object: %s has been deleted, send singal!\n", object);
    thread_sem_post(ts);
    free(object);
}

errval_t dist_lock(char* lock_name, char** lock_record)
{
	assert(lock_name != NULL);
	errval_t err = SYS_ERR_OK;
	char* name = NULL;

	err = dist_set_get(SET_SEQUENTIAL, lock_record, "%s_ { lock: '%s' }",
			lock_name, lock_name);
	assert(err_is_ok(err));
	debug_printf("set lock: %s\n", *lock_record);

	err = dist_read(*lock_record, "%s", &name);
	assert(err_is_ok(err));
	assert(name != NULL);

	char** names = NULL;
	size_t len = 0;

	while (true) {
		err = dist_get_names(&names, &len, "_ { lock: '%s' }", lock_name);
		assert(err_is_ok(err));

		debug_printf("lock queue:\n");
		size_t i = 0;
		bool found = false;
		for (; i < len; i++) {
			debug_printf("%s\n", names[i]);
			if (strcmp(names[i], name) == 0) {
				found = true;
				break;
			}
		}
		assert(found);

		if (i == 0) {
			// We are the lock owner
			dist_free_names(names, len);
			break;
		} else {
		    struct dist2_rpc_client* cl = get_dist_rpc_client();

		    struct thread_sem ts;
		    thread_sem_init(&ts, 0);

		    dist2_trigger_t t = { .in_case = SYS_ERR_OK,
		                          .m = DIST_ON_DEL,
		                          .trigger = (uint64_t) trigger_lock_deleted,
		                          .st = (uint64_t) &ts };
		    errval_t exist_err;
		    err = cl->vtbl.exists(cl, names[i-1], t, &exist_err);
			assert(err_is_ok(err));

		    if(err_is_ok(exist_err)) {
		        thread_sem_wait(&ts);
		    }
		}

		dist_free_names(names, len);
	}

	//debug_printf("id:%d locked: %s\n", id, *lock_record);

	free(name);
	return err;
}

errval_t dist_unlock(char* lock_record)
{
	assert(lock_record != NULL);
	errval_t err = SYS_ERR_OK;

	char* name;
	err = dist_read(lock_record, "%s", &name);
	assert(err_is_ok(err));
	err = dist_del(name);

	//debug_printf("id:%d unlocking: %s\n", id, name);

	free(name);
	return err;
}
