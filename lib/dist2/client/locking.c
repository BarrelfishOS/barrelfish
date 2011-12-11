#include <barrelfish/barrelfish.h>

#include <dist2/lock.h>
#include <dist2/getset.h>

#include "common.h"

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
			err = dist_exists_not(true, "%s { lock: '%s' }", names[i - 1],
					lock_name);
			assert(err_is_ok(err));
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
