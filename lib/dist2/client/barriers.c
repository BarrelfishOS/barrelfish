#include <dist2/barrier.h>
#include <dist2/getset.h>

#include "common.h"


/**
 *
 * a)
 *  dist_set(sequential, barrier.name.x { barrier: name })
 *
 *  dist_get_names(barrier.name.*, &names, &len)
 *  if(len == wait_for) {
 *  	dist_set(barrier.name);
 *  }
 *  else {
 *  	dist_exists(barrier.name);
 *  }
 *
 *  return;
 *
 * a)
 *  dist_set(sequential, barrier.name.x { barrier: name })
 *  dist_wait_for("len(barrier.name.* { barrier: name }) == wait_for)
 *  Pro: wake up only once
 *  Bad: - additional stuff to parse
 *       - lots of extra syntax only for barrier?
 *
 * b)
 *  new_barrier_added() {
 *      trigger_done = true;
 *  }
 *
 *  dist_set(sequential, barrier.name.x { barrier: name })
 *  while(1) {
 *      trigger_done = false;
 *      struct trigger t = { .mode = ON_SET, .callback = new_barrier_added(); }
 *      dist_get_names(barrier.name.* {}, &names, &size, t)
 *      if(size == wait_for)
 *          return;
 *      else
 *          while(!trigger_done) {
 *              message_wait_and_handle_next();
 *          }
 *  }
 *
 *  Pro: - Implementation easy
 *  Bad: - message_wait_and_handle_next() very bad
 *
 * c) Use THC:
 *
 * Bad: - rewrite client library
 *      - dont know what I gain
 *      - Dont know effect as a library
 *      - not much tested technology
 * Pro: - Likely cleanest solution?
 *
 **/




errval_t dist_barrier_enter(char* name, char** barrier_record, size_t wait_for) {
	errval_t err = dist_set_get(SET_SEQUENTIAL, barrier_record,
			"%s_ { barrier: '%s' }", name, name);

	char** names;
	size_t current_barriers = 0;
	err = dist_get_names(&names, &current_barriers, "%s", name);
	dist_free_names(names, current_barriers);

	if (current_barriers != wait_for) {
		err = dist_exists(true, "%s", name);
	} else {
		err = dist_set(SET_DEFAULT, "%s", name);
	}

	return err;
}

errval_t dist_barrier_leave(char* barrier_record) {
	char* rec_name = NULL;
	char* barrier_name = NULL;
	errval_t err = dist_read(barrier_record, "%s { barrier: %s }", &rec_name,
			&barrier_name);

	if (err_is_ok(err)) {
		err = dist_del(rec_name);
		char** names;
		size_t remaining_barriers;
		err = dist_get_names(names, remaining_barriers, "r'%s.*'",
				barrier_name);
		if (remaining_barriers == 0) {
			dist_del("%s", barrier_name);
		} else {
			dist_exists_not(true, "%s", barrier_name);
			// exists not does not really work here atm
			// because this will atm trigger on every del event
			// matching the query
		}
	}

	free(rec_name);
	free(barrier_name);
	return err;
}
