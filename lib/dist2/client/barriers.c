#include <dist2/barrier.h>
#include <dist2/getset.h>

#include "common.h"

errval_t dist_barrier_enter(char* name, char** barrier_record, size_t wait_for)
{
    errval_t err = dist_set_get(SET_SEQUENTIAL, barrier_record,
                                "%s_ { barrier: '%s' }", name, name);
    if(err_is_ok(err)) {
        err = dist_exists(true, barrier_record, "%s_%lu { barrier: '%s' }",
                          name, wait_for, name);
    }

    return err;
}

errval_t dist_barrier_leave(char* barrier_record)
{
    char* rec_name = NULL;
    char* barrier_name = NULL;
    errval_t err = dist_read(barrier_record, "%s { barrier: %s }",
                             &rec_name, &barrier_name);

    if (err_is_ok(err)) {
        err = dist_del(rec_name);
        if(err_is_ok(err)) {
            err = dist_exists_not(true, "%s_0", barrier_name);
        }
    }

    free(rec_name);
    free(barrier_name);
    return err;
}
