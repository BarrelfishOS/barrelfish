#include <barrelfish/barrelfish.h>
#include <if/intermon_defs.h>
#include "transport.h"
#include "monitor.h"
#include "magic.h"

/*
 * Messaging
 */

errval_t
intermon_enqueue_send_target(coreid_t dest, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // get destination intermon_binding and _state
    struct intermon_binding *dest_b;
    err = intermon_binding_get(dest, &dest_b);
    if (err_is_fail(err)) {
        return err;
    }
    struct intermon_state *inter_st = (struct intermon_state*)dest_b->st;

    // enqueue message
    return intermon_enqueue_send(dest_b, &inter_st->queue, dest_b->waitset, queue_elem);
}

errval_t
intermon_enqueue_send_owner(struct capref capref, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // read cap owner
    coreid_t owner;
    err = cap_get_owner(capref, &owner);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue to owner
    return intermon_enqueue_send_target(owner, queue_elem);
}

errval_t
intermon_enqueue_send_one(struct capref capref, struct msg_queue_elem *queue_elem)
{
    USER_PANIC("NYI");
}

errval_t
intermon_enqueue_send_all(struct capref capref, struct msg_queue_elem *queue_elem)
{
    USER_PANIC("NYI");
}

/*
 * Ownership update {{{1
 */

/*
 * Update owner broadcast {{{2
 */

struct update_owner_broadcast_msg_st;

struct update_owner_broadcast_st {
    intermon_caprep_t caprep;
    struct update_owner_broadcast_msg_st *msg_st_arr;
    int num_pending;
    int num_queued;
    struct event_closure completion_continuation;
    bool do_send;
};

struct update_owner_broadcast_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    struct update_owner_broadcast_st *bc_st;
};

static void
intermon_update_owner_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct update_owner_broadcast_msg_st *msg_st = (struct update_owner_broadcast_msg_st*)e;
    struct update_owner_broadcast_st *bc_st = msg_st->bc_st;

    // if do_send is false, an error occured in the broadcast setup, so do not
    // send anything
    if (bc_st->do_send) {
        err = intermon_update_owner__tx(b, NOP_CONT, bc_st->caprep, (genvaddr_t)msg_st->bc_st);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to send ownership update");
        }
    }

    // decrement counter of number of queued broadcast messages
    if (!--bc_st->num_queued) {
        // if counter is zero, cleanup outgoing memory
        free(bc_st->msg_st_arr);
        bc_st->msg_st_arr = NULL;
        if (!bc_st->do_send) {
            // if the send has been aborted, also cleanup cross-call state
            free(bc_st);
        }
    }
}

errval_t
intermon_update_owner(struct capref capref, struct event_closure completion_continuation)
{
    errval_t err;
    struct capability cap;
    err = monitor_cap_identify(capref, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct update_owner_broadcast_st *bc_st = calloc(1, sizeof(struct update_owner_broadcast_st));
    if (!bc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bc_st->num_pending = 0;
    capability_to_caprep(&cap, &bc_st->caprep);
    bc_st->completion_continuation = completion_continuation;
    bc_st->do_send = false;

    int num_current_monitors = num_monitors;
    struct update_owner_broadcast_msg_st *msg_st_arr;
    msg_st_arr = calloc(num_current_monitors, sizeof(*msg_st_arr));
    if (!msg_st_arr) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bc_st->msg_st_arr = msg_st_arr;

    for (coreid_t dest = 0; dest < MAX_COREID && bc_st->num_queued < num_current_monitors; dest++)
    {
        // get next msg_st
        struct update_owner_broadcast_msg_st *msg_st = &msg_st_arr[bc_st->num_queued];
        msg_st->queue_elem.cont = intermon_update_owner_send_cont;
        msg_st->bc_st = bc_st;

        err = intermon_enqueue_send_target(dest, (struct msg_queue_elem*)msg_st);
        if (err_no(err) == MON_ERR_NO_MONITOR_FOR_CORE) {
            // no connection for this core, skip
            continue;
        }
        else if (err_is_fail(err)) {
            // failure, disable broadcast (cleanup is done when all msg_sts have been dequeued)
            bc_st->do_send = false;
            return err;
        }
        else {
            // count the number of queued messages and number of pending responses for the broadcast
            bc_st->num_queued++;
            bc_st->num_pending++;
        }
    }

    return SYS_ERR_OK;
}

/*
 * Owner updated response {{{2
 */

struct owner_updated_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    genvaddr_t st;
};

static void
owner_updated_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct owner_updated_msg_st *msg_st = (struct owner_updated_msg_st*)e;

    err = intermon_owner_updated__tx(b, NOP_CONT, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send owner_updated message");
    }
    free(msg_st);
}

static errval_t
owner_updated(coreid_t owner, genvaddr_t st)
{
    struct owner_updated_msg_st *msg_st = calloc(1, sizeof(struct owner_updated_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = owner_updated_send_cont;
    msg_st->st = st;

    return intermon_enqueue_send_target(owner, (struct msg_queue_elem*)msg_st);
}

/*
 * Receive handlers {{{2
 */

__attribute__((unused))
static void
owner_updated__rx_handler(struct intermon_binding *b, genvaddr_t st)
{
    struct update_owner_broadcast_st *bc_st = (struct update_owner_broadcast_st*)st;

    // decrease number of pending broadcasts
    if (!--bc_st->num_pending) {
        // if counter is zero, trigger completion closure and free memory
        struct event_closure *cl = &bc_st->completion_continuation;
        cl->handler(cl->arg);
        free(bc_st);
    }
}

__attribute__((unused))
static void
update_owner__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capref capref;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    err = copy_if_exists(&cap, &capref);
    if (err_is_ok(err)) {
        err = cap_set_owner(capref, from);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to set update cap ownership");
        }
    }
    else if (err_no(err) != CAP_ERR_NOTFOUND) {
        USER_PANIC_ERR(err, "failed to lookup cap for ownership change");
    }
    cap_destroy(capref);

    err = owner_updated(from, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send ownership update response");
    }
}

