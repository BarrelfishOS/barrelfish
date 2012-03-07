


struct revoke_st {
    struct capref capref, delcap;
    revoke_result_handler_t result_handler;
    void *st;
};

/*
 * Request revoke from owner
 */

struct request_revoke_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    struct intermon_caprep_t caprep;
    struct revoke_st *st;
}

static void
request_revoke_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct request_revoke_msg_st *msg_st = (struct request_revoke_msg_st*)e;
    errval_t err;
    err = intermon_request_revoke__tx(b, NOP_CONT, msg_st->caprep, msg_st->st);
    if (err_is_fail(err)) {
        struct revoke_st *rst = msg_st->st;
        rst->result_handler(err, r->st);
        free(rst);
    }
    free(msg_st);
}

static errval_t
request_revoke(struct capref capref, revoke_result_handler_t result_handler, revoke_st *st)
{
    struct capability cap;
    err = monitor_identify_cap(capref, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct request_revoke_msg_st *msg_st = malloc(sizeof(struct request_revoke_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = request_revoke_send_cont;
    capability_to_caprep(&cap, &msg_st->caprep);
    msg_st->st = st;

    err = enqueue_send_owner(capref, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}

static void
request_revoke_move_result(errval_t status, void *st)
{
    errval_t err;
    struct revoke_st *rst = (struct revoke_st*)st;

    if (err_is_ok(status)) {
        err = revoke_local(rst);
    }
    else {
        err = status;
    }

    if (err_is_fail(err)) {
        rst->result_handler(err, rst->st);
        free(rst);
    }
}

static void
request_revoke__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    struct capref capref;
    err = copy_if_exists(&cap, &capref);
    if (err_is_fail(err)) {
        goto send_err;
    }

    capstate_t state;
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        goto send_err;
    }

    if (cap_state_is_busy(state)) {
        err = CAP_ERR_BUSY;
        goto send_err;
    }

send_err:
}

/*
 * Local revocation handling
 */

static void
revoke_delete_result(errval_t status, void *st)
{
    errval_t err;
    struct revoke_st *rst = (struct revoke_st*)st;

    if (err_is_fail(status)) {
        rst->result_handler(status, rst->st);
        free(rst);
    }

    err = monitor_revoke(capref, rst->delcap);
    if (err_no(err) == CAP_ERR_LASTLOCAL) {
        err = delete(rst->delcap, revoke_delete_result, rst);
        if (err_is_ok(err)) {
            return;
        }
    }
    else {
        slot_free(rst->delcap);
    }

    rst->result_handler(err, rst->st);
    free(rst);
}

static errval_t
revoke_local(struct revoke_st *rst)
{
    err = slot_alloc(&rst->delcap);
    if (err_is_fail(err)) {
        free(rst);
        return err;
    }

    err = monitor_revoke(rst->capref, rst->delcap);
    if (err_is_ok(err)) {
        result_handler(err, st);
    }
    else if (err_no(err) == CAP_ERR_LASTLOCAL) {
        // kernel encountered a local cap with no copies, explicitly perform a
        // delete in the monitor to deal with possible remote copies
        err = delete(rst->delcap, revoke_delete_result, rst);
        if (err_is_ok(err)) {
            return err;
        }
    }

    slot_free(rst->delcap);
    free(rst);

    return err;
}

/*
 * Revoke operation
 */

errval_t
revoke(struct capref capref, revoke_result_handler_t result_handler, void *st)
{
    errval_t err;
    capstate_t state;

    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        return err;
    }

    if (!cap_state_is_valid(state)) {
        return CAP_ERR_BUSY;
    }

    err = cap_set_busy(capref);
    if (err_is_fail(err)) {
        return err;
    }

    struct revoke_st *rst = malloc(sizeof(struct revoke_st));
    if (!rst) {
        err = LIB_ERR_MALLOC_FAIL;
        goto ready_cap;
    }
    rst->capref = capref;
    rst->result_handler = result_handler;
    rst->st = st;

    if (cap_is_owner(state)) {
        err = revoke_local(capref, result_handler, rst);
    }
    else {
        err = request_revoke(capref, result_handler, rst);
    }

ready_cap:
    if (err_is_fail(err)) {
        errval_t err2 = cap_set_ready(capref);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err, "failed to set cap to ready for cleanup");
        }
    }

    return err;
}
