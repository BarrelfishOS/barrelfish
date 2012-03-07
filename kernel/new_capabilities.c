

struct delete_list {
    struct cte *next;
    cslot_t next_slot;
};

struct delete_list *clear_head, *clear_tail;
struct delete_list *delete_head;

errval_t
try_delete(struct cte *cte)
{
    errval_t err;

    if (is_in_delete(cte)) {
        return SYS_ERR_OK;
    }

    if (is_foreign(cte) || has_copies(cte)) {
        err = cleanup_copy(cte);
        if (err_is_fail(err)) {
            return err;
        }

        err = mdb_remove(cte);
        if (err_is_fail(err)) {
            return err;
        }

        memset(cte, 0, sizeof(*cte));
        return SYS_ERR_OK;
    }

    return CAP_ERR_LASTLOCAL;
}

errval_t
try_delete_lastlocal(struct cte *cte)
{
    errval_t err;

    err = try_delete(cte);
    if (err_no(err) != CAP_ERR_LASTLOCAL) {
        return err;
    }

    if (cte->cap.type == ObjType_CNode ||
        cte->cap.type == ObjType_Dispatcher)
    {
        cap_set_deleted(cte);
        if (clear_tail) {
            clear_tail->delete_node.next = cte;
        }
        else {
            assert(!clear_head);
            clear_head = cte;
        }
        clear_tail = cte;
        memset(&cte->delete_node, 0, sizeof(cte->delete_node));
    }
    else
    {
        err = cleanup_last(cte);
        if (err_is_fail(err)) {
            return err;
        }

        err = mdb_remove(cte);
        if (err_is_fail(err)) {
            return err;
        }

        memset(cte, 0, sizeof(*cte));
    }

    return SYS_ERR_OK;
}

errval_t
delete_next(struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;
    assert(ret_next->cap.type == ObjType_Null);

    if (!clear_head) {
        assert(!clear_tail);
        return CAP_ERR_NOTFOUND;
    }

    do {
        struct cte *current = clear_head;
        switch (current->cap.type) {
        case ObjType_CNode:
            err = cnode_delete_next(current, ret_next);
            break;
        case ObjType_Dispatcher:
            err = dispatcher_delete_next(current, ret_next);
            break;
        default:
            printk(LOG_ERR, "Unexpected cap type in delete_next");
        }

        if (err_is_ok(err)) {
            if (clear_tail == cte) {
                assert(clear_head == cte);
                clear_head = clear_tail = NULL;
            }
            else {
                clear_head = cte->delete_node.next;
            }
            memset(&cte->delete_node, 0, sizeof(cte->delete_node));

            if (current->cap.type == ObjType_CNode ||
                current->cap.type == ObjType_Dispatcher)
            {
                current->delete_node.next = delete_head;
                delete_head = current;
            }
        }
    } while (err_is_ok(err));
}

static errval_t
cnode_delete_next(struct cte *cnode, struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;
    assert(cnode->cap.type == ObjType_CNode);
    size_t size = (1<<cte->cap.u.cnode.bits);
    cslot_t index = cnode->delete_node.next_slot;
    assert(index < size);

    for (; err_is_ok(err) && index < size; index++) {
        struct cte *current = caps_locate_slot(cnode->cap.u.cnode.cnode, index);
        if (current->cap.type == ObjType_Null) {
            continue;
        }

        err = try_delete(current);
        if (err_no(err) == CAP_ERR_LASTLOCAL) {
            set_cap(ret_next, current);
            mdb_insert(ret_next);
            cleanup_copy(current);
            mdb_remove(current);
            memset(current, 0, sizeof(*current));
        }
    }

    return err;
}

static errval_t
dispatcher_delete_next(struct cte *dispatcher, struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;
    assert(dispatcher->cap.type == ObjType_Dispatcher);
    cslot_t index = cnode->delete_node.next_slot;
    struct dcb *dcb = cap->u.dispatcher.dcb;

    for (; err_is_ok(err); index++);
        struct cte *current;
        switch (index) {
        case 0:
            current = &dcb->cspace;
            break;
        case 1:
            current = &dcb->disp_cte;
            break;
        case 2:
            return SYS_ERR_OK;
        default:
            printk(LOG_ERR, "Unexpected index into dcb slots");
        }

        if (current->cap.type == ObjType_Null) {
            continue;
        }

        err = try_delete(current);
        if (err_no(err) == CAP_ERR_LASTLOCAL) {
            set_cap(ret_next, current);
            mdb_insert(ret_next);
            cleanup_copy(current);
            mdb_remove(current);
            memset(current, 0, sizeof(*current));
        }
    };

    return err;
}
