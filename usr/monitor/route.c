/**
 * \file
 * \brief Handle monitors route setup
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <trace/trace.h>
#include <inttypes.h>

static routeid_t uni_routeid;  // routeid for unicast-tree broadcast route

bool done_route_op;       // used to wait for monitors join/connect to route
iref_t route_reply_iref;  // used for monitor join replies

struct lock_req_payload_t {
    struct capability cap;
    coreid_t from_core;
    bool recursive;
};

struct lock_reply_payload_t {
    coremask_t cores_locked;
    errval_t err;
    bool has_descendants;// XXX:this can be removed when rcap_central is removed
};

struct unlock_payload_t {
    struct capability cap;
    coreid_t from_core;
    bool recursive;
};

struct new_core_payload_t {
    struct capability cap;
    coreid_t send_core;
    coreid_t recv_core;
};

struct cap_details_payload_t {
    struct capability cap;
    bool has_descendants;
    coreid_t from_core;
};

struct route_payload_t {
    enum MESSAGE_ID {
        RCAP_LOCK_REQUEST,
        RCAP_LOCK_REPLY,
        RCAP_UNLOCK,
        RCAP_SEND_DETAILS,
        RCAP_REQUEST_DETAILS,
        RCAP_NEW_CORE,
        RCAP_RETYPE,
        RCAP_DELETE,
        RCAP_REVOKE
    } id;
    union {
        struct lock_req_payload_t lock_req;
        struct lock_reply_payload_t lock_reply;
        struct unlock_payload_t unlock;
        struct new_core_payload_t new_core;
        struct cap_details_payload_t cap_details;
    } p;
};

/*-------------------- Calls used by other monitor components ---------------*/


static void route_rcap_ccast_cb(void *rs, routeid_t rid, void *ccast_st,
                                uint8_t *payload_arg, size_t size)
{
   
    assert(sizeof(struct route_payload_t) == size);
    struct route_payload_t * payload = (struct route_payload_t *) payload_arg;
    assert (payload->id == RCAP_LOCK_REPLY);

    struct rcap_st * st = (struct rcap_st *) ccast_st;
    st->err          =  payload->p.lock_reply.err;
    st->cores_locked |= payload->p.lock_reply.cores_locked;

    // XXX: has_descendants piggybacked on lock reply, this can be removed when
    // rcap_central is unused
    rcap_db_remote_recv_details(&st->capability, 0, 
                                payload->p.lock_reply.has_descendants);

    assert (st->cb);
    st->cb(st);
}

errval_t route_rcap_lock_req(struct capability * cap, coremask_t send_to,
                             coreid_t from_core, struct rcap_st * st,
                             bool recursive)
{
    errval_t err;

    struct route_payload_t payload = {
        .id = RCAP_LOCK_REQUEST,
        .p.lock_req = { 
            .cap = *cap,
            .from_core = from_core,
            .recursive = recursive,
        },
    };
    if (recursive) {
        // recursive lock requests always go to all cores
        err = routing_send_bcast_with_ccast(uni_routeid, (uint8_t*)&payload, 
                                            sizeof(payload),route_rcap_ccast_cb,
                                            st);
    } else {
        err = routing_send_sel_bcast_with_ccast(uni_routeid, send_to, 
                                            (uint8_t*)&payload, sizeof(payload),
                                            route_rcap_ccast_cb, st);
    }

    return err;
}

errval_t route_rcap_unlock(struct capability * cap, coremask_t send_to,
                           coreid_t from_core, bool recursive)
{
    errval_t err;

    struct route_payload_t payload = {
        .id = RCAP_UNLOCK,
        .p.unlock = { 
            .cap = *cap,
            .from_core = from_core,
            .recursive = recursive,
        },
    };
    err = routing_send_sel_bcast(uni_routeid, send_to, 
                                 (uint8_t*)&payload, sizeof(payload));
    return err;
}

errval_t route_rcap_new_core(struct capability * cap, coremask_t send_to,
                             coreid_t send_core, coreid_t recv_core)
{
    struct route_payload_t payload = {
        .id = RCAP_NEW_CORE,
        .p.new_core = { 
            .cap = *cap,
            .send_core = send_core,
            .recv_core = recv_core,
        },
    };
    return routing_send_sel_bcast(uni_routeid, send_to,
                                  (uint8_t*)&payload, sizeof(payload));
}

errval_t route_rcap_send_details(struct capability * cap, coremask_t send_to,
                                 bool has_desc)
{
    struct route_payload_t payload = {
        .id = RCAP_SEND_DETAILS,
        .p.cap_details = { 
            .cap = *cap,
            .has_descendants = has_desc,
            .from_core = my_core_id,
        },
    };

    return routing_send_sel_bcast(uni_routeid, send_to, (uint8_t*)&payload, 
                                  sizeof(payload));
}

errval_t route_rcap_request_details(struct capability * cap, coremask_t send_to)
{
    struct route_payload_t payload = {
        .id = RCAP_REQUEST_DETAILS,
        .p.cap_details = { 
            .cap = *cap,
            .from_core = my_core_id,
        },
    };

    return routing_send_sel_bcast(uni_routeid, send_to, 
                                  (uint8_t*)&payload, sizeof(payload)); 
}

errval_t route_rcap_lock_reply(errval_t reply_err, coremask_t cores_locked,
                               bool has_descendants, recordid_t ccast_recordid)
{
    struct route_payload_t payload = {
        .id = RCAP_LOCK_REPLY,
        .p.lock_reply = { 
            .err = reply_err,
            .cores_locked = cores_locked,
            .has_descendants = has_descendants
        },
    };
    return routing_send_ccast(uni_routeid, ccast_recordid,
                              (uint8_t*)&payload, sizeof(payload));
}

errval_t route_rcap_retype(struct capability * cap, bool has_descendants, 
                           coremask_t send_to)
{
    struct route_payload_t payload = {
        .id = RCAP_RETYPE,
        .p.cap_details = { 
            .cap = *cap,
            .has_descendants = has_descendants,
            .from_core = my_core_id,
        },
    };

    return routing_send_sel_bcast(uni_routeid, send_to,
                                 (uint8_t*)&payload, sizeof(payload));
}

errval_t route_rcap_delete(struct capability * cap, coremask_t send_to)
{
    struct route_payload_t payload = {
        .id = RCAP_DELETE,
        .p.cap_details = { 
            .cap = *cap,
            .from_core = my_core_id,
        },
    };

    return routing_send_sel_bcast(uni_routeid, send_to,
                                  (uint8_t*)&payload, sizeof(payload));
}

errval_t route_rcap_revoke(struct capability * cap)
{
    struct route_payload_t payload = {
        .id = RCAP_REVOKE,
        .p.cap_details = { 
            .cap = *cap,
            .from_core = my_core_id,
        },
    };

    return routing_send_bcast(uni_routeid, (uint8_t*)&payload, sizeof(payload));
}
/*----------------------------- Message handlers ----------------------------*/

static void ucast_recv (void *rs, routeid_t id, 
                        uint8_t *payload_arg, size_t size)
{
    assert(sizeof(struct route_payload_t) == size);
    struct route_payload_t * payload = (struct route_payload_t *) payload_arg;

    switch (payload->id) {
    default:
        assert(!"monitor_route: unicast message id");
        break;
    }
}

static bool ccast_recv (void *rs, routeid_t id, void * ccast_state, 
                        coreid_t remaining, uint8_t *payload_arg, size_t size)
{
    assert(sizeof(struct route_payload_t) == size);
    struct route_payload_t * payload = (struct route_payload_t *) payload_arg;

    switch (payload->id) {
    case RCAP_LOCK_REPLY:
        assert (ccast_state);
        struct rcap_st * st = ccast_state;
        if (err_is_fail(payload->p.lock_reply.err)) {
            st->err = payload->p.lock_reply.err;
        }
        st->cores_locked |= payload->p.lock_reply.cores_locked;
        if (remaining == 0) {
            payload->p.lock_reply.cores_locked = st->cores_locked;
            payload->p.lock_reply.err = st->err;
            if (st->free_at_ccast) {
                free(st);
            }
            return true;
        } else {
            return false;
        }
    default:
        assert(!"monitor_route: Invalid ccast message id");
        return false;
    }
}

static bool bcast_recv (void *rs, routeid_t id, uint8_t *payload_arg, 
                        size_t size)
{
    errval_t err;
    assert(sizeof(struct route_payload_t) == size);
    struct route_payload_t * payload = (struct route_payload_t *) payload_arg;

    switch (payload->id) {
    case RCAP_UNLOCK:
        if (payload->p.unlock.recursive) {
            err = rcap_db_remote_recursive_unlock(&(payload->p.unlock.cap),
                                                  payload->p.unlock.from_core);
        } else {
            err = rcap_db_remote_unlock(&(payload->p.unlock.cap),
                                        payload->p.unlock.from_core);
        }
        assert(err_is_ok(err));
        return true;
    case RCAP_NEW_CORE:
        err = rcap_db_remote_new_core(&(payload->p.new_core.cap),
                                      payload->p.new_core.send_core,
                                      payload->p.new_core.recv_core);
        assert(err_is_ok(err));
        return true;
    case RCAP_REQUEST_DETAILS:
        err = rcap_db_remote_details_req(&(payload->p.cap_details.cap),
                                         payload->p.cap_details.from_core);
        assert(err_is_ok(err));
        return true;
    case RCAP_SEND_DETAILS:
        err = rcap_db_remote_recv_details(&(payload->p.cap_details.cap),
                                          payload->p.cap_details.from_core,
                                          payload->p.cap_details.has_descendants);
        assert(err_is_ok(err));
        return true;
    case RCAP_RETYPE:
        err = rcap_db_remote_retype (&(payload->p.cap_details.cap),
                                     payload->p.cap_details.has_descendants,
                                     payload->p.cap_details.from_core);
        assert(err_is_ok(err));
        return true;
    case RCAP_DELETE:
        err = rcap_db_remote_delete(&(payload->p.cap_details.cap),
                                    payload->p.cap_details.from_core);
        assert(err_is_ok(err));
        return true;
    case RCAP_REVOKE:
        err = rcap_db_remote_revoke(&(payload->p.cap_details.cap),
                                    payload->p.cap_details.from_core);
        assert(err_is_ok(err));
        return true;
    default:
        assert(!"monitor_route: Invalid bcast message id");
        return false;
    }
}

static bool bcast_with_ccast_recv (void *rs, routeid_t id, recordid_t ccast_id,
                                   void ** ccast_state, uint8_t *payload_arg, 
                                   size_t size)
{
    errval_t err;
    assert(sizeof(struct route_payload_t) == size);
    struct route_payload_t * payload = (struct route_payload_t *) payload_arg;

    switch (payload->id) {
    case RCAP_LOCK_REQUEST:
        // allocate state to agrigate result 
        // TODO, avoid this if we are a leaf on the bcast route and don't need tp aggrigate.
        *ccast_state = malloc(sizeof(struct rcap_st));
        memset(*ccast_state, 0, sizeof(struct rcap_st));
        struct rcap_st*  st = (struct rcap_st *) *ccast_state;
        st->free_at_ccast = true;

        // try to lock
        if (payload->p.lock_req.recursive) {
            err = rcap_db_remote_recursive_lock_req(&(payload->p.lock_req.cap),
                                              payload->p.lock_req.from_core,
                                              ccast_id);
        } else {
            err = rcap_db_remote_lock_req(&(payload->p.lock_req.cap),
                                          payload->p.lock_req.from_core,
                                          ccast_id);
        }
        assert(err_is_ok(err));
        return true;     // TODO - short-circuit if request rejected already
    default:
        assert(!"monitor_route: Invalid bcast_with_ccast message id");
        return false;
    }
}

static bool connect_req_cb(void *state, routeid_t routeid, coreid_t init_core, 
                            coreid_t bcast_parent, coremask_t childcores, 
                            struct neighbor ** children, coreid_t * c_count, 
                            struct neighbor ** ccast_parent) 
{ 
    if (routeid == uni_routeid) {
        // allow connection, use defaults for children (none) 
        // and ccast_parent (same as bcast parent)
        return true;
    } else {
        USER_PANIC("NYI");
        return false;
    }
}


/*------------------------ Route setup / initialization ----------------------*/

struct routing_cb_vtbl routing_vtbl = {
    .unicast_recv_cb          = ucast_recv,
    .bcast_recv_cb            = bcast_recv,
    .bcast_with_ccast_recv_cb = bcast_with_ccast_recv,
    .ccast_recv_cb            = ccast_recv,
    .neighbor_connect_req_cb  = connect_req_cb,
};

static size_t get_uni_neighbors(iref_t * irefs, size_t size, 
                                struct neighbor ** neighbors)
{
    size_t neighbors_count = 0;

    // every other monitor is my neighbor on unicast-tree route
    for (int i = 0; i < size; i++) {
        if (irefs[i] != NULL_IREF && i != my_core_id) {
            neighbors[neighbors_count++] = 
                routing_new_neighbor_iref(i, 1<<i, irefs[i]);
        }  
    }
    return neighbors_count;
}

static bool *bsp_monitors_list;
static iref_t monitor_irefs[MAX_CPUS];
static coreid_t bsp_connecting;
bool routes_up;  // extern in monitor.h

void route_done_connect(struct intermon_binding *st, routeid_t id, errval_t err)
{
    assert (err_is_ok(err));
    assert(bsp_connecting != 0);
    bsp_connecting--;
    if (bsp_connecting == 0) {
        routes_up = true;
    }
}

static void route_initialize_bsp_3(void *rs, routeid_t routeid, errval_t err)
{
    assert(err_is_ok(err));

    bsp_connecting = 0;
    // signal other monitors to connect 
    for (int i=0; i<MAX_CPUS; i++) {
        if (bsp_monitors_list[i] && i != my_core_id) {
            bsp_connecting++;

            struct intermon_binding *intern_bind = intern[i].closure;
            assert(intern_bind);
            err = intern_bind->tx_vtbl.connect_neighbors(
                              intern_bind, NOP_CONT, intermon_UNICAST_TREE, 
                              uni_routeid, (uint8_t *)monitor_irefs,
                              BYTES_IN_IREF * MAX_CPUS);
            assert(err_is_ok(err));
        }
    }
}

static errval_t connect_uni_neighbors(iref_t * irefs, size_t size,
                                      connected_cb_func_t connected_cb)
{
    errval_t err; 
    assert(size <= MAX_CPUS);

    struct neighbor * uni_neighbors [MAX_CPUS];
    int uni_neighbor_count = get_uni_neighbors(irefs, size, uni_neighbors);
    err = routing_connect_to_neighbors(uni_routeid, uni_neighbors, 
                                       uni_neighbor_count, connected_cb);
    assert(err_is_ok(err));

    // continues in connected_cb
    return err;
}

void route_done_join(struct intermon_binding *st, routeid_t id, 
                     coreid_t core, errval_t err, iref_t iref)
{
    assert (err_is_ok(err));
    monitor_irefs[core] = iref;

    // check if all irefs have been receieved
    for (int i=0; i<MAX_CPUS; i++) {
        if (bsp_monitors_list[i] && monitor_irefs[i] == NULL_IREF) {
            return;
        }
    }
    
    // all neighbors have joined, start connecting 
    err = connect_uni_neighbors(monitor_irefs, MAX_CPUS, route_initialize_bsp_3);
    assert(err_is_ok(err));
    // continues in callback route_initialize_bsp_3
}

static void route_initialize_bsp_2(void *rs, routeid_t routeid, errval_t err)
{
    assert(routeid == uni_routeid);
    assert(err == SYS_ERR_OK);
    
    // add my iref
    monitor_irefs[my_core_id] = routing_get_iref(uni_routeid);

    // signal other cores with routeid, get their iref
    for (int i=0; i<MAX_CPUS; i++) {
        if (bsp_monitors_list[i] && i != my_core_id) {
            monitor_irefs[i] = NULL_IREF;
            struct intermon_binding *intern_bind = intern[i].closure;
            assert (intern_bind);
            err = intern_bind->tx_vtbl.join_route(
                            intern_bind, NOP_CONT, intermon_UNICAST_TREE,
                            uni_routeid);
            assert(err_is_ok(err));
        }
    }

    // continues in route_done_join
}

/* 
 * Initialise route on bsp core
 */
errval_t route_initialize_bsp(bool monitors_list[MAX_CPUS]) 
{
    bsp_monitors_list = monitors_list;

    /* hack to avoid going through monitor blocking rpc binding */
    routing_id_alloc_func = mon_allocate_route_id;

    // create unicast-tree route
    return routing_new_route(&uni_routeid, routing_vtbl, NULL, route_initialize_bsp_2, false);
    // continues in route_initialize_bsp_2
}

struct route_done_join_state {
    struct intermon_msg_queue_elem elem;
    routeid_t routeid;
    errval_t err;
};

static void route_app_core_join_cb(void * st_arg, routeid_t id, errval_t err);

static void route_done_join_retry(struct intermon_binding *b,
                                  struct intermon_msg_queue_elem *e)
{
    struct route_done_join_state *st = (struct route_done_join_state *)e;
    route_app_core_join_cb(b, st->routeid, st->err);
    free(e);
}

static void route_app_core_join_cb(void * st_arg, routeid_t id, errval_t err)
{
    assert(err_is_ok(err));
    struct intermon_binding *b = (struct intermon_binding*)st_arg;
    iref_t iref = routing_get_iref(id);
    err = b->tx_vtbl.route_done_join(b, NOP_CONT, id, my_core_id, err, iref);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct route_done_join_state *e
                = malloc(sizeof(struct route_done_join_state));
            assert(e != NULL);
            e->routeid = id;
            e->err = err;
            e->elem.cont = route_done_join_retry;

            struct intermon_state *st = b->st;
            err = intermon_enqueue_send(b, &st->queue, get_default_waitset(),
                                        &e->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "intermon_enqueue_send failed");
            }
        } else {
            USER_PANIC_ERR(err, "route_done_join tx failed");
        }
    }
}

/* 
 * Initialise route on app core
 */
errval_t route_join_app_core(struct intermon_binding *st, 
                             intermon_ROUTE_TYPE_t route_type, 
                             routeid_t routeid)
{
    errval_t err;
    switch (route_type) {
    case intermon_UNICAST_TREE:
        // join unicast-tree route
        uni_routeid = routeid;
        err = routing_join_route(uni_routeid, routing_vtbl, st,
                                 route_app_core_join_cb, false);
        assert (err_is_ok(err));
        break;
    case intermon_MULTICAST_TREE:
        USER_PANIC("NYI");
        break;
    default:
        USER_PANIC("Error, invalid route type received");
        break;
    }
    return SYS_ERR_OK;
}

static void route_app_core_connect_cb(void * st_arg, routeid_t id,
                                      errval_t err) {
    assert(err_is_ok(err));
    struct intermon_binding *st = (struct intermon_binding*)st_arg;
    err = st->tx_vtbl.route_done_connect(st, NOP_CONT, id, err);
    assert(err_is_ok(err));
}

errval_t route_connect_app_core(intermon_ROUTE_TYPE_t route_type, 
                                routeid_t routeid, iref_t * irefs, size_t size)
{
    errval_t err;
    switch (route_type) {
    case intermon_UNICAST_TREE:
        // connect to unicast-tree neighbors
        assert(routeid == uni_routeid);
        err = connect_uni_neighbors(irefs, size, route_app_core_connect_cb);
        assert (err_is_ok(err));
        break;
    case intermon_MULTICAST_TREE:
        USER_PANIC("NYI");
        break;
    default:
        USER_PANIC("Error, invalid route type received");
        break;
    }
    return SYS_ERR_OK;
} 
