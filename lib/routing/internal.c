/**
 * \file
 * \brief Internal functions of the routing library
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <routing/routing.h>
#include <trace/trace.h>
#include <if/routing_defs.h>
#include "internal.h"

static coreid_t my_coreid;
static struct routing_rx_vtbl rx_vtbl;

/* ----------------- Message Receive / Forward Functions------------------ */

static void recv_bcast_msg(struct routing_binding *b, routeid_t routeid,
                           coremask_t mask, linkid_t linkid,
                           uint8_t *payload, size_t size)
{
    errval_t err;
    bool forward = true;
    struct route *route = get_route_struct(routeid);

    /* Send the msg to the application layer */
    if (mask_cmp(my_coreid, mask) && route->vtbl.bcast_recv_cb != NULL) {
        forward = route->vtbl.
            bcast_recv_cb(route->user_state, routeid, payload, size);
    }
    
    if (forward) {
        // broadcast onto neighboring links
        struct link *link = get_link_struct(linkid);
        for (coreid_t i = 0; i < link->bcast_children_count; i++) {
            if (mask & link->bcast_children[i]->mask) {
                struct neighbor *child = link->bcast_children[i];
                err = child->binding->tx_vtbl.
                    send_bcast_msg(child->binding, NOP_CONT, routeid, mask, 
                                   child->linkid, payload, size);
                assert(err_is_ok(err));
            }
        }
    }
    //free(payload); // XXX This does not seem to be safe, this means routing library leaks payload memory at the moment, FIXME!!
}

static void recv_bcast_with_ccast_msg(struct routing_binding *b,
                           routeid_t routeid, coremask_t mask, linkid_t linkid,
                           recordid_t recordid, uint8_t *payload, size_t size)
{
    errval_t err;
    bool forward = true;
    struct route *route = get_route_struct(routeid);
    struct link *link   = get_link_struct(linkid);

    /* Create a ccast record for this message on this core */
    coreid_t replies = mask_count(link->bcast_nexthop_mask & mask) + 
        (mask_cmp(my_coreid, mask) ? 1 : 0); /* one extra for this core's ccast reply? */
    struct record * record = alloc_record_from_bcast(route, link->ccast_parent, 
                                                     recordid, replies);

    /* Send the msg to the application layer */
    if (mask_cmp(my_coreid, mask) && 
        route->vtbl.bcast_with_ccast_recv_cb != NULL) {
        forward = route->vtbl.
            bcast_with_ccast_recv_cb(route->user_state, routeid, recordid, 
                                     &record->user_state, payload, size);
    }

    if (forward) {
        // broadcast onto neighboring links
        for (coreid_t i = 0; i < link->bcast_children_count; i++) {
            if (mask & link->bcast_children[i]->mask) {
                struct neighbor *child = link->bcast_children[i];
                err = child->binding->tx_vtbl.
                    send_bcast_with_ccast_msg(child->binding, NOP_CONT, routeid,
                                              mask, child->linkid, recordid, 
                                              payload, size);
                assert(err_is_ok(err));
            }
        }
    }
    //free(payload); // XXX This does not seem to be safe, this means routing library leaks payload memory at the moment, FIXME!!
}

void recv_ccast_msg(struct routing_binding *b, routeid_t routeid, 
                    recordid_t recordid, uint8_t *payload, size_t size)
{
    errval_t err;
    bool forward = true;
    struct route *route = get_route_struct(routeid);
    struct record *record = get_record_struct(route, recordid);

    if (record == NULL) {
        // this ccast must have already been forwarded, silently ignore
        return;
    }

    // one less reply
    record->replies_expected--;

    /* Send the msg to the application layer */
    if (route->vtbl.ccast_recv_cb != NULL) {
        forward = route->vtbl.
            ccast_recv_cb(route->user_state, routeid, record->user_state,
                          record->replies_expected, payload, size);
    }

    if (forward) {
        struct neighbor * parent = record->parent;
        if (parent == NULL) {
            assert(record->ccast_complete_cb != NULL);
            record->ccast_complete_cb(route->user_state, routeid, 
                                      record->user_state, payload, size);
        } else {
            err = parent->binding->tx_vtbl.
                send_ccast_msg(parent->binding, NOP_CONT, routeid, recordid, 
                               payload, size);
            assert(err_is_ok(err));
        }
        free_record(route, record);  // this record is not required any longer
    }
    //free(payload); // XXX This does not seem to be safe, this means routing library leaks payload memory at the moment, FIXME!!
}

static void recv_unicast_msg(struct routing_binding *b, routeid_t routeid, 
                             linkid_t linkid, coreid_t coreid,
                             uint8_t *payload, size_t size)
{
    errval_t err;
    struct route *route = get_route_struct(routeid);

    /* is this message meant for us */
    if (coreid == my_coreid) {
        /* if so, upcall app */
        assert (route->vtbl.unicast_recv_cb != NULL);
        route->vtbl.unicast_recv_cb(route->user_state, routeid, payload, size);
    } else {
        /* Otherwise, send to the neighbor that can reach coreid */
        struct link *link = get_link_struct(linkid);
        for (coreid_t i = 0; i < link->bcast_children_count; i++) {
            if (mask_cmp(coreid, link->bcast_children[i]->mask)) {
                struct neighbor *neighbor = link->bcast_children[i];
                err = neighbor->binding->tx_vtbl.
                    send_unicast_msg(neighbor->binding, NOP_CONT, routeid,
                                     neighbor->linkid, coreid, payload, size);
                assert(err_is_ok(err));
                return;
            }
        }
        assert(!"LINK_ERR_CORE_NOT_FOUND");
    }
    //free(payload); // XXX This does not seem to be safe, this means routing library leaks payload memory at the moment, FIXME!!
}


static void recv_bcast_fixmsg(struct routing_binding *b, routeid_t routeid,
                              coremask_t mask, linkid_t linkid, uint64_t w1, 
                              uint64_t w2, uint8_t size)
{
    errval_t err;
    bool forward = true;
    struct route *route = get_route_struct(routeid);

    /* Send the msg to the application layer */
    if (mask_cmp(my_coreid, mask) && route->vtbl.bcast_recv_cb != NULL) {
        uint64_t payload_64[2] = {w1,w2};
        uint8_t * payload = (uint8_t *)payload_64;
        forward = route->vtbl.
            bcast_recv_cb(route->user_state, routeid, payload, size);
    }
    
    if (forward) {
        // broadcast onto neighboring links
        struct link *link = get_link_struct(linkid);
        for (coreid_t i = 0; i < link->bcast_children_count; i++) {
            if (mask & link->bcast_children[i]->mask) {
                struct neighbor *child = link->bcast_children[i];
                err = child->binding->tx_vtbl.
                    send_bcast_fixmsg(child->binding, NOP_CONT, routeid, mask, 
                                      child->linkid, w1, w2, size);
                assert(err_is_ok(err));
            }
        }
    }
}

static void recv_bcast_with_ccast_fixmsg(struct routing_binding *b,
                           routeid_t routeid, coremask_t mask, linkid_t linkid,
                           recordid_t recordid, uint64_t w1, uint64_t w2,
                           uint8_t size)
{
    errval_t err;
    bool forward = true;
    struct route *route = get_route_struct(routeid);
    struct link *link   = get_link_struct(linkid);

    trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_RECV_BCAST_WITH_CCAST, disp_get_core_id());

    /* Create a ccast record for this message on this core */
    coreid_t replies = mask_count(link->bcast_nexthop_mask & mask) + 
        (mask_cmp(my_coreid, mask) ? 1 : 0); /* one extra for this core's ccast reply? */
    struct record * record = alloc_record_from_bcast(route, link->ccast_parent, 
                                                     recordid, replies);

    /* Send the msg to the application layer */
    if (mask_cmp(my_coreid, mask) && 
        route->vtbl.bcast_with_ccast_recv_cb != NULL) {
        uint64_t payload_64[2] = {w1,w2};
        uint8_t * payload = (uint8_t *)payload_64;

        forward = route->vtbl.
            bcast_with_ccast_recv_cb(route->user_state, routeid, recordid, 
                                     &record->user_state, payload, size);
    }

    if (forward) {
        // broadcast onto neighboring links
        for (coreid_t i = 0; i < link->bcast_children_count; i++) {
            if (mask & link->bcast_children[i]->mask) {
                struct neighbor *child = link->bcast_children[i];
                err = child->binding->tx_vtbl.
                    send_bcast_with_ccast_fixmsg(child->binding, NOP_CONT, routeid,
                                              mask, child->linkid, recordid, 
                                              w1, w2, size);
                assert(err_is_ok(err));
            }
        }
    }
}

void recv_ccast_fixmsg(struct routing_binding *b, routeid_t routeid, 
                       recordid_t recordid, uint64_t w1, uint64_t w2,
                       uint8_t size)
{
    errval_t err;
    bool forward = true;
    struct route *route = get_route_struct(routeid);
    struct record *record = get_record_struct(route, recordid);

    trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_RECV_CCAST, disp_get_core_id());

    if (record == NULL) {
        // this ccast must have already been forwarded, silently ignore
        return;
    }

    // one less reply
    record->replies_expected--;

    uint64_t payload_64[2] = {w1,w2};
    uint8_t * payload = (uint8_t *)payload_64;

    /* Send the msg to the application layer */
    if (route->vtbl.ccast_recv_cb != NULL) {
        forward = route->vtbl.
            ccast_recv_cb(route->user_state, routeid, record->user_state,
                          record->replies_expected, payload, size);
    }

    if (forward) {
        struct neighbor * parent = record->parent;
        if (parent == NULL) {
            assert(record->ccast_complete_cb != NULL);
            record->ccast_complete_cb(route->user_state, routeid, 
                                      record->user_state, payload, size);
        } else {
            err = parent->binding->tx_vtbl.
                send_ccast_fixmsg(parent->binding, NOP_CONT, routeid, recordid, 
                                  w1, w2, size);
            assert(err_is_ok(err));
        }
        free_record(route, record);  // this record is not required any longer
    }
}

static void recv_unicast_fixmsg(struct routing_binding *b, routeid_t routeid, 
                                linkid_t linkid, coreid_t coreid,
                                uint64_t w1, uint64_t w2, uint8_t size)
{
    errval_t err;
    struct route *route = get_route_struct(routeid);


    /* is this message meant for us */
    if (coreid == my_coreid) {
        /* if so, upcall app */
        uint64_t payload_64[2] = {w1,w2};
        uint8_t * payload = (uint8_t *)payload_64;
        assert (route->vtbl.unicast_recv_cb != NULL);
        route->vtbl.unicast_recv_cb(route->user_state, routeid, payload, size);
    } else {
        /* Otherwise, send to the neighbor that can reach coreid */
        struct link *link = get_link_struct(linkid);
        for (coreid_t i = 0; i < link->bcast_children_count; i++) {
            if (mask_cmp(coreid, link->bcast_children[i]->mask)) {
                struct neighbor *neighbor = link->bcast_children[i];
                err = neighbor->binding->tx_vtbl.
                    send_unicast_fixmsg(neighbor->binding, NOP_CONT, routeid,
                                        neighbor->linkid, coreid, w1, w2, size);
                assert(err_is_ok(err));
                return;
            }
        }
        assert(!"LINK_ERR_CORE_NOT_FOUND");
    }
}


/* ----------------------- Route Connect / Init Functions ------------------ */


static void connect_neighbor_cont(struct route *route, struct neighbor *neighbor,
                                  coreid_t init_core, linkid_t mylinkid,
                                  bool return_link)
{
    errval_t err;
    assert (neighbor->bind_done && neighbor->binding != NULL);
    
    // Send the remote node my core id
    err = neighbor->binding->tx_vtbl.
        neighbor_connect_request(neighbor->binding, NOP_CONT, route->id,
                                 get_neighbor_id(neighbor), mylinkid, 
                                 route->my_iref, return_link, init_core,
                                 my_coreid, neighbor->mask);
    assert(err_is_ok(err));
}

struct bind_state {
    struct route *route;
    struct neighbor *neighbor;
    coreid_t init_core;
    linkid_t mylinkid;
    bool return_link;
};

static void route_bind_cb(void *st, errval_t err, struct routing_binding *b)
{
    struct bind_state * bind_st = st;

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }

    struct neighbor *neighbor = bind_st->neighbor;
    assert(!neighbor->bind_done);
    
    if (bind_st->route->bindings[neighbor->coreid] == NULL) {
        b->rx_vtbl = rx_vtbl;
        bind_st->route->bindings[neighbor->coreid] = b;
    } else {
        // someone set up a binding in the meantime, use that instead
        // XXX FIXME, throw away old binding or prevent this from happening
    }

    neighbor->binding   = bind_st->route->bindings[neighbor->coreid];
    neighbor->bind_done = true;

    connect_neighbor_cont(bind_st->route, neighbor, bind_st->init_core,
                          bind_st->mylinkid, bind_st->return_link);

    free(bind_st);
}

static errval_t bind_neighbor(struct route * route, struct neighbor * neighbor,
                              coreid_t init_core, linkid_t mylinkid,
                              bool return_link)
{
    errval_t err;

    if (!route->bindings[neighbor->coreid]) {
       
        if (neighbor->iref == NULL_IREF) {
            // you must provide irefs if not using the nameserver
            assert (route->use_nameservice); 

            char service_name[128];
            get_service_name(service_name, neighbor->coreid, route->id);
            err = nameservice_blocking_lookup(service_name, &(neighbor->iref));
            
            if (err_is_fail(err) || neighbor->iref == NULL_IREF) {
                return err_push(err, ROUTE_ERR_LOOKUP); 
            }
        }
        
        struct bind_state* bind_st = malloc(sizeof(struct bind_state));
        bind_st->route = route;
        bind_st->neighbor = neighbor;
        bind_st->init_core = init_core;
        bind_st->mylinkid = mylinkid;
        bind_st->return_link = return_link;
        
        // create new binding to this core
        err = routing_bind(neighbor->iref, route_bind_cb, bind_st,
                           get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            free(bind_st);
            err_push(err, ROUTE_ERR_BIND); 
        }
        // connect_neighbor_cont will be called in bind continuation
        return err;
    } else {
        // reuse existing binding to this core
        neighbor->binding   = route->bindings[neighbor->coreid];
        neighbor->bind_done = true;
        // call connect_neighbor_cont directly
        connect_neighbor_cont(route, neighbor, init_core, mylinkid, 
                              return_link);
    }

    return SYS_ERR_OK;
}

errval_t connect_neighbors(struct route *route, struct neighbor **neighbors, 
                           coreid_t neighbors_count, coreid_t init_core,
                           linkid_t mylinkid, bool return_link) 
{
    errval_t err;
    assert(route->exported);

    // connect to each neighbor
    for (int i=0; i<neighbors_count; i++) {
        struct neighbor * neighbor = neighbors[i]; 

        assert (!neighbor->connected);

        err = bind_neighbor(route, neighbor, init_core, mylinkid, return_link);
        if (err_is_fail(err)) {
            DEBUG_ERR(err,"error binding");
            abort();
        }

    }
    return SYS_ERR_OK;
}

static void neighbor_connect_request(struct routing_binding *b, routeid_t routeid,
                                     neighborid_t neighborid, linkid_t lasthopid,
                                     iref_t lasthop_iref, uint8_t return_link, 
                                     coreid_t init_core, coreid_t lasthop_core,
                                     coremask_t childcores)
{
    errval_t err;

    struct route * route = get_route_struct(routeid);
    assert(route);

    // Only callback up to user and setup link if this is not a return link
    if (!return_link) {
        struct neighbor * ccast_parent = NULL;
        struct neighbor * children [MAX_CPUS];
        coreid_t children_count = 0;

        assert (route->vtbl.neighbor_connect_req_cb);
        bool connect = route->vtbl.
            neighbor_connect_req_cb(route->user_state, routeid, init_core,
                                    lasthop_core, childcores, children,
                                    &children_count, &ccast_parent);

        if (connect) {  
            if (ccast_parent == NULL) {
                // set ccast parent to be the same as bcast parent
                ccast_parent = routing_new_neighbor_iref(lasthop_core, 0, lasthop_iref);
            }

            // create and initialise link structure
            struct link * link   = malloc (sizeof(struct link));
            link->ccast_parent   = ccast_parent;
            link->bcast_children = malloc(sizeof(struct neighbors*) * children_count);
            memcpy(link->bcast_children, children, 
                   sizeof(struct neighbors*) * children_count);
            link->bcast_children_count = children_count;
            link->p_neighborid = neighborid;
            link->p_linkid = lasthopid;
            link->remaining_responses = 1 + children_count;
            link->p_bind = b;
            link->bcast_nexthop_mask = 0;
            link->bcast_reachable_mask = 0;
            for (int i=0; i<children_count; i++) {
                link->bcast_reachable_mask |= children[i]->mask;
                link->bcast_nexthop_mask |= 0x1 << children[i]->coreid; 
            }
            linkid_t mylinkid = get_link_id(link);
          
            // connect to parent
            err = connect_neighbors(route, &ccast_parent, 1, init_core, 
                                    mylinkid, true);
            assert(err_is_ok(err));
            
            
            // connect to children
            if (children_count > 0) {
                err = connect_neighbors(route, children, children_count,
                                        init_core, mylinkid, false);
                assert(err_is_ok(err));
            }
            
        } else {
            // Not connecting
            err = b->tx_vtbl.neighbor_connect_response(b, NOP_CONT, routeid, 
                                                       neighborid, lasthopid,
                                                       0, false); 
            assert(err_is_ok(err));
        }
    } else {
        // if return link, send response immediatly
        err = b->tx_vtbl.neighbor_connect_response(b, NOP_CONT, routeid, 
                                                   neighborid, lasthopid,
                                                   0, true);
        assert(err_is_ok(err));
    }   
}

static void neighbor_connect_response(struct routing_binding *b, routeid_t routeid,
                                      neighborid_t neighborid, linkid_t mylinkid,
                                      linkid_t otherlinkid, uint8_t connected)
{
    errval_t err;

    assert(connected == true);  /// XXX 
    struct neighbor * neighbor = get_neighbor_struct(neighborid);
    
    neighbor->linkid = otherlinkid;
    neighbor->connected = true;

    if (mylinkid != 0) {
        struct link * link = get_link_struct(mylinkid);
        assert (link->remaining_responses != 0);
        link->remaining_responses--;
        if (link->remaining_responses == 0) {
            err = link->p_bind->tx_vtbl.neighbor_connect_response(
                               link->p_bind, NOP_CONT, routeid, 
                               link->p_neighborid, link->p_linkid, mylinkid,
                               true);
            assert(err_is_ok(err));
        }
    } else {
        struct route * route = get_route_struct(routeid);
        assert (route->waiting_for_connection != 0);
        route->waiting_for_connection--;
        if (route->waiting_for_connection == 0 && route->connected_cb) {
            // call connection callback
            route->connected_cb(route->user_state, route->id, SYS_ERR_OK);
        }
    }
}    

/* -------------------------- Route Accessor Functions--------------------- */
/** XXX Do something very much less braindead here!! */
static int route_db_count = 0;
static struct route * route_db[MAX_ROUTES];
void set_route_struct(routeid_t id, struct route * route)
{
    assert(route_db_count < (MAX_ROUTES - 1));
    route_db[route_db_count++] = route;
}

struct route* get_route_struct(routeid_t id)
{
    // very braindead just to get things working...
    for (int i=0; i<route_db_count; i++) {
        if (route_db[i]->id == id) {
            return route_db[i];
        }
    }
    assert(!"Couldn't find route for routeid");
    return NULL;
}


/* ----------------------- Route Initialisation Functions------------------ */


static struct routing_rx_vtbl rx_vtbl = {
    .send_bcast_msg           = recv_bcast_msg,
    .send_bcast_with_ccast_msg= recv_bcast_with_ccast_msg,
    .send_ccast_msg           = recv_ccast_msg,
    .send_unicast_msg         = recv_unicast_msg,
    .send_bcast_fixmsg           = recv_bcast_fixmsg,
    .send_bcast_with_ccast_fixmsg= recv_bcast_with_ccast_fixmsg,
    .send_ccast_fixmsg           = recv_ccast_fixmsg,
    .send_unicast_fixmsg         = recv_unicast_fixmsg,
    .neighbor_connect_request = neighbor_connect_request,
    .neighbor_connect_response= neighbor_connect_response,
};

static errval_t route_connect_cb(void *st, struct routing_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

static void route_export_cb(void *st, errval_t err, iref_t iref)
{
    struct route * route = st;

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service if requested
    if (route->use_nameservice) {
        char service_name[128];
        get_service_name(service_name, my_coreid, route->id);

        errval_t err2 = nameservice_register(service_name, iref);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "routing libary nameservice_register failed");
            abort();
        }
    }
    route->my_iref  = iref;
    route->exported = true;
    if (route->joined_cb) {
        route->joined_cb(route->user_state, route->id, err);
    }
}

errval_t init_route(routeid_t id, struct routing_cb_vtbl handlers, 
                    void * handler_state, joined_cb_func_t joined_cb,
                    bool use_nameservice) 
{
    errval_t err;
    struct route * route = malloc(sizeof(struct route));

    my_coreid = disp_get_core_id();

    if (!route) {
        return LIB_ERR_MALLOC_FAIL;
    }
    
    route->id             = id;
    route->user_state     = handler_state;
    route->vtbl           = handlers;
    route->bcast_neighbors_count = 0;
    route->records        = NULL;
    route->records_count  = 0;
    route->exported       = false;
    route->joined_cb      = joined_cb;
    route->use_nameservice= use_nameservice;

    // save route for later
    set_route_struct(id, route);

    err = routing_export(route, route_export_cb, route_connect_cb,
                         get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        free(route);
        // XXX FIXME, remove route struct from route_db list
        return err_push(err, ROUTE_ERR_EXPORT);
    }

    return SYS_ERR_OK;
}
