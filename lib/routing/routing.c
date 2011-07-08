/**
 * \file
 * \brief Routing library
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
#include <routing/routing.h>
#include <trace/trace.h>
#include <if/routing_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include "internal.h"

/* Hack for monitor which doesn't have a binding to its blocking interface */
routeid_t(*routing_id_alloc_func)(void) = NULL;

/* ---------------------------------- API ---------------------------- */

/**
 * \brief Create a new route that can be used for bcast, ccast or unicast
 */
errval_t routing_new_route(routeid_t *id, struct routing_cb_vtbl handlers,
                           void * handler_state, joined_cb_func_t joined_cb,
                           bool use_nameservice)
{
    errval_t err;

    //printf("%d: routing_new_route\n", disp_get_core_id());

    if (routing_id_alloc_func) {
        *id = routing_id_alloc_func();
        err = SYS_ERR_OK;
    } else {
        struct monitor_blocking_rpc_client *mrc = get_monitor_blocking_rpc_client();
        err = mrc->vtbl.allocate_route_id(mrc, id);
    }

    if (err_is_fail(err)) {
        err_push(err, ROUTE_ERR_NEW_ROUTE);
        return err;
    }

    err = init_route(*id, handlers, handler_state, joined_cb, use_nameservice);
    if (err_is_fail(err)) {
        err_push(err, ROUTE_ERR_NEW_ROUTE);
    }
    return err;
}

/**
 * \brief Create a new route that can be used for bcast, ccast or unicast
 */
errval_t routing_join_route(routeid_t id, struct routing_cb_vtbl handlers,
                            void * handler_state, joined_cb_func_t joined_cb,
                            bool use_nameservice)
{
    errval_t err;

    //printf("%d: routing_join_route\n", disp_get_core_id());
    // XXX - TODO ensure route exists and no one else on this core has joined it

    err = init_route(id, handlers, handler_state, joined_cb, use_nameservice);
    if (err_is_fail(err)) {
        err_push(err, ROUTE_ERR_NEW_ROUTE);
    }
    return err;
}

/**
 * Returns this core's iref for the given route
 */
iref_t routing_get_iref(routeid_t id)
{
    struct route * route = get_route_struct(id);
    assert(route);
    return route->my_iref;
}

/**
 * \brief Creates and returns a new neighbor object
 */
struct neighbor * routing_new_neighbor_iref(coreid_t coreid,
                                            coremask_t child_neighbors_mask,
                                            iref_t iref)
{
    struct neighbor * neighbor = malloc(sizeof(struct route));

    if (!neighbor) {
        return NULL;
    }
    
    neighbor->coreid = coreid;
    neighbor->mask   = child_neighbors_mask;
    neighbor->iref   = iref;
    neighbor->connected = false;
    neighbor->bind_done = false;

    return neighbor;
}



/**
 * \brief Connects the provided routeid to the given group of neighbours for 
 * this core
 */
errval_t routing_connect_to_neighbors(routeid_t id, struct neighbor **neighbors,
                                      size_t neighbors_count,
                                      connected_cb_func_t connected_cb)
{
    struct route * route = get_route_struct(id);
    assert(route->exported);
    assert(route->bcast_neighbors_count == 0);  // TODO allow additions to route

    route->bcast_neighbors = malloc(sizeof(struct neigbours*) *neighbors_count);
    memcpy(route->bcast_neighbors, neighbors,
           sizeof(struct neigbours*) * neighbors_count);
    route->bcast_neighbors_count = neighbors_count;
    route->waiting_for_connection = neighbors_count;
    route->connected_cb = connected_cb;
    route->bcast_reachable_mask = 0;
    route->bcast_nexthop_mask = 0;
    for (int i=0; i<neighbors_count; i++) {
        route->bcast_reachable_mask |= neighbors[i]->mask; 
        route->bcast_nexthop_mask |= 0x1 << neighbors[i]->coreid; 
    }

    return connect_neighbors(route, neighbors, neighbors_count, 
                             disp_get_core_id(), 0, false);
}

/**
 * \brief Broadcast a message.
 * The message is sent to the application on bcast_recv_cb
 * and then if the cb allows, sent to all neighbors.
 *
 * \param id       The route to bcast the msg on
 * \param payload  The payload
 * \param size     Size of the payload
 */
errval_t routing_send_sel_bcast(routeid_t id, coremask_t mask,
                                uint8_t *payload, size_t size)
{
    errval_t err = SYS_ERR_OK;
    struct route *route = get_route_struct(id);
    assert(route->waiting_for_connection == 0);

    // size multiple of 64 bits and less than 2 * 64 bit words
    bool use_fixed = ((size & 0x7) == 0) && (size <= 16); 
    uint64_t * p_64 = (uint64_t *) payload;

    for (coreid_t i = 0; i < route->bcast_neighbors_count; i++) {
        if (mask & route->bcast_neighbors[i]->mask) {
            struct neighbor *neighbor = route->bcast_neighbors[i];
            if (use_fixed) {
                err = neighbor->binding->tx_vtbl.
                    send_bcast_fixmsg(neighbor->binding, NOP_CONT, id, mask,
                                      neighbor->linkid, 
                                      (size >= 8)  ? p_64[0] : 0,
                                      (size >= 16) ? p_64[1] : 0,
                                      (uint8_t) size);
            } else {
                err = neighbor->binding->tx_vtbl.
                    send_bcast_msg(neighbor->binding, NOP_CONT, id, mask,
                                   neighbor->linkid, payload, size);
            }
            assert(err_is_ok(err));
        }
    }

    return err;
}

/**
 * \brief Broadcast a message.
 * The message is sent to the application on bcast_recv_cb
 * and then if the cb allows, sent to all neighbors.
 *
 * \param id       The route to bcast the msg on
 * \param payload  The payload
 * \param size     Size of the payload
 */
errval_t routing_send_bcast(routeid_t id, uint8_t *payload, size_t size)
{
    struct route *route = get_route_struct(id);
    
    return routing_send_sel_bcast(id, route->bcast_reachable_mask, payload, size);
}

/**
 * \brief Broadcast a message which will initiate a convergecast. 
 * 
 * The message is sent to all other cores on this route through the 
 * bcast_recv_cb callback supplied by the application when the route was 
 * created. A convergecast record is created during the broadcast, and passed
 * to the bcast_with_ccast_recv_cb, with the intention that eventually a 
 * convergecast will be initiated using this record.  Once the converge 
 * cast has completed, the result will be returned to the application
 * using the supplied ccast_complete_cb 
 *
 * \param id                The route to bcast the msg on
 * \param payload           The payload
 * \param size              Size of the payload
 * \param ccast_complete_cb Callback used to return result of convergecast
 */
errval_t routing_send_sel_bcast_with_ccast(
             routeid_t id, coremask_t mask, uint8_t *payload, size_t size,
             ccast_complete_func_t ccast_complete_cb, void * ccast_state)
{
    errval_t err = SYS_ERR_OK;
    struct route *route = get_route_struct(id);

    // allocate a record for this convergecast 
    coreid_t replies = mask_count(route->bcast_nexthop_mask & mask);

    struct record *record = alloc_new_record(route, ccast_complete_cb, replies);

    // size multiple of 64 bits and less than 2 * 64 bit words
    bool use_fixed = ((size & 0x7) == 0) && (size <= 16); 
    uint64_t * p_64 = (uint64_t *) payload;

    record->user_state = ccast_state;

    // send broadcast to all neighbours on this route and in mask
    for (coreid_t i = 0; i < route->bcast_neighbors_count; i++) {
        if (mask & route->bcast_neighbors[i]->mask) {
            struct neighbor *neighbor = route->bcast_neighbors[i];
            if (use_fixed) {
                trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_BCAST_WITH_CCAST_SEND, i);
                err = neighbor->binding->tx_vtbl.
                    send_bcast_with_ccast_fixmsg(neighbor->binding, NOP_CONT, 
                                                 id,  mask, neighbor->linkid, 
                                                 record->id,
                                                 (size >= 8)  ? p_64[0] : 0,
                                                 (size >= 16) ? p_64[1] : 0,
                                                 (uint8_t) size);
            } else {
                err = neighbor->binding->tx_vtbl.
                    send_bcast_with_ccast_msg(neighbor->binding, NOP_CONT, id, 
                                              mask, neighbor->linkid, 
                                              record->id, payload, size);
            }
            assert(err_is_ok(err));
        }
    }

    return err;
}

/**
 * \brief Broadcast a message which will initiate a convergecast. 
 * 
 * The message is sent to all other cores on this route through the 
 * bcast_recv_cb callback supplied by the application when the route was 
 * created. A convergecast record is created during the broadcast, and passed
 * to the bcast_with_ccast_recv_cb, with the intention that eventually a 
 * convergecast will be initiated using this record.  Once the converge 
 * cast has completed, the result will be returned to the application
 * using the supplied ccast_complete_cb 
 *
 * \param id                The route to bcast the msg on
 * \param payload           The payload
 * \param size              Size of the payload
 * \param ccast_complete_cb Callback used to return result of convergecast
 */
errval_t routing_send_bcast_with_ccast(routeid_t id, uint8_t *payload, size_t size,
                                       ccast_complete_func_t ccast_complete_cb, 
                                       void * ccast_state)
{
    struct route *route = get_route_struct(id);
    assert(route->waiting_for_connection == 0);

    trace_event(TRACE_SUBSYS_ROUTE, TRACE_EVENT_BCAST_WITH_CCAST, 0);

    return routing_send_sel_bcast_with_ccast(id, route->bcast_reachable_mask, payload, size,
                                             ccast_complete_cb, ccast_state);

}

/**
 * \brief Send a convergecast message
 * 
 * The message is sent to the application on route's bcast_with_ccast_recv_cb 
 * callback and then if the cb allows, sent on to all neighbors.  The 
 * A convergecast record is created during the broadcast, and passed to 
 * bcast_with_ccast_recv_cb, with the intention that eventually a convergecast
 * will be initiated using this record.
 *
 * \param id           The route to ccast the msg on
 * \param ccast_record The record associated with this ccast
 * \param payload      The payload
 * \param size         Size of the payload
 */
errval_t routing_send_ccast(routeid_t id, recordid_t ccast_record_id,
                            uint8_t *payload, size_t size)
{
    // size multiple of 64 bits and less than 2 * 64 bit words
    bool use_fixed = ((size & 0x7) == 0) && (size <= 16); 
    uint64_t * p_64 = (uint64_t *) payload;

    if (use_fixed) {
        recv_ccast_fixmsg(NULL, id, ccast_record_id, 
                          (size >= 8)  ? p_64[0] : 0,
                          (size >= 16) ? p_64[1] : 0,
                          (uint8_t) size);
    } else {
        recv_ccast_msg(NULL, id, ccast_record_id, payload, size);
    }
    
    return SYS_ERR_OK;
}

/**
 * \brief Unicast a msg.
 * Send the msg to a single node.
 *
 * \param routeid  The route on which to send the msg
 * \param coreid   The core to send the msg to
 * \param payload  The payload
 * \param size     Size of the payload
 */
errval_t routing_send_unicast(routeid_t routeid, coreid_t coreid,
                              uint8_t *payload, size_t size)
{
    errval_t err;
    struct route *route = get_route_struct(routeid);
    assert(route->waiting_for_connection == 0);

    // size multiple of 64 bits and less than 2 * 64 bit words
    bool use_fixed = ((size & 0x7) == 0) && (size <= 16); 
    uint64_t * p_64 = (uint64_t *) payload;

    if (coreid == disp_get_core_id()) {
        // was meant for local core, send to application
        assert(route->vtbl.unicast_recv_cb);
        route->vtbl.unicast_recv_cb(route->user_state, routeid, payload, size);
        return SYS_ERR_OK;
    } else {
        /* Send to the neighbor that can reach coreid */
        for (coreid_t i = 0; i < route->bcast_neighbors_count; i++) {
            if (mask_cmp(coreid, route->bcast_neighbors[i]->mask)) {
                struct neighbor *neighbor = route->bcast_neighbors[i];
                if (use_fixed) {
                    err = neighbor->binding->tx_vtbl.
                        send_unicast_fixmsg(neighbor->binding, NOP_CONT, 
                                            routeid,  neighbor->linkid, coreid,
                                            (size >= 8)  ? p_64[0] : 0,
                                            (size >= 16) ? p_64[1] : 0,
                                            (uint8_t) size);
                } else {
                    err = neighbor->binding->tx_vtbl.
                        send_unicast_msg(neighbor->binding, NOP_CONT, routeid,
                                         neighbor->linkid, coreid, payload, size);
                }
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "sending failed");
                }
                return SYS_ERR_OK;
            }
        }
        return ROUTE_ERR_CORE_NOT_FOUND;
    }
}
