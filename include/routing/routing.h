/**
 * \file
 * \brief Routing library.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ROUTING_H
#define ROUTING_H


/* record id type */
typedef uint32_t recordid_t;

/* Route id type */
typedef uint32_t routeid_t;

/* neighbor objects */
struct neighbor;

/* can be set to a different route id allocation function (hack for monitor) */
extern routeid_t(*routing_id_alloc_func)(void);

/* ------------------------ Callback functions ---------------------------*/
/**
 * \brief Callback to application code when a unicast msg is received.
 *
 * \param state     User provided state associated with this route
 * \param payload   The payload
 * \param size      Size of the payload in bytes
 */
typedef void (*unicast_recv_func_t)(void *state, routeid_t routeid,
                                    uint8_t *payload, size_t size);

/**
 * \brief Callback to application code when a bcast msg is received 
 * The application can decides if it wants to further forward the msg by 
 * returning true or false.  The contents of the msg can be modified before 
 * it is forwarded if required.
 *
 * \param state     User provided state associated with this route
 * \param payload   The payload
 * \param size      Size of the payload in bytes
 *
 * \return          If true, payload should be forwarded to neighbors,
 *                  if false, routing libary will not forward message
 */
typedef bool (*bcast_recv_func_t)(void *state, routeid_t routeid,
                                  uint8_t *payload, size_t size);

/**
 * \brief Callback to application code when a bcast msg, that expects a
 * ccast reponse, is received.  The ccast_record passed should be used to send 
 * the ccast response.  The application can decides if it wants to further 
 * forward the msg by returning true or false.  The contents of the msg can be 
 * modified before it is forwarded if required.
 *
 * \param route_state       User provided state associated with this route
 * \param ccast_record      Record to use when sending response ccast
 * \param record_state      Can set to user state which will be returned on 
 *                          ccast_recv callbacks associated with this ccast
 * \param payload           The payload
 * \param size              Size of the payload in bytes
 *
 * \return          If true, payload should be forwarded to neighbors,
 *                  if false, routing libary will not forward message
 */
typedef bool (*bcast_with_ccast_recv_func_t)(void *route_state, routeid_t routeid,
                                             recordid_t ccast_record,
                                             void ** record_state,
                                             uint8_t *payload, size_t size);

/**
 * \brief Callback to aggregate a received ccast msg
 * 
 * The application can modify the the payload before sending it.
 * The application can also decide if the library should wait for remaining children
 * This is called even after the application layer ccasts a msg.
 *
 * \param route_state        User provided state associated with this route
 * \param ccast_record       Record associated with this convergecast
 * \param ccast_state        State associated with this ccast 
 * \param children_remaining Number of responces from children that are still
 *                           expected
 * \param payload            The payload received from the child
 * \param size               Size of the received and parent payload in bytes
 *
 * \return          If true, do not wait for remaining children and send to parent immediately,
 *                  if false, wait for children and do not send to parent yet.
 */
typedef bool (*ccast_recv_func_t)(void *route_state, routeid_t routeid,
                                  void * record_state, coreid_t children_remaining,
                                  uint8_t *payload, size_t size);


/**
 * \brief Callback when a neighbor requests a binding connection.
 * User should connect to the cores specified in childcores coremask.
 *   
 * \param state          User provided state associated with this route
 * \param routeid        The route id
 * \param init_core      Core that initated this request for a link
 * \param bcast_parent   Core that is our bcast parent on this link
 * \param childcores     Mask of cores which should be connected to as children
 *                       of this neighbors connection
 * \param children       An ordered list of bcast children that will be
 *                       connected to after this callback returns
 * \param children_count If required, set this to count of bcast children to 
 *                       be connect 
 * \param ccast_parent   If ccast parent should be different from bcast parent,
 *                       set this to ccast_parent
 *
 * \return             True if bind is accepted
 */
typedef bool (*neighbor_connect_req_func_t)(
                   void *state, routeid_t routeid,
                   coreid_t init_core, coreid_t bcast_parent,
                   coremask_t childcores, struct neighbor * children[MAX_CPUS],
                   coreid_t * children_count, struct neighbor ** ccast_parent);


/** Callback handler table associated with each route */
struct routing_cb_vtbl {
    unicast_recv_func_t unicast_recv_cb;
    bcast_recv_func_t bcast_recv_cb;
    bcast_with_ccast_recv_func_t bcast_with_ccast_recv_cb;
    ccast_recv_func_t ccast_recv_cb;
    neighbor_connect_req_func_t neighbor_connect_req_cb;
};


/**
 * \brief Callback to application when a convergecast has completed
 * 
 * \param route_state    User provided state associated with this route
 * \param routeid        Route id
 * \param ccast_state    State associated with this ccast 
 * \param payload        Payload received from convergecast
 * \param size           Size of payload in bytes
 */
typedef void (*ccast_complete_func_t)(void *route_state, routeid_t routeid,
                                      void *ccast_state,  uint8_t *payload,
                                      size_t size);

/** 
 * \brief Called when route successfully joined 
 */
typedef void (*joined_cb_func_t)(void *route_state, routeid_t routeid, 
                                 errval_t err);

/** 
 * \brief Called when route successfully connected 
 */
typedef void (*connected_cb_func_t)(void *route_state, routeid_t routeid, 
                                    errval_t err);

/* --------------------------- API functions ------------------------------*/


/**
 * \brief Create a new route that can be used for bcast, ccast or unicast, 
 *        should only be called once to set up the route.
 *
 * \param id         Returns the route id allocated to this newly created route
 * \param handlers      Table of callback handlers
 * \param handler_state State passed to handlers when they are called
 */
errval_t routing_new_route(routeid_t *id, struct routing_cb_vtbl handlers,
                           void * handler_state, joined_cb_func_t joined_cb,
                           bool use_nameservice);

/**
 * \brief Join an existing route.  Only one domain on each core should join
 * the same routeid.
 *
 * \param id       The route id to join.
 * \param handlers      Table of callback handlers
 * \param handler_state State passed to handlers when they are called
 */
errval_t routing_join_route(routeid_t id, struct routing_cb_vtbl handlers,
                            void * handler_state, joined_cb_func_t joined_cb,
                            bool use_nameservice);

/**
 * Returns this core's iref for the given route
 */
iref_t routing_get_iref(routeid_t id);


/**
 * \brief Creates and returns a new neighbor object
 *
 * \param coreid                neighbors core id
 * \param child_neighbors_mask  mask of cores which are reachable (as children)
 *                              from this core
 * \param iref                  the iref of this neighbour
 */
struct neighbor * routing_new_neighbor_iref(coreid_t coreid,
                                 coremask_t child_neighbors_mask, iref_t iref);

/**
 * \brief Creates and returns a new neighbor object
 *
 * \param coreid                neighbors core id
 * \param child_neighbors_mask  mask of cores which are reachable (as children)
 *                              from this core
 */
static inline struct neighbor * routing_new_neighbor(coreid_t coreid,
                                       coremask_t child_neighbors_mask)
{
    return routing_new_neighbor_iref(coreid, child_neighbors_mask, NULL_IREF);
}

/**
 * \brief Connects the provided routeid to the given group of neighbors for 
 * this core
 *
 * \param routeid        The route
 * \param neighbors      Ordered list of neighbors
 * \param neigbors_count Number of neighbors
 */
errval_t routing_connect_to_neighbors(routeid_t id, struct neighbor **neighbors,
                                      size_t neigbors_count, 
                                      connected_cb_func_t connected_cb);

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
                              uint8_t *payload, size_t size);

/**
 * \brief Broadcast a message.
 * 
 * The message is sent to all other cores on this route through the 
 * bcast_recv_cb callback supplied by the application when the route was 
 * created.
 *
 * \param id       The route to bcast the msg on
 * \param payload  The payload
 * \param size     Size of the payload
 */
errval_t routing_send_bcast(routeid_t id, uint8_t *payload, size_t size);

/**
 * \brief Broadcast a message.
 * 
 * The message is sent to all other cores on this route through the 
 * bcast_recv_cb callback supplied by the application when the route was 
 * created.
 *
 * \param id       The route to bcast the msg on
 * \param tocores  Cores to send this bcast to
 * \param payload  The payload
 * \param size     Size of the payload
 */
errval_t routing_send_sel_bcast(routeid_t id, coremask_t tocores,
                                uint8_t *payload, size_t size);

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
 * \param ccast_state       State associated with this ccast and passed to
 *                          ccast_complete_cb and ccast_recv (on this core)
 */
errval_t routing_send_bcast_with_ccast(routeid_t id, uint8_t *payload, size_t size,
                                       ccast_complete_func_t ccast_complete_cb,
                                       void * ccast_state);

/**
 * \brief Selectivly Broadcast a message which will initiate a convergecast. 
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
 * \param tocores           Cores to send this bcast to
 * \param payload           The payload
 * \param size              Size of the payload
 * \param ccast_complete_cb Callback used to return result of convergecast
 * \param ccast_state       State associated with this ccast and passed to
 *                          ccast_complete_cb and ccast_recv (on this core)
 */
errval_t routing_send_sel_bcast_with_ccast(routeid_t id, coremask_t tocores,
                                           uint8_t *payload, size_t size, 
                                           ccast_complete_func_t ccast_complete_cb,
                                           void * ccast_state);
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
                            uint8_t *payload, size_t size);

#endif // ROUTING_H
