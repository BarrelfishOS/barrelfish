/**
 * \file
 * \brief Routing library internals
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ROUTING_INTERNAL_H
#define ROUTING_INTERNAL_H

#include <string.h>

#define ROUTE_SERVICE_NAME_BASE "route_"

// XXX get rid of this limit...
#define MAX_ROUTES 128


typedef genvaddr_t linkid_t;
typedef genvaddr_t neighborid_t;

struct neighbor {
    coreid_t coreid;   ///< Core id of the neighbor
    coremask_t mask;   ///< A mask of the cores reachable from this neighbor
    iref_t iref;       ///< Iref of neighbor
    bool bind_done;    ///< Used when binding to neighbors
    bool connected;    ///< Are we connected to this neighbor
    linkid_t linkid;   ///< neighbor's link id for this connection
    struct routing_binding *binding;    ///< Binding object
};

struct record;
struct record {
    recordid_t id;             ///< Id of the record provided by to application
    coreid_t replies_expected; ///< Number of ccast replies expected
    struct neighbor *parent;   ///< Parent to respond to with ccast
    ccast_complete_func_t ccast_complete_cb; ///< handler to call when ccast is complete
    void * user_state;
    // records are held in a linked list
    struct record * next;
    struct record * prev;
};

struct route {
    routeid_t id;                       ///< Route id
    void * user_state;                  ///< State provided by user application
    struct routing_cb_vtbl vtbl;        ///< Handler functions
    bool use_nameservice;               ///< If true, will use nameservice to connect bindings automatically.

    coremask_t bcast_reachable_mask;    ///< All cores reachable by bcast on this route
    coremask_t bcast_nexthop_mask;      ///< All cores on next hop of this route
    struct neighbor ** bcast_neighbors; ///< Ordered list of bcast neighbors
    coreid_t bcast_neighbors_count;     ///< Number of neighbors

    struct routing_binding * bindings[MAX_CPUS];  ///< bindings that are already setups
    iref_t my_iref;                     ///< iref of my routing binding
    struct record *records;             ///< List of active ccast records on this route
    uint32_t records_count;             ///< This cores current record_id_cnt
    struct record static_record[1];     ///< Static record to avoid constant alloc/free
    bool static_record_used;            ///< True if static record is in use
    bool exported;                      ///< Is the route exported yet
    joined_cb_func_t joined_cb;        ///< Called when route is joined
    connected_cb_func_t connected_cb;  ///< Called when connected to neighbors
    uint32_t waiting_for_connection;    ///< Number of connect_responces outstanding
};

struct link {
    struct routing_binding * p_bind;
    coreid_t remaining_responses;   
    linkid_t p_linkid;
    neighborid_t p_neighborid;
    coremask_t bcast_reachable_mask;   ///< All cores reachable by bcast on this link
    coremask_t bcast_nexthop_mask;     ///< All cores on next hop of this link
    struct neighbor ** bcast_children; ///< Ordered list of bcast children for this link
    coreid_t bcast_children_count;     ///< Number of bcast children

    struct neighbor *ccast_parent;     ///< Parent to send ccast response to 
                                       ///< (may be different from bcast parent)
};

/* ---------------------------------- ACCESSORS ---------------------------- */


static inline struct link* get_link_struct(linkid_t id)
{
    return (struct link *)(uintptr_t)id;
}

static inline linkid_t get_link_id(struct link* link)
{
    return (linkid_t)(uintptr_t)link;
}

static inline struct neighbor* get_neighbor_struct(neighborid_t id)
{
    return (struct neighbor *)(uintptr_t)id;
}

static inline neighborid_t get_neighbor_id(struct neighbor* neighbor)
{
    return (neighborid_t)(uintptr_t)neighbor;
}

static inline void get_service_name(char * service_name, coreid_t coreid, 
                                    routeid_t route_id) 
{
    strncpy(service_name, ROUTE_SERVICE_NAME_BASE,
            sizeof(ROUTE_SERVICE_NAME_BASE));
    char local_id[32];
    snprintf(local_id, sizeof(local_id), "%d_%d", 
             coreid, route_id);
    strcat(service_name, local_id);
}

/**
 * \brief Returns true if coreid is in mask
 */
static inline bool mask_cmp(coreid_t coreid, coremask_t mask)
{
    return mask & ((coremask_t)1 << coreid);
}

/**
 * \brief Count
 */
static inline coreid_t mask_count(coremask_t mask)
{
    coreid_t ret = 0;  
    // TODO, do this in a smarter way
    while (mask) {
        ret += (mask & 0x1);
        mask = mask >> 1;
    }
    return ret;
}
/* ------------------------- Internal routing functions -------------------- */

/* internal.c */
void recv_ccast_msg(struct routing_binding *b, routeid_t routeid, 
                    recordid_t recordid, uint8_t *payload, size_t size);
void recv_ccast_fixmsg(struct routing_binding *b, routeid_t routeid, 
                       recordid_t recordid, uint64_t w1, uint64_t w2,
                       uint8_t size);
errval_t init_route(routeid_t id, struct routing_cb_vtbl handlers,
                    void * handler_state, joined_cb_func_t joined_cb,
                    bool use_nameservice);
errval_t connect_neighbors(struct route *route, struct neighbor **neighbors, 
                           coreid_t neighbors_count, coreid_t initiating_core,
                           linkid_t mylinkid, bool return_link);
void set_route_struct(routeid_t id, struct route * route);
struct route* get_route_struct(routeid_t id);

/* ccast_record.c */
struct record * alloc_new_record(
                   struct route *route, ccast_complete_func_t ccast_complete_cb,
                   coreid_t replies_expected);
struct record * alloc_record_from_bcast(
                   struct route * route, struct neighbor * parent,
                   uint32_t record_id, coreid_t replies_expected);
struct record * get_record_struct(struct route * route, 
                                  recordid_t ccast_record_id);
void free_record(struct route * route, struct record * record);

#endif // ROUTING_INTERNAL_H
