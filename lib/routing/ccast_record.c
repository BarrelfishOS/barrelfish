/**
 * \file
 * \brief ccast record management for the routing library
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
#include <if/routing_defs.h>
#include "internal.h"

static recordid_t alloc_id(struct route * route) 
{          
    coreid_t coreid = disp_get_core_id();
    assert (coreid < 0xff);
    assert (++(route->records_count) < 0xffffff); // XXX TODO - deal with rollover
    
    return (coreid << 24) | route->records_count;
}
     
struct record * alloc_new_record(struct route * route, 
                                 ccast_complete_func_t ccast_complete_cb,
                                 coreid_t replies_expected) 
{
    struct record * record;
    if (route->static_record_used) {
        record = malloc(sizeof (struct record));
    } else {
        route->static_record_used = true;
        record = route->static_record;
    }
    assert (record);

    record->id                = alloc_id(route);
    record->replies_expected  = replies_expected;
    record->parent            = NULL;
    record->ccast_complete_cb = ccast_complete_cb;

    // add to linked list
    record->prev = NULL;
    record->next = route->records;
    route->records = record;
    
    return record;
}


struct record * alloc_record_from_bcast(struct route * route, 
                                        struct neighbor * parent,
                                        recordid_t record_id,
                                        coreid_t replies_expected) 
{
    struct record * record;
    if (route->static_record_used) {
        record = malloc(sizeof (struct record));
    } else {
        route->static_record_used = true;
        record = route->static_record;
    }
    assert (record);
    
    record->id                = record_id;
    record->replies_expected  = replies_expected;
    record->parent            = parent;
    record->ccast_complete_cb = NULL;

    // add to linked list
    record->prev = NULL;
    record->next = route->records;
    route->records = record;
    
    return record;
}

struct record * get_record_struct(struct route * route, 
                                  recordid_t ccast_record_id)
{
    struct record * record = route->records;

    while (record != NULL) {
        if (record->id == ccast_record_id) {
            return record;
        }
        record = record->next;
    }

    sys_print("Could not find record\n", 23);
    return NULL;
}

void free_record(struct route * route, struct record * record) {
     
    // remove from linked list
    if (route->records == record) {
        assert(record->prev == NULL);
        route->records = record->next;
    } else if (record->prev != NULL) {
        record->prev->next = record->next;
    }
    if (record->next != NULL) {
        record->next->prev = record->prev;
    }

    // free record
    if (record == route->static_record) {
        assert(route->static_record_used);
        route->static_record_used = false;
    } else {
        free(record);
    }
}
