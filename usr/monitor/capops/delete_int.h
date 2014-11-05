/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPOPS_DELETE_INT_H
#define CAPOPS_DELETE_INT_H

#include <capops.h>
#include <barrelfish/waitset.h>
#include <barrelfish/event_queue.h>

struct waitset *delete_steps_get_waitset(void);
void delete_steps_init(struct waitset *ws);
void delete_steps_trigger(void);
void delete_steps_pause(void);
void delete_steps_resume(void);

struct delete_queue_node {
    struct event_queue_node qn;
    struct delete_queue_node *next;
    struct event_closure cont;
};

void delete_queue_wait(struct delete_queue_node *qn,
                       struct event_closure cont);
void delete_queue_init(struct waitset *ws);

struct delete_st {
    struct delete_queue_node qn;
    struct event_queue_node lock_qn;
    struct domcapref capref;
    struct capability cap;
    struct capref newcap;
    bool wait;
    delete_result_handler_t result_handler;
    void *st;
};

void capops_delete_int(struct delete_st *del_st);

void send_new_ram_cap(struct capref cap);

#endif
