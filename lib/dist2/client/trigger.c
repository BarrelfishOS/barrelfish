/**
 * \file
 * \brief Handler function for asynchronous triggers sent by server.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include "trigger.h"
#include <barrelfish/threads.h>

void trigger_handler(struct dist2_binding* b, uint64_t t, uint64_t st,
        char* record)
{
    assert(t != 0);
    trigger_handler_fn trigger_fn = (trigger_handler_fn) t;
    void* state = (void*) st;

    trigger_fn(record, state);
}

/*
 struct dist_trigger {
 trigger_handler_fn trigger;
 void* state;
 uint64_t version;
 };

 struct thread_mutex trigger_mutex;

 static struct dist_trigger trigger_table[MAX_TRIGGERS] = { { NULL, NULL, 0 } };

 static struct dist_trigger* find_free_slot(size_t* id)
 {
 for (size_t i = 1; i < MAX_TRIGGERS; i++) {
 if (trigger_table[i].trigger == NULL) {
 *id = i;
 return &trigger_table[i];
 }
 }

 *id = 0;
 return NULL;
 }

 // TODO locking for concurrent access!
 static errval_t dist_register_trigger(trigger_handler_fn trigger, void* st,
 size_t* id)
 {
 assert(trigger != NULL);

 thread_mutex_lock(&trigger_mutex);
 struct dist_trigger* t = find_free_slot(id);
 if (t == NULL) {
 return DIST2_ERR_NO_TRIGGER_SLOT;
 }

 debug_printf("set trigger at id: %lu\n", *id);
 t->state = st;
 t->trigger = trigger;
 t->version++;
 //assert(t->version == 1);
 thread_mutex_unlock(&trigger_mutex);

 return SYS_ERR_OK;
 }

 errval_t dist_unregister_trigger(dist2_trigger_t t)
 {
 assert(t.id < MAX_TRIGGERS);

 thread_mutex_lock(&trigger_mutex);
 trigger_table[t.id].trigger = NULL;
 trigger_table[t.id].state = NULL;
 thread_mutex_unlock(&trigger_mutex);

 return SYS_ERR_OK;
 }*/
