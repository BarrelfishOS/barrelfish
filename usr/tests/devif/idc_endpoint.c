/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/backends/descq.h>


static uint16_t qid = 0;

static struct ele* list = NULL;
static struct ele* end = NULL;

struct ele {
    struct  descq* q;
    uint16_t qid;
    struct ele* next;
};

static errval_t create(struct descq* q, bool notifications, uint8_t role,
                       uint64_t* queue_id)
{
    if (list == NULL) {
        list = malloc(sizeof(struct ele));
        list->q = q;
        list->qid = qid;
        list->next = NULL;
        end = list;
    } else {
        struct ele* item = malloc(sizeof(struct ele));
        item->q = q;
        item->qid = qid;
        item->next = NULL;
        end->next = item;
        end = item;
    }
    
    qid++;
    return SYS_ERR_OK;
}

static errval_t destroy(struct descq* q)
{
    return SYS_ERR_OK;
}


static errval_t notify(struct descq* q)
{

    struct devq* queue = (struct devq*) q;
    errval_t err = SYS_ERR_OK;
    //errval_t err2 = SYS_ERR_OK;
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;
    bool exit = false;
    uint16_t num_enq = 0;
    while(!exit) {
        err = devq_dequeue(queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &flags);
        if (err_is_fail(err)) {
            exit = true;
        } else {
           bool exit2 = false;
            while(!exit2) {
                err = devq_enqueue(queue, rid, offset, length, valid_data,
                                   valid_length, flags);
                if (err_is_ok(err)) {
                    exit2 = true;
                    num_enq++;
                }
            }
        }
    }

    if (num_enq > 0) {
        err = devq_notify(queue);
    } else {
        err = SYS_ERR_OK;
    }

    return err;
}

static errval_t reg(struct descq* q, struct capref cap,
                    regionid_t rid)
{
    return SYS_ERR_OK;
}


static errval_t dereg(struct descq* q, regionid_t rid)
{
    return SYS_ERR_OK;
}


static errval_t control(struct descq* q, uint64_t cmd, uint64_t value, uint64_t* res)
{
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    uint64_t id;
    errval_t err;
    struct descq_func_pointer* f = malloc(sizeof(struct descq_func_pointer));
    assert(f != NULL);

    f->notify = notify;
    f->create = create;
    f->destroy = destroy;
    f->reg = reg;
    f->dereg = dereg;
    f->control = control;

    struct descq* exp_queue;

    err = descq_create(&exp_queue, DESCQ_DEFAULT_SIZE, "test_queue", 
                       true, true, 0, &id, f);

    assert(err_is_ok(err));

    while(true) {
        event_dispatch(get_default_waitset());
    }
}

