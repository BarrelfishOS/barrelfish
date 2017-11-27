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
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <devif/queue_interface.h>
#include <devif/queue_interface_backend.h>
#include <devif/backends/null.h>

struct null_q {
    struct devq my_q;
    struct devq* q;
};

static errval_t null_register(struct devq* q, struct capref cap,
                              regionid_t rid) 
{
    struct null_q* que = (struct null_q*) q;
    return que->q->f.reg(que->q, cap, rid);
}

static errval_t null_deregister(struct devq* q, regionid_t rid) 
{
    struct null_q* que = (struct null_q*) q;
    return que->q->f.dereg(que->q, rid);
}


static errval_t null_control(struct devq* q, uint64_t cmd, uint64_t value,
                             uint64_t* ret)
{
    struct null_q* que = (struct null_q*) q;
    return que->q->f.ctrl(que->q, cmd, value, ret);
}


static errval_t null_notify(struct devq* q)
{
    struct null_q* que = (struct null_q*) q;
    return que->q->f.notify(que->q);
}

static errval_t null_enqueue(struct devq* q, regionid_t rid, genoffset_t offset,
                             genoffset_t len, genoffset_t valid_data,
                             genoffset_t valid_length, uint64_t flags)
{
    struct null_q* que = (struct null_q*) q;
    return que->q->f.enq(que->q, rid, offset, len, valid_data, valid_length, flags);
}

static errval_t null_dequeue(struct devq* q, regionid_t* rid, genoffset_t* offset,
                             genoffset_t* len, genoffset_t* valid_data,
                             genoffset_t* valid_length, uint64_t* flags)
{
    struct null_q* que = (struct null_q*) q;
    return que->q->f.deq(que->q, rid, offset, len, valid_data, valid_length, flags);
}

static errval_t null_destroy(struct devq* devq)
{
    free(devq);    

    // TODO cleanup
    return SYS_ERR_OK;
}

/**
 * Public functions
 *
 */

errval_t null_create(struct null_q** q, struct devq* other_q)
{
    errval_t err;
    *q = malloc(sizeof(struct null_q));
    assert(*q);

    (*q)->q = other_q;
    err = devq_init(&(*q)->my_q, false);
    if (err_is_fail(err)) {
        return err;
    }   

    (*q)->my_q.f.reg = null_register;
    (*q)->my_q.f.dereg = null_deregister;
    (*q)->my_q.f.ctrl = null_control;
    (*q)->my_q.f.notify = null_notify;
    (*q)->my_q.f.enq = null_enqueue;
    (*q)->my_q.f.deq = null_dequeue;
    (*q)->my_q.f.destroy = null_destroy;
    return SYS_ERR_OK;
}

