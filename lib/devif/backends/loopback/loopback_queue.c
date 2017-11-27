/*
 * Copyright (c) 2016,2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/backends/loopback_devif.h>
#include <devif/queue_interface_backend.h>

#define LOOPBACK_QUEUE_SIZE 256

struct loopback_queue
{
    struct devq q;
    struct devq_buf queue[LOOPBACK_QUEUE_SIZE];
    size_t head;
    size_t tail;
    size_t num_ele;
};

static errval_t loopback_enqueue(struct devq* q, regionid_t rid, genoffset_t offset,
                             genoffset_t length, genoffset_t valid_data,
                             genoffset_t valid_length, uint64_t flags)
{
    struct loopback_queue *lq = (struct loopback_queue *)q;


    if (lq->num_ele == LOOPBACK_QUEUE_SIZE) {
        //debug_printf("enqueue: head=%lu tail=%lu full\n", lq->head, lq->tail);
        return DEVQ_ERR_QUEUE_FULL;
    }

    //debug_printf("enqueue: head=%lu tail=%lu \n", lq->head, lq->tail);
    lq->queue[lq->head].offset = offset; // 8
    lq->queue[lq->head].length = length; // 16
    lq->queue[lq->head].valid_data = valid_data; // 24
    lq->queue[lq->head].valid_length = valid_length; // 32
    lq->queue[lq->head].flags = flags; // 40
    lq->queue[lq->head].rid = rid; // 44

    lq->head = (lq->head + 1) % LOOPBACK_QUEUE_SIZE;
    lq->num_ele++;

    return SYS_ERR_OK;
}

static errval_t loopback_dequeue(struct devq* q, regionid_t* rid,
                                 genoffset_t* offset, genoffset_t* length,
                                 genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    struct loopback_queue *lq = (struct loopback_queue *)q;

    if (lq->num_ele == 0) {
        //debug_printf("dequeue: head=%lu tail=%lu emtpy\n", lq->head, lq->tail);
        return DEVQ_ERR_QUEUE_EMPTY;
    }

    //debug_printf("dequeue: head=%lu tail=%lu \n", lq->head, lq->tail);

    *offset = lq->queue[lq->tail].offset; // 8
    *length = lq->queue[lq->tail].length; // 16
    *valid_data = lq->queue[lq->tail].valid_data; // 24
    *valid_length  =lq->queue[lq->tail].valid_length; // 32
    *flags  = lq->queue[lq->tail].flags; // 40
    *rid = lq->queue[lq->tail].rid; // 44

    lq->tail = (lq->tail + 1) % LOOPBACK_QUEUE_SIZE;
    lq->num_ele--;
    return SYS_ERR_OK;
}


static errval_t loopback_notify(struct devq *q)
{

#if 0
        err = devq_dequeue(q, &(buf.rid), &(buf.addr), 
                           &(buf.len), &(buf.bid), &(buf.flags));
        if (err_is_fail(err))  {
            return err;
        }   

        err = devq_enqueue(q, buf.rid, buf.addr, buf.len,
                           buf.flags, &buf.bid);
        if (err_is_fail(err))  {
            return err;
        }   
#endif
   

    return SYS_ERR_OK;
}

static errval_t loopback_register(struct devq *q, struct capref cap,
                                regionid_t region_id)
{
    return SYS_ERR_OK;
}

static errval_t loopback_deregister(struct devq *q, regionid_t region_id)
{
    return SYS_ERR_OK;
}

static errval_t loopback_control(struct devq *q,
                                 uint64_t request,
                                 uint64_t value,
                                 uint64_t *result)
{
    // TODO Might have some options for loopback device?
    return SYS_ERR_OK;
}


static errval_t loopback_destroy(struct devq* q)
{
    free((struct loopback_queue*)q);
    return SYS_ERR_OK;
}

errval_t loopback_queue_create(struct loopback_queue** q)
{
    errval_t err;

    struct loopback_queue *lq = calloc(1, sizeof(struct loopback_queue));
    if (lq == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    err = devq_init(&lq->q, false);
    if (err_is_fail(err)) {
        free(lq);
        return err;
    }

    lq->head = 0;
    lq->tail = 0;
    lq->num_ele = 0;

    lq->q.f.enq = loopback_enqueue;
    lq->q.f.deq = loopback_dequeue;
    lq->q.f.reg = loopback_register;
    lq->q.f.dereg = loopback_deregister;
    lq->q.f.ctrl = loopback_control;
    lq->q.f.notify = loopback_notify;
    lq->q.f.destroy = loopback_destroy;

    *q = lq;

    return SYS_ERR_OK;
}
