/*
 * Copyright (c) 2016 ETH Zurich.
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

#include "../../queue_interface_internal.h"

#define LOOPBACK_QUEUE_SIZE 128

struct loopback_queue
{
    struct devq q;
    struct devq_buf queue[LOOPBACK_QUEUE_SIZE];
    size_t head;
    size_t tail;

};

/*
// Loopback device functions
errval_t devq_loopback_setup(uint32_t coreid, uint64_t flags,   
                             uint64_t *features, uint32_t* default_qsize, 
                             uint32_t* default_bufsize, bool* reconnect, 
                             char* name)
{
    *features = 0;
    *default_qsize = 0;
    *default_bufsize = 0;
    *reconnect = false;
    name = "loopback";
    return SYS_ERR_OK;
}
*/

static errval_t loopback_enqueue(struct devq* q, regionid_t rid, genoffset_t offset,
                             genoffset_t length, genoffset_t valid_data,
                             genoffset_t valid_length, uint64_t flags)
{
    struct loopback_queue *lq = (struct loopback_queue *)q;

    size_t head_next = lq->head + 1;
    if (head_next == LOOPBACK_QUEUE_SIZE) {
        head_next = 0;
    }


    if (head_next == lq->tail) {
        return DEVQ_ERR_QUEUE_FULL;
    }

    lq->queue[head_next].offset = offset; // 8
    lq->queue[head_next].length = length; // 16
    lq->queue[head_next].valid_data = valid_data; // 24
    lq->queue[head_next].valid_length = valid_length; // 32
    lq->queue[head_next].flags = flags; // 40
    lq->queue[head_next].rid = rid; // 44

    lq->head = head_next;

    return SYS_ERR_OK;
}

static errval_t loopback_dequeue(struct devq* q, regionid_t* rid,
                                 genoffset_t* offset, genoffset_t* length,
                                 genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    struct loopback_queue *lq = (struct loopback_queue *)q;

    size_t tail_next = lq->tail + 1;
    if (tail_next == LOOPBACK_QUEUE_SIZE) {
        tail_next = 0;
    }

    if (tail_next == lq->head) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }

    *offset = lq->queue[tail_next].offset; // 8
    *length = lq->queue[tail_next].length; // 16
    *valid_data = lq->queue[tail_next].valid_data; // 24
    *valid_length  =lq->queue[tail_next].valid_length; // 32
    *flags  = lq->queue[tail_next].flags; // 40
    *rid = lq->queue[tail_next].rid; // 44

    lq->tail = tail_next;

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

    lq->head = 1;
    lq->tail = 0;

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
