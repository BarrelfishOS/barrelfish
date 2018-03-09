/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>

#include "queue_manager.h"

struct device_queue *device_queues[QUEUE_TYPE_MAX] = {0};


errval_t dqm_init_queue(struct capref dev, uint64_t id, queue_t type,
                        struct device_queue *q)
{
    errval_t err;

    memset(q, 0, sizeof(*q));

    q->devcap = dev;
    q->id = id;
    q->type = type;

    err = slot_alloc(&q->domcap);
    if (err_is_fail(err)) {
        return err;
    }

    err = vnode_create(q->domcap, ObjType_VNode_x86_64_pml4);
    if (err_is_fail(err)) {
        slot_free(q->domcap);
        return err;
    }

    err = driverkit_iommu_create_domain(q->domcap, dev);
    if (err_is_fail(err)) {
        cap_destroy(q->domcap);
        return err;
    }

    err = driverkit_iommu_add_device(q->domcap, dev);
    if (err_is_fail(err)) {
        driverkit_iommu_delete_domain(q->domcap);
        cap_destroy(q->domcap);
        return err;
    }

    return SYS_ERR_OK;
}


errval_t dqm_add_queue(struct device_queue *q)
{
    assert(q->type < QUEUE_TYPE_MAX);

    q->prev = NULL;
    q->next = NULL;

    device_queues[q->type]->prev = q;
    q->next = device_queues[q->type];
    device_queues[q->type] = q;

    return SYS_ERR_OK;
}

static void dqm_remove_queue(struct device_queue *q)
{
    if (q == device_queues[q->type]) {
        device_queues[q->type] = q->next;
    }

    if (q->next) {
        q->next->prev = q->prev;
    }

    if (q->prev) {
        q->prev->next = q->next;
    }

    q->next = NULL;
    q->prev = NULL;
}

errval_t dqm_alloc_by_id(queue_t type, uint64_t id, struct device_queue **rq)
{
    assert(type < QUEUE_TYPE_MAX);

    struct device_queue *q = device_queues[type];
    while(q) {
        if (q->id == id) {
            break;
        }
        q = q->next;
    }

    if (q == NULL) {
        return -1;
    }

    dqm_remove_queue(q);

    q->allocated = true;

    *rq = q;

    return SYS_ERR_OK;

}

errval_t dqm_alloc_queue(queue_t type, struct device_queue **rq)
{
    struct device_queue *q = device_queues[type];
    if (q == NULL) {
        return -1;
    }

    dqm_remove_queue(q);

    q->allocated = true;

    *rq = q;

    return SYS_ERR_OK;
}

errval_t dqm_free_queue(struct device_queue *q)
{
    assert(q->allocated);

    q->allocated = false;
    q->binding = NULL;

    return dqm_add_queue(q);
}


errval_t dqm_queue_manager_init(void)
{
    /*
     * TODO: export services etc..
     */
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}