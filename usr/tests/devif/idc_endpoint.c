/*
 * Copyright (c) 2017 ETH Zurich.
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
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>
#include <devif/backends/descq.h>
#include <devif/backends/debug.h>
#include <if/devif_test_defs.h>


static uint16_t qid = 0;

static struct ele* list = NULL;
static struct ele* end = NULL;
static struct devq* descq;
static struct devq* debug;


struct ele {
    struct  descq* q;
    uint16_t qid;
    struct ele* next;
};

static errval_t create(struct descq* q, uint64_t* queue_id)
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

    // stack debug queue on top
    errval_t err;
    err = debug_create((struct debug_q**) &debug, (struct devq*) q);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating debug q failed \n");
    }
   
    qid++;
    return SYS_ERR_OK;
}

static errval_t destroy(struct descq* q)
{
    return SYS_ERR_OK;
}

static errval_t rpc_request_ep(struct devif_test_binding* b, coreid_t core, 
                               errval_t* err, struct capref* cap)
{
    *err = slot_alloc(cap);
    if (err_is_fail(*err)) {
        return *err;
    }

    *err = descq_create_ep((struct descq*) descq, core, cap);
    if (err_is_fail(*err)) { 
        slot_free(*cap);
        return *err;
    }

    return *err;
}

static void request_ep(struct devif_test_binding* b, coreid_t core)
{
    errval_t err, err2;
    struct capref ep;
    err = rpc_request_ep(b, core, &err2, &ep);
    err = b->tx_vtbl.request_ep_response(b, NOP_CONT, err, ep);
    assert(err_is_ok(err));
}

static errval_t notify(struct descq* q)
{

    //struct devq* queue = (struct devq*) q;
    struct devq* queue = (struct devq*) debug;
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
    return debug_add_region((struct debug_q*) debug, cap, rid);
}


static errval_t dereg(struct descq* q, regionid_t rid)
{
    return debug_remove_region((struct debug_q*) debug, rid);
}


static errval_t control(struct descq* q, uint64_t cmd, uint64_t value, uint64_t* res)
{
    return SYS_ERR_OK;
}

static struct devif_test_rpc_rx_vtbl rpc_rx_vtbl = {
    .request_ep_call = rpc_request_ep,
};

static struct devif_test_rx_vtbl rx_vtbl = {
    .request_ep_call = request_ep,
};


static void export_cb(void *st, errval_t err, iref_t iref)
{
    printf("exported devif_test_ep: interface\n");
    err = nameservice_register("devif_test_ep", iref);
    assert(err_is_ok(err));
}


static errval_t connect_cb(void *st, struct devif_test_binding *b)
{
    printf("New connection on devif_test_ep interface\n");
    b->rx_vtbl = rx_vtbl;
    b->rpc_rx_vtbl = rpc_rx_vtbl;
    b->st = st;
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

    err = descq_create((struct descq**)&descq, DESCQ_DEFAULT_SIZE, "test_queue", 
                       true, &id, f);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating debug q failed \n");
    }
    
    err = devif_test_export(&descq, export_cb, connect_cb, get_default_waitset(), 
                            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC("Exporting devif_test failed\n");
    }

    while(true) {
        event_dispatch(get_default_waitset());
    }
}

