/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>
#include <devif/backends/descq.h>
#include <if/descq_defs.h>
#include "../../queue_interface_internal.h"
#include "descq_debug.h"


struct __attribute__((aligned(DESCQ_ALIGNMENT))) desc {
    genoffset_t offset; // 8
    genoffset_t length; // 16
    genoffset_t valid_data; // 24
    genoffset_t valid_length; // 32
    uint64_t flags; // 40
    uint64_t seq; // 48
    regionid_t rid; // 52
    uint8_t pad[12];
};

union __attribute__((aligned(DESCQ_ALIGNMENT))) pointer {
    volatile size_t value;
    uint8_t pad[64];
};


struct descq {
    struct devq q;
    struct descq_func_pointer f;

    // General info
    size_t slots;
    char* name;
    bool bound_done;
 
    // Descriptor Ring
    struct desc* rx_descs;
    struct desc* tx_descs;
    
    // Flow control
    uint64_t rx_seq;
    uint64_t tx_seq;
    union pointer* rx_seq_ack;
    union pointer* tx_seq_ack;
  
    // Flounder
    struct descq_binding* binding;
    bool local_bind;
    bool lmp_bind;
    bool ump_bind;
    uint64_t resend_args;
  
    // linked list
    struct descq* next;
    uint64_t qid;
    
    bool notifications;
};

struct descq_endpoint_state {
    bool exp_done;
    char* name;
    struct descq_func_pointer f;
    struct descq* head;
    struct descq* tail;
    uint64_t qid;
};


/**
 * @brief Enqueue a descriptor (as seperate fields)
 *        into the descriptor queue
 *
 * @param q                     The descriptor queue
 * @param region_id             Region id of the enqueued buffer
 * @param offset                Offset into the region where the buffer resides
 * @param length                Length of the buffer
 * @param valid_data            Offset into the region where the valid data
 *                              of the buffer resides
 * @param valid_length          Length of the valid data of the buffer
 * @param misc_flags            Miscellaneous flags
 *
 * @returns error if queue is full or SYS_ERR_OK on success
 */
static errval_t descq_enqueue(struct devq* queue,
                              regionid_t region_id,
                              genoffset_t offset,
                              genoffset_t length,
                              genoffset_t valid_data,
                              genoffset_t valid_length,
                              uint64_t misc_flags)
{
    struct descq* q = (struct descq*) queue;
    size_t head = q->tx_seq % q->slots;
    if ((q->tx_seq - q->tx_seq_ack->value) > (q->slots-1)) {
        return DEVQ_ERR_QUEUE_FULL;
    }
    
 
    //assert(length > 0);

    q->tx_descs[head].rid = region_id;
    q->tx_descs[head].offset = offset;
    q->tx_descs[head].length = length;
    q->tx_descs[head].valid_data = valid_data;
    q->tx_descs[head].valid_length = valid_length;
    q->tx_descs[head].flags = misc_flags;

    __sync_synchronize();

    q->tx_descs[head].seq = q->tx_seq;

    // only write local head
    q->tx_seq++;

    DESCQ_DEBUG("tx_seq=%lu tx_seq_ack=%lu \n",
                    q->tx_seq, q->tx_seq_ack->value);
    // if (q->local_bind) {
    //q->binding->tx_vtbl.notify(q->binding, NOP_CONT);
    // }
    return SYS_ERR_OK;
}

/**
 * @brief Dequeue a descriptor (as seperate fields)
 *        from the descriptor queue
 *
 * @param q                     The descriptor queue
 * @param region_id             Return pointer to the region id of
 *                              the denqueued buffer
 * @param offset                Return pointer to the offset into the region
 *                              where the buffer resides
 * @param length                Return pointer to the length of the buffer
 * @param valid_data            Return pointer to the offset into the region
 *                              where the valid data of the buffer resides
 * @param valid_lenght          Return pointer to the length of the valid
 *                              data of the buffer
 * @param misc_flags            Return pointer to miscellaneous flags
 *
 * @returns error if queue is empty or SYS_ERR_OK on success
 */
static errval_t descq_dequeue(struct devq* queue,
                              regionid_t* region_id,
                              genoffset_t* offset,
                              genoffset_t* length,
                              genoffset_t* valid_data,
                              genoffset_t* valid_length,
                              uint64_t* misc_flags)
{
    struct descq* q = (struct descq*) queue;
    uint64_t seq = q->rx_descs[q->rx_seq % q->slots].seq;
    
    if (!(q->rx_seq == seq)) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }

    size_t tail = q->rx_seq % q->slots;
    *region_id = q->rx_descs[tail].rid;
    *offset = q->rx_descs[tail].offset;
    *length = q->rx_descs[tail].length;
    *valid_data = q->rx_descs[tail].valid_data;
    *valid_length = q->rx_descs[tail].valid_length;
    *misc_flags = q->rx_descs[tail].flags;

    //assert(*length > 0);       

    q->rx_seq++;
    q->rx_seq_ack->value = q->rx_seq;

    DESCQ_DEBUG("rx_seq_ack=%lu\n", q->rx_seq_ack->value);
    return SYS_ERR_OK;
}

static void resend_notify(void* a)
{
    errval_t err;
    struct descq* queue = (struct descq*) a;
    err = queue->binding->tx_vtbl.notify(queue->binding, NOP_CONT);
}

static errval_t descq_notify(struct devq* q)
{
    errval_t err;
    //errval_t err2;
    struct descq* queue = (struct descq*) q;

    err = queue->binding->tx_vtbl.notify(queue->binding, NOP_CONT);
    if (err_is_fail(err)) {
        
        err = queue->binding->register_send(queue->binding, get_default_waitset(),
                                            MKCONT(resend_notify, queue));
        if (err == LIB_ERR_CHAN_ALREADY_REGISTERED) {
            // dont care about this failure since there is an oustanding message
            // anyway if this fails 
            return SYS_ERR_OK;
        } else {
            return err;     
        }
    }
    return SYS_ERR_OK;
}

static errval_t descq_control(struct devq* q, uint64_t cmd,
                              uint64_t value, uint64_t *result)
{
    errval_t err, err2;
    struct descq* queue = (struct descq*) q;

    DESCQ_DEBUG("start \n");
    err = queue->binding->rpc_tx_vtbl.control(queue->binding, cmd, value, result, &err2);
    err = err_is_fail(err) ? err : err2;
    DESCQ_DEBUG("end\n");
    return err;
}

static errval_t descq_register(struct devq* q, struct capref cap,
                               regionid_t rid)
{
    errval_t err, err2;
    struct descq* queue = (struct descq*) q;

    DESCQ_DEBUG("start %p\n", queue);
    err = queue->binding->rpc_tx_vtbl.register_region(queue->binding, cap, rid, &err2);
    err = err_is_fail(err) ? err : err2;
    DESCQ_DEBUG("end\n");
    return err;
}

static void try_deregister(void* a)
{
    errval_t err, err2;
    struct descq* queue = (struct descq*) a;
    
    err = queue->binding->rpc_tx_vtbl.deregister_region(queue->binding, queue->resend_args, 
                                                        &err2);
    assert(err_is_ok(err2) && err_is_ok(err));
}


static errval_t descq_deregister(struct devq* q, regionid_t rid)
{
    errval_t err, err2;
    err2 = SYS_ERR_OK;
    struct descq* queue = (struct descq*) q;

    err = queue->binding->rpc_tx_vtbl.deregister_region(queue->binding, rid, &err2);
    if (err_is_fail(err)) {
        queue->resend_args = rid;
        while(err_is_fail(err)) {
            err = queue->binding->register_send(queue->binding, get_default_waitset(),
                                                MKCONT(try_deregister, queue));
            if (err_is_fail(err)) {
                event_dispatch(get_default_waitset());
            }
        }
    }
    return err2;
}

/*
 * Flounder interface implementation
 */

static void mp_notify(struct descq_binding* b) {
    DESCQ_DEBUG("start \n");
    errval_t err;
    struct descq* q = (struct descq*) b->st;

    DESCQ_DEBUG("%p \n",q->f.notify);
    err = q->f.notify(q);

    DESCQ_DEBUG("end\n");
    assert(err_is_ok(err));
}


static errval_t mp_reg(struct descq_binding* b, struct capref cap,
                   uint32_t rid, errval_t *err)
{
    DESCQ_DEBUG("start \n");
    struct descq* q = (struct descq*) b->st;

    *err = devq_add_region((struct devq*) q, cap, rid);
    if (err_is_fail(*err)) {
        return SYS_ERR_OK;
    }

    *err = q->f.reg(q, cap, rid);
    DESCQ_DEBUG("end \n");
    return SYS_ERR_OK;
}

static errval_t mp_dereg(struct descq_binding* b, uint32_t rid,
                         errval_t *err)
{
    struct descq* q = (struct descq*) b->st;

    *err = devq_remove_region((struct devq*) q, rid);
    if (err_is_fail(*err)) {
        return SYS_ERR_OK;
    }

    *err = q->f.dereg(q, rid);
    return SYS_ERR_OK;
}

static errval_t mp_control(struct descq_binding* b, uint64_t cmd,
                       uint64_t value, uint64_t *result, errval_t *err)
{
    struct descq* q = (struct descq*) b->st;

    *err = q->f.control(q, cmd, value, result);
    return SYS_ERR_OK;
}

static errval_t mp_destroy(struct descq_binding* b, errval_t *err)
{
    struct descq* q = (struct descq*) b->st;

    *err = q->f.destroy(q);
    
    USER_PANIC("Destroy NYI \n");
    return SYS_ERR_OK;
}

static errval_t mp_create(struct descq_binding* b, uint32_t slots,
        struct capref rx, struct capref tx, bool notifications, uint8_t role,
        errval_t *err, uint64_t *queue_id) {
    
    struct descq* q = (struct descq*) b->st;
    DESCQ_DEBUG("start %p\n",q);
    
    // switch RX/TX for correct setup
    *err = vspace_map_one_frame_attr((void**) &(q->rx_descs),
                                    slots*DESCQ_ALIGNMENT, tx,
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(*err)) {
        goto end2;
    }

    *err = vspace_map_one_frame_attr((void**) &(q->tx_descs),
                                    slots*DESCQ_ALIGNMENT, rx,
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(*err)) {
        goto end1;
    }
 
    q->tx_seq_ack = (void*)q->tx_descs;
    q->rx_seq_ack = (void*)q->rx_descs;
    q->tx_descs++;
    q->rx_descs++;
    q->slots = slots-1;
    q->rx_seq = 0;
    q->tx_seq = 0;

    devq_init(&q->q, true);

    q->q.f.enq = descq_enqueue;
    q->q.f.deq = descq_dequeue;
    q->q.f.notify = descq_notify;
    q->q.f.reg = descq_register;
    q->q.f.dereg = descq_deregister;
    q->q.f.ctrl = descq_control;
     
    *err = q->f.create(q, notifications, role, queue_id);
    if (err_is_ok(*err)) {
        goto end2;
    }

end1:
    *err = vspace_unmap(q->rx_descs);
    assert(err_is_ok(*err));
end2:
    DESCQ_DEBUG("end \n");
    return SYS_ERR_OK;
}

static struct descq_rpc_rx_vtbl rpc_rx_vtbl = {
    .create_queue_call = mp_create,
    .destroy_queue_call = mp_destroy,
    .register_region_call = mp_reg,
    .deregister_region_call = mp_dereg,
    .control_call = mp_control,
};

static struct descq_rx_vtbl rx_vtbl = {
    .notify = mp_notify,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    struct descq_endpoint_state* q = (struct descq_endpoint_state*) st;

    err = nameservice_register(q->name, iref);
    assert(err_is_ok(err));
    q->exp_done = true;
    // state is only function pointers
    DESCQ_DEBUG("Control interface exported (%s)\n", q->name);
}

static errval_t connect_cb(void *st, struct descq_binding* b)
{
    struct descq* q;
    struct descq_endpoint_state* state = (struct descq_endpoint_state*) st;
    // Allocate state
    q = malloc(sizeof(struct descq));
    if (q == NULL) {
        return DEVQ_ERR_DESCQ_INIT;
    }
    q->binding = b;

    q->qid = state->qid;
    state->qid++;
    q->next = NULL;

    q->f.create = state->f.create;
    q->f.notify = state->f.notify;
    q->f.destroy = state->f.destroy;
    q->f.control = state->f.control;
    q->f.reg = state->f.reg;
    q->f.dereg = state->f.dereg;

    if (state->head == NULL) {
        // allocated state
        state->head = q;
        state->tail = q;
    } else {
        state->tail->next = q;
        state->tail = q;
    }

    b->rpc_rx_vtbl = rpc_rx_vtbl;
    b->rx_vtbl = rx_vtbl;
    b->st = q;
    q->local_bind = b->local_binding != NULL;
    // if (q->local_bind) {
        q->ump_bind = false;
        q->lmp_bind = false;
    // } else {
    //     q->ump_bind = b->get_receiving_chanstate(b)->chantype == CHANTYPE_UMP_IN;
    //     q->lmp_bind = !q->ump_bind;
    // }

    return SYS_ERR_OK;
}


static void bind_cb(void *st, errval_t err, struct descq_binding* b)

{

    struct descq* q = (struct descq*) st;
    DESCQ_DEBUG("Interface bound \n");
    q->binding = b;
    b->rx_vtbl = rx_vtbl;
    descq_rpc_client_init(q->binding);

    q->bound_done = true;
    b->st = q;
}

/**
 * @brief initialized a descriptor queue
 */

errval_t descq_create(struct descq** q,
                      size_t slots,
                      char* name,
                      bool exp,
                      bool notifications,
                      uint8_t role,
                      uint64_t *queue_id,
                      struct descq_func_pointer* f)
{
    DESCQ_DEBUG("create start\n");
    errval_t err;
    struct descq* tmp;
    struct capref rx;
    struct capref tx;

    // Init basic struct fields
    tmp = malloc(sizeof(struct descq));
    assert(tmp != NULL);
    tmp->name = strdup(name);
    assert(tmp->name != NULL);

    if (exp) {  // exporting
        struct descq_endpoint_state* state = malloc(sizeof(struct descq_endpoint_state));
        state->name = strdup(name);
        assert(state->name);

        state->f.notify = f->notify;
        state->f.dereg = f->dereg;
        state->f.reg = f->reg;
        state->f.create = f->create;
        state->f.destroy = f->destroy;
        state->f.control = f->control;

        err = descq_export(state, export_cb, connect_cb,
                                get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            goto cleanup1;
        }

        while(!state->exp_done) {
            event_dispatch(get_default_waitset());
        }
    
    } else {

        tmp->f.notify = f->notify;
        tmp->f.dereg = f->dereg;
        tmp->f.reg = f->reg;
        tmp->f.create = f->create;
        tmp->f.destroy = f->destroy;
        tmp->f.control = f->control;
        size_t bytes;

        err = frame_alloc(&rx, DESCQ_ALIGNMENT*slots, &bytes);
        if (err_is_fail(err)) {
            goto cleanup1;
        }

        assert(bytes >= DESCQ_ALIGNMENT*slots);

        err = frame_alloc(&tx, DESCQ_ALIGNMENT*slots, &bytes);
        if (err_is_fail(err)) {
            goto cleanup2;
        }

        assert(bytes >= DESCQ_ALIGNMENT*slots);

        err = vspace_map_one_frame_attr((void**) &(tmp->rx_descs),
                                        slots*DESCQ_ALIGNMENT, rx,
                                        VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            goto cleanup3;
        }

        err = vspace_map_one_frame_attr((void**) &(tmp->tx_descs),
                                        slots*DESCQ_ALIGNMENT, tx,
                                        VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            goto cleanup4;
        }

        memset(tmp->tx_descs, 0, slots*DESCQ_ALIGNMENT);
        memset(tmp->rx_descs, 0, slots*DESCQ_ALIGNMENT);

        tmp->bound_done = false;
        iref_t iref;

        err = nameservice_blocking_lookup(name, &iref);
        if (err_is_fail(err)) {
            goto cleanup5;
        }

        err = descq_bind(iref, bind_cb, tmp, get_default_waitset(),
                              IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            goto cleanup5;
        }
 
        while(!tmp->bound_done) {
            event_dispatch(get_default_waitset());
        }

        tmp->local_bind = tmp->binding->local_binding != NULL;
        // if (tmp->local_bind) {
            tmp->ump_bind = false;
            tmp->lmp_bind = false;
        // } else {
        //     tmp->ump_bind = tmp->binding->get_receiving_chanstate(tmp->binding)->chantype == CHANTYPE_UMP_IN;
        //     tmp->lmp_bind = !tmp->ump_bind;
        // }
        
        errval_t err2;
        err = tmp->binding->rpc_tx_vtbl.create_queue(tmp->binding, slots, rx, tx,
            notifications, role, &err2, queue_id);
        if (err_is_fail(err) || err_is_fail(err2)) {
            err = err_is_fail(err) ? err: err2;
            goto cleanup5;
        }

        tmp->tx_seq_ack = (void*)tmp->tx_descs;
        tmp->rx_seq_ack = (void*)tmp->rx_descs;
        tmp->tx_seq_ack->value = 0;
        tmp->rx_seq_ack->value = 0;
        tmp->tx_descs++;
        tmp->rx_descs++;
        tmp->slots = slots-1;
        tmp->rx_seq = 0;
        tmp->tx_seq = 0;
        
        devq_init(&tmp->q, false);

        tmp->q.f.enq = descq_enqueue;
        tmp->q.f.deq = descq_dequeue;
        tmp->q.f.notify = descq_notify;
        tmp->q.f.reg = descq_register;
        tmp->q.f.dereg = descq_deregister;
        tmp->q.f.ctrl = descq_control;
        
        tmp->notifications = notifications;
    }


    *q = tmp;

    DESCQ_DEBUG("create end %p \n", *q);
    return SYS_ERR_OK;

cleanup5:
    vspace_unmap(tmp->rx_descs);
cleanup4:
    vspace_unmap(tmp->rx_descs);
cleanup3:
    cap_destroy(tx);
cleanup2:
    cap_destroy(rx);
cleanup1:
    free(tmp->name);
    free(tmp);

    return err;

}



/**
 * @brief Destroys a descriptor queue and frees its resources
 *
 * @param q                     The descriptor queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t descq_destroy(struct descq* q)
{
    errval_t err;
    err = vspace_unmap(q->tx_descs);
    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_unmap(q->rx_descs);
    if (err_is_fail(err)) {
        return err;
    }
    free(q->name);
    free(q);

    return SYS_ERR_OK;
}
