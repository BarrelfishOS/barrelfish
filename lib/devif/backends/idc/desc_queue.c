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
#include <if/descq_data_defs.h>
#include <if/descq_ctrl_defs.h>
#include <if/descq_ctrl_rpcclient_defs.h>
#include "../../queue_interface_internal.h"
#include "descq_debug.h"


struct __attribute__((aligned(DESCQ_ALIGNMENT))) desc {
    regionid_t rid; // 4
    bufferid_t bid; // 8
    lpaddr_t addr; // 16
    size_t len; // 24
    uint64_t flags; // 32
    uint64_t seq; // 40
    uint8_t pad[24];
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
    struct descq_data_binding* data;
    struct descq_ctrl_binding* ctrl;
    struct descq_ctrl_rpc_client* rpc;

    // linked list
    struct descq* next;
    uint64_t qid;
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
 * @param buffer_id             Buffer id of the buffer
 * @param base                  Physical address of hte buffer
 * @param len                   Lenght of the buffer
 * @param misc_flags            Miscellaneous flags
 *
 * @returns error if queue is full or SYS_ERR_OK on success
 */
static errval_t descq_enqueue(struct devq* queue,
                              regionid_t region_id,
                              bufferid_t buffer_id,
                              lpaddr_t base,
                              size_t len,
                              uint64_t misc_flags)
{
    struct descq* q = (struct descq*) queue;
    size_t head = q->tx_seq % q->slots;
    if ((q->tx_seq - q->tx_seq_ack->value) > (q->slots-1)) {
        return DEVQ_ERR_TX_FULL;
    }
    
    q->tx_descs[head].rid = region_id;
    q->tx_descs[head].bid = buffer_id;
    q->tx_descs[head].addr = base;
    q->tx_descs[head].len = len;
    q->tx_descs[head].flags = misc_flags;
    q->tx_descs[head].seq = q->tx_seq;    

    // only write local head
    q->tx_seq++;

    DESCQ_DEBUG("tx_seq=%lu tx_seq_ack=%lu bid=%d\n", 
                    q->tx_seq, q->tx_seq_ack->value, buffer_id);
    return SYS_ERR_OK;
}
/**
 * @brief Dequeue a descriptor (as seperate fields) 
 *        from the descriptor queue
 *
 * @param q                     The descriptor queue
 * @param region_id             Return pointer to the region id of 
 *                              the denqueued buffer
 * @param buffer_id             Return pointer to the buffer id of the buffer
 * @param base                  Return pointer to the physical address 
 *                              of the buffer
 * @param len                   Return pointer to the lenght of the buffer
 * @param misc_flags            Return pointer to miscellaneous flags
 *
 * @returns error if queue is empty or SYS_ERR_OK on success
 */
static errval_t descq_dequeue(struct devq* queue,
                              regionid_t* region_id,
                              bufferid_t* buffer_id,
                              lpaddr_t* base,
                              size_t* len,
                              uint64_t* misc_flags)
{
    struct descq* q = (struct descq*) queue;
    uint64_t seq = q->rx_descs[q->rx_seq % q->slots].seq;   
    
    if (!(q->rx_seq == seq)) {
        return DEVQ_ERR_RX_EMPTY;
    }

    size_t tail = q->rx_seq % q->slots;
    *region_id = q->rx_descs[tail].rid;
    *buffer_id = q->rx_descs[tail].bid;
    *base = q->rx_descs[tail].addr;
    *len = q->rx_descs[tail].len;
    *misc_flags = q->rx_descs[tail].flags;
 
       
    q->rx_seq++;
    q->rx_seq_ack->value = q->rx_seq;

    DESCQ_DEBUG("rx_seq_ack=%lu bid=%d \n", q->rx_seq_ack->value, *buffer_id);
    return SYS_ERR_OK;
}

static void resend_notify(void* a)
{
    errval_t err;
    struct descq* queue = (struct descq*) a;
    err = queue->data->tx_vtbl.notify(queue->data, NOP_CONT);
}

static errval_t descq_notify(struct devq* q)
{
    errval_t err;
    //errval_t err2;
    struct descq* queue = (struct descq*) q;
    /*
    DESCQ_DEBUG("start \n");
    err = queue->rpc->vtbl.notify(queue->rpc, &err2);
    err = err_is_fail(err) ? err : err2;
    DESCQ_DEBUG("end\n");
    */
    err = queue->data->tx_vtbl.notify(queue->data, NOP_CONT);
    if (err_is_fail(err)) {
        while(err_is_fail(err)) {
            err = queue->data->register_send(queue->data, get_default_waitset(), 
                                             MKCONT(resend_notify, queue));
            if (err_is_fail(err)) {
                event_dispatch(get_default_waitset());
            }
        }
    }
    return SYS_ERR_OK;
}

static errval_t descq_control(struct devq* q, uint64_t cmd,
                              uint64_t value)
{
    errval_t err, err2;
    struct descq* queue = (struct descq*) q;

    DESCQ_DEBUG("start \n");
    err = queue->rpc->vtbl.control(queue->rpc, cmd, value, &err2);
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
    err = queue->rpc->vtbl.register_region(queue->rpc, cap, rid, &err2);
    err = err_is_fail(err) ? err : err2;
    DESCQ_DEBUG("end\n");
    return err;
}

static errval_t descq_deregister(struct devq* q, regionid_t rid)
{
    errval_t err, err2;
    struct descq* queue = (struct descq*) q;

    err = queue->rpc->vtbl.deregister_region(queue->rpc, rid, &err2);
    err = err_is_fail(err) ? err : err2;
    return err;
}

/*
 * Flounder interface implementation
 */

static void mp_notify(struct descq_data_binding* b) {
    
    DESCQ_DEBUG("start \n");
    errval_t err;    
    struct descq* q = (struct descq*) b->st;

    DESCQ_DEBUG("%p \n",q->f.notify);
    err = q->f.notify(q);

    DESCQ_DEBUG("end\n");
    assert(err_is_ok(err));
}


static void mp_reg(struct descq_ctrl_binding* b, struct capref cap,
                   uint32_t rid) 
{
    DESCQ_DEBUG("start \n");
    errval_t err;    
    struct descq* q = (struct descq*) b->st;    

    err = devq_add_region((struct devq*) q, cap, rid);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.register_region_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }

    err = q->f.reg(q, cap, rid);

    err = b->tx_vtbl.register_region_response(b, NOP_CONT, err);
    DESCQ_DEBUG("end \n");
    assert(err_is_ok(err));
}

static void mp_dereg(struct descq_ctrl_binding* b, uint32_t rid) 
{
    errval_t err;    
    struct descq* q = (struct descq*) b->st;

    err = devq_remove_region((struct devq*) q, rid);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.deregister_region_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }

    err = q->f.dereg(q, rid);

    err = b->tx_vtbl.deregister_region_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void mp_control(struct descq_ctrl_binding* b, uint64_t cmd,
                       uint64_t value) 
{
    errval_t err;    
    struct descq* q = (struct descq*) b->st;

    err = q->f.control(q, cmd, value);

    err = b->tx_vtbl.control_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void mp_destroy(struct descq_ctrl_binding* b) 
{
    errval_t err;    
    struct descq* q = (struct descq*) b->st;

    err = q->f.destroy(q);
    
    USER_PANIC("Destroy NYI \n");

    err = b->tx_vtbl.destroy_queue_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void mp_create(struct descq_ctrl_binding* b, uint32_t slots,
                      struct capref rx, struct capref tx) {
    
    struct descq* q = (struct descq*) b->st;
    DESCQ_DEBUG("start %p\n",q);
    errval_t err;    
    
    // switch RX/TX for correct setup
    err = vspace_map_one_frame_attr((void**) &(q->rx_descs),
                                    slots*DESCQ_ALIGNMENT, tx, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        goto end;
    }

    err = vspace_map_one_frame_attr((void**) &(q->tx_descs),
                                    slots*DESCQ_ALIGNMENT, rx, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        goto end;
    }
 
    q->tx_seq_ack = (void*)q->tx_descs;
    q->rx_seq_ack = (void*)q->rx_descs;
    q->tx_descs++;
    q->rx_descs++;
    q->slots = slots-1;

    devq_init(&q->q, true);

    q->q.f.enq = descq_enqueue;
    q->q.f.deq = descq_dequeue;
    q->q.f.notify = descq_notify;
    q->q.f.reg = descq_register;
    q->q.f.dereg = descq_deregister;
    q->q.f.ctrl = descq_control;
     
    err = q->f.create(q);
    err = SYS_ERR_OK;

end:
    err = b->tx_vtbl.create_queue_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
    DESCQ_DEBUG("end \n");
}

static struct descq_ctrl_rx_vtbl ctrl_rx_vtbl = {
    .create_queue_call = mp_create,
    .destroy_queue_call = mp_destroy,
    .register_region_call = mp_reg,
    .deregister_region_call = mp_dereg,
    .control_call = mp_control,
};

static struct descq_data_rx_vtbl data_rx_vtbl = {
    .notify = mp_notify,
};

static void ctrl_export_cb(void *st, errval_t err, iref_t iref)
{
    struct descq_endpoint_state* q = (struct descq_endpoint_state*) st;
    const char* suffix = "_ctrl";
    char name[strlen(q->name)+strlen(suffix)+1];
    
    sprintf(name, "%s%s", q->name, suffix);
    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    q->exp_done = true;
    // state is only function pointers
    DESCQ_DEBUG("Control interface exported (%s)\n", name);
}

static void data_export_cb(void *st, errval_t err, iref_t iref)
{
    struct descq_endpoint_state* q = (struct descq_endpoint_state*) st;
    const char* suffix = "_data";
    char name[strlen(q->name)+strlen(suffix)+1];
    
    sprintf(name, "%s%s", q->name, suffix);
    err = nameservice_register(name, iref);
    DESCQ_DEBUG("Data interface exported (%s)\n", name);
    assert(err_is_ok(err));
}

static errval_t data_connect_cb(void *st, struct descq_data_binding* b)
{

    struct descq_endpoint_state* state = (struct descq_endpoint_state*) st;
    struct descq* q = state->tail;
    b->rx_vtbl = data_rx_vtbl;
    b->st = q;
    q->data = b;
    DESCQ_DEBUG("New connection data %p q->data %p \n", q, q->data);
    return SYS_ERR_OK;
}

static errval_t ctrl_connect_cb(void *st, struct descq_ctrl_binding* b)
{
    //errval_t err;
    struct descq* q;
    struct descq_endpoint_state* state = (struct descq_endpoint_state*) st;
    // Allocate state
    q = malloc(sizeof(struct descq));
    assert(q != NULL);
    q->ctrl = b;

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

    b->rx_vtbl = ctrl_rx_vtbl;
    b->st = q;

    return SYS_ERR_OK;
}


static void ctrl_bind_cb(void *st, errval_t err, struct descq_ctrl_binding* b)

{

    struct descq* q = (struct descq*) st;
    DESCQ_DEBUG("Control interface bound \n");
    q->ctrl = b;
    q->rpc = malloc(sizeof(struct descq_ctrl_rpc_client));
    assert(q->rpc != NULL); 

    err = descq_ctrl_rpc_client_init(q->rpc, b);
    assert(err_is_ok(err));

    b->rx_vtbl = ctrl_rx_vtbl;
    b->st = q;
}

static void data_bind_cb(void *st, errval_t err, struct descq_data_binding* b)

{
    struct descq* q = (struct descq*) st;
    q->data = b;
    b->rx_vtbl = data_rx_vtbl;
    DESCQ_DEBUG("Data interface bound\n");
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
                      struct descq_func_pointer* f)
{
    DESCQ_DEBUG("create start\n");
    errval_t err;
    struct descq* tmp;

    // Init basic struct fields
    tmp = malloc(sizeof(struct descq));
    assert(tmp != NULL);

    if (exp) {
        struct descq_endpoint_state* state = malloc(sizeof(struct descq_endpoint_state));
        state->name = malloc(sizeof(strlen(name)));
        strncpy(state->name, name, strlen(name));

        state->f.notify = f->notify;
        state->f.dereg = f->dereg;
        state->f.reg = f->reg;
        state->f.create = f->create;
        state->f.destroy = f->destroy;
        state->f.control = f->control;

        err = descq_data_export(state, data_export_cb, data_connect_cb, 
                                get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            free(tmp->name);
            free(tmp);
            return err;
        }

        err = descq_ctrl_export(state, ctrl_export_cb, ctrl_connect_cb, 
                                get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            free(tmp->name);
            free(tmp);
            return err;
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
        struct capref rx;
        struct capref tx;
        size_t bytes;
        err = frame_alloc(&rx, DESCQ_ALIGNMENT*slots, &bytes);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }

        assert(bytes >= DESCQ_ALIGNMENT*slots);

        err = frame_alloc(&tx, DESCQ_ALIGNMENT*slots, &bytes);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }
        assert(bytes >= DESCQ_ALIGNMENT*slots);

        err = vspace_map_one_frame_attr((void**) &(tmp->rx_descs),
                                        slots*DESCQ_ALIGNMENT, rx, 
                                        VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }

        err = vspace_map_one_frame_attr((void**) &(tmp->tx_descs),
                                        slots*DESCQ_ALIGNMENT, tx, 
                                        VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }

        memset(tmp->tx_descs, 0, slots*DESCQ_ALIGNMENT);
        memset(tmp->rx_descs, 0, slots*DESCQ_ALIGNMENT);

        tmp->bound_done = false;
        iref_t iref;

        const char *suffix_ctrl = "_ctrl";
        char name_ctrl[strlen(name)+strlen(suffix_ctrl)+1];
        sprintf(name_ctrl, "%s%s", name, suffix_ctrl);

        err = nameservice_blocking_lookup(name_ctrl, &iref);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }

        err = descq_ctrl_bind(iref, ctrl_bind_cb, tmp, get_default_waitset(),
                              IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }
 
        const char *suffix_data = "_data";
        char name_data[strlen(name)+strlen(suffix_data)+1];
        sprintf(name_data, "%s%s", name, suffix_data);
        iref_t iref2;
   
        err = nameservice_blocking_lookup(name_data, &iref2);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }
    
        err = descq_data_bind(iref2, data_bind_cb, tmp, get_default_waitset(),
                              IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            free(tmp);
            return err;
        }

        while(!tmp->bound_done) {
            event_dispatch(get_default_waitset());
        }

        errval_t err2;
        err = tmp->rpc->vtbl.create_queue(tmp->rpc, slots, rx, tx, &err2);
        if (err_is_fail(err) || err_is_fail(err2)) {
            err = err_is_fail(err) ? err: err2;
            return err;
        }

        tmp->tx_seq_ack = (void*)tmp->tx_descs;
        tmp->rx_seq_ack = (void*)tmp->rx_descs;
        tmp->tx_descs++;
        tmp->rx_descs++;
        tmp->slots = slots-1;

        devq_init(&tmp->q, false);

        tmp->q.f.enq = descq_enqueue;
        tmp->q.f.deq = descq_dequeue;
        tmp->q.f.notify = descq_notify;
        tmp->q.f.reg = descq_register;
        tmp->q.f.dereg = descq_deregister;
        tmp->q.f.ctrl = descq_control;

        tmp->name = malloc(sizeof(strlen(name)));
        strncpy(tmp->name, name, strlen(name));

    }


    *q = tmp;

    DESCQ_DEBUG("create end %p \n", *q);
    return SYS_ERR_OK;
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
