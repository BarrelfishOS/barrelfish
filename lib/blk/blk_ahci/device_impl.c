#include <barrelfish/barrelfish.h>
#include <assert.h>
#include <devif/queue_interface.h>
#include <devif/backends/blk/ahci_devq.h>

#include "blk_ahci.h"
#include "ahci_dev.h" // TODO: get rid of this include
#include "../dma_mem/dma_mem.h"
#include "../blk_debug.h"
#include "../../devif/queue_interface_internal.h"

struct ahci_queue {
    struct devq q;
    struct ahci_port* port;
    struct dma_mem buffers[MAX_BUFFERS];
    struct dev_queue_request requests[MAX_REQUESTS];
};


static bool is_valid_buffer(struct ahci_queue* dq, size_t slot)
{
    return !capref_is_null(dq->buffers[slot].frame);
}

static errval_t request_slot_alloc(struct ahci_queue* dq, size_t* slot)
{
    assert(dq->port->ncs <= MAX_REQUESTS);

    for (size_t i=0; i < dq->port->ncs; i++) {
        struct dev_queue_request *dqr = &dq->requests[i];
        if (dqr->status == RequestStatus_Unused) {
            dqr->status = RequestStatus_InProgress;
            *slot = i;
            return SYS_ERR_OK;
        }
    }

    return DEVQ_ERR_QUEUE_FULL;
}

static errval_t get_port(struct ahci_disk* hba, size_t port_num, struct ahci_port** p) {
    assert(hba != NULL);
    assert(port_num < MAX_AHCI_PORTS);
    errval_t err = SYS_ERR_OK;

    struct ahci_port* port = &hba->ports[port_num];
    if (!port->is_initialized) {
        return err_push(err, DEVQ_ERR_INIT_QUEUE);
    }

    *p = port;
    return SYS_ERR_OK;
}

static errval_t init_queue(struct ahci_queue** dq) {
    struct ahci_queue* queue = calloc(1, sizeof(struct ahci_queue));
    if (dq == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    for (size_t i = 0; i< MAX_BUFFERS; i++) {
        queue->buffers[i].frame = NULL_CAP;
    }

    *dq = queue;
    return SYS_ERR_OK;
}

static bool slice_is_in_range(struct dma_mem *mem, genpaddr_t offset, size_t length)
{
    // do not have to check lower bound since it is unsigned
    bool upper_bound = (mem->bytes >= length);
    bool upper_bound2 = (mem->paddr + offset + length) <= (mem->paddr + mem->bytes);
   // printf("mem->paddr %lx, mem->bytes %lx, offset %lx, length %lx \n",
   //        mem->paddr, mem->bytes, offset, length);
    return upper_bound2 && upper_bound;
}

static uint64_t flags_get_block(uint64_t flags)
{
    return flags & ((1ULL<<49) - 1);
}

static bool flags_is_write(uint64_t flags) {
    return (flags & (1ULL << 63)) > 0;
}

void ahci_interrupt_handler(void* q)
{
    if (q == NULL) {
        BLK_DEBUG("Ignored interrupt, device not yet initialized?\n");
        return;
    }
    struct ahci_queue *queue = q;
    struct ahci_port *port = queue->port;

    assert(port->interrupt != NULL);
    port->interrupt(port, queue->requests, port->ncs);
}

errval_t ahci_destroy(struct ahci_queue *q)
{
    // TODO: Wait for stuff to finish...!

    // Clean-up memory:
    for (size_t i = 0; i< MAX_BUFFERS; i++) {
        dma_mem_free(&q->buffers[i]);
    }
    free(q);
    return SYS_ERR_OK;
}

static errval_t ahci_enqueue(struct devq *q,
                             regionid_t region_id,
                             genoffset_t offset,
                             genoffset_t length,
                             genoffset_t valid_data,
                             genoffset_t valid_length,
                             uint64_t flags)
{
    struct ahci_queue *queue = (struct ahci_queue*) q;
    
    assert(is_valid_buffer(queue, (region_id % MAX_BUFFERS)));
    assert(length >= 512);

    struct dma_mem* mem = &queue->buffers[(region_id % MAX_BUFFERS)];

    if (!slice_is_in_range(mem, offset, length)) {
        return DEVQ_ERR_INVALID_BUFFER_ARGS;
    }

    size_t slot = 0;
    
    errval_t err = request_slot_alloc(queue, &slot);
    if (err_is_fail(err)) {
        return err;
    }

    struct dev_queue_request *dqr = &queue->requests[slot];
    dqr->status = RequestStatus_InProgress;
    dqr->region_id = region_id;
    dqr->offset = offset;
    dqr->length = length;
    dqr->valid_data = valid_data;
    dqr->valid_length = valid_length;
    dqr->command_slot = slot;

    uint64_t block = flags_get_block(flags);
    bool write = flags_is_write(flags);

    err = blk_ahci_port_dma_async(queue->port, slot, block, mem->paddr+offset,
                                  length, write);
    return err;
}

static errval_t ahci_dequeue(struct devq* q,
                             regionid_t* region_id,
                             genoffset_t* offset,
                             genoffset_t* length,
                             genoffset_t* valid_data,
                             genoffset_t* valid_length,
                             uint64_t* misc_flags)
{
    assert(q != NULL);
    assert(region_id != NULL);
    assert(offset != NULL);
    assert(valid_data != NULL);
    assert(valid_length != NULL);
    assert(length != NULL);

    struct ahci_queue *queue = (struct ahci_queue*) q;

    for (size_t i=0; i < queue->port->ncs; i++) {
        struct dev_queue_request *dqr = &queue->requests[i];
        if (dqr->status == RequestStatus_Done) {
            *region_id = dqr->region_id;
            *offset = dqr->offset;
            *length = dqr->length;
            *valid_data = dqr->valid_data;
            *valid_length = dqr->valid_length;
            dqr->status = RequestStatus_Unused;
            return dqr->error;
        }
    }

    return DEVQ_ERR_QUEUE_EMPTY;
}

static errval_t ahci_register(struct devq *q,
                              struct capref cap,
                              regionid_t region_id)
{

    errval_t err = DEVQ_ERR_REGISTER_REGION;
    assert(!capref_is_null(cap));
    struct ahci_queue *queue = (struct ahci_queue*) q;

    for (size_t i=0; i<MAX_BUFFERS; i++) {
        uint16_t slot = ((region_id+i) % MAX_BUFFERS);

        if (is_valid_buffer(queue, slot)) {
            printf("Don't overwrite existing buffer\n");
            continue;
        }
        
        queue->buffers[slot].frame = cap;
        
        struct dma_mem* mem = &queue->buffers[slot];
        err = dma_mem_from_capref(cap, mem);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "call failed");
            return err_push(err, DEVQ_ERR_REGISTER_REGION);
        }
        return SYS_ERR_OK;
    }

    return err;
}

static errval_t ahci_deregister(struct devq *q, regionid_t region_id)
{
    assert(q != NULL);
    struct ahci_queue *queue = (struct ahci_queue*) q;

    struct dma_mem* mem = &queue->buffers[(region_id % MAX_BUFFERS)];
    assert(!capref_is_null(mem->frame));

    return dma_mem_free(mem);
}

static errval_t ahci_notify(struct devq *q)
{
    return SYS_ERR_OK;
}

static errval_t ahci_control(struct devq *q, uint64_t request, uint64_t value,
                             uint64_t *result)
{
    return SYS_ERR_OK;
}

errval_t ahci_create(struct ahci_queue** q, void* st, uint64_t flags)
{
    errval_t err = SYS_ERR_OK;

    struct ahci_port* port = NULL;
    err = get_port(st, flags, &port);
    if (err_is_fail(err)) {
        return err;
    }

    struct ahci_queue *dq;
    err = init_queue(&dq);
    if (err_is_fail(err)) {
        return err;
    }

    dq->port = port;

    dq->q.f.enq = ahci_enqueue;
    dq->q.f.deq = ahci_dequeue;
    dq->q.f.reg = ahci_register;
    dq->q.f.dereg = ahci_deregister;
    dq->q.f.ctrl = ahci_control;
    dq->q.f.notify = ahci_notify;

    err = devq_init(&dq->q, false);
    if (err_is_fail(err)) {
        return err;
    }

    *q = dq;

    return err;
}
