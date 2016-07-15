#include <barrelfish/barrelfish.h>
#include <assert.h>
#include <devif/queue.h>

#include "blk_ahci.h"
#include "ahci_dev.h" // TODO: get rid of this include
#include "../dma_mem/dma_mem.h"
#include "../blk_debug.h"

static bool is_valid_buffer(struct dev_queue* dq, size_t slot)
{
    return !capref_is_null(dq->buffers[slot].frame);
}

static errval_t request_slot_alloc(struct dev_queue* dq, size_t* slot)
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

    return DEV_ERR_QUEUE_FULL;
}

static errval_t get_port(struct ahci_disk* hba, size_t port_num, struct ahci_port** p) {
    assert(hba != NULL);
    assert(port_num < MAX_AHCI_PORTS);
    errval_t err = SYS_ERR_OK;

    struct ahci_port* port = &hba->ports[port_num];
    if (!port->is_initialized) {
        return err_push(err, DEV_ERR_NOT_INITIALIZED);
    }

    *p = port;
    return SYS_ERR_OK;
}

static errval_t init_queue(struct dev_queue** dq, struct ahci_port *port) {
    struct dev_queue* queue = calloc(1, sizeof(struct dev_queue));
    if (dq == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    queue->port = port;
    for (size_t i = 0; i< MAX_BUFFERS; i++) {
        queue->buffers[i].frame = NULL_CAP;
    }

    *dq = queue;
    return SYS_ERR_OK;
}

static bool slice_is_in_range(struct dma_mem *mem, lpaddr_t base, size_t length)
{
    bool lower_bound = mem->paddr <= base;
    bool upper_bound = mem->paddr+mem->bytes >= base+length;

    return lower_bound && upper_bound;
}

static uint64_t flags_get_block(uint64_t flags)
{
    return flags & ((1ULL<<49) - 1);
}

static bool flags_is_write(uint64_t flags) {
    return (flags & (1ULL << 63)) > 0;
}

void devq_interrupt_handler(void* q);
void devq_interrupt_handler(void* q)
{
    if (q == NULL) {
        BLK_DEBUG("Ignored interrupt, device not yet initialized?\n");
        return;
    }
    struct dev_queue *queue = q;
    struct ahci_port *port = queue->port;

    assert(port->interrupt != NULL);
    port->interrupt(port, queue);
}

errval_t devq_create(void* st, char *device_name, uint64_t flags, void **queue)
{
    errval_t err = SYS_ERR_OK;

    struct ahci_port* port;
    err = get_port(st, flags, &port);
    if (err_is_fail(err)) {
        return err;
    }

    struct dev_queue *dq;
    err = init_queue(&dq, port);
    if (err_is_fail(err)) {
        return err;
    }

    *queue = dq;

    return err;
}

errval_t devq_destroy(void *qp)
{
    struct dev_queue *queue = qp;

    // TODO: Wait for stuff to finish...!

    // Clean-up memory:
    for (size_t i = 0; i< MAX_BUFFERS; i++) {
        dma_mem_free(&queue->buffers[i]);
    }
    free(qp);
    return SYS_ERR_OK;
}

errval_t devq_enqueue(void *q, regionid_t region_id, lpaddr_t base, size_t length, bufferid_t buffer_id, uint64_t flags)
{
    struct dev_queue *queue = q;
    assert(region_id < MAX_BUFFERS);
    assert(is_valid_buffer(queue, region_id));
    assert(base != 0);
    assert(length >= 512);

    struct dma_mem* mem = &queue->buffers[region_id];

    if (!slice_is_in_range(mem, base, length)) {
        return DEV_ERR_INVALID_BUFFER_ARGS;
    }

    size_t slot;
    errval_t err = request_slot_alloc(queue, &slot);
    if (err_is_fail(err)) {
        return err;
    }

    struct dev_queue_request *dqr = &queue->requests[slot];
    dqr->status = RequestStatus_InProgress;
    dqr->buffer_id = buffer_id;
    dqr->base = base;
    dqr->length = length;
    dqr->region_id = region_id;
    dqr->command_slot = slot;

    uint64_t block = flags_get_block(flags);
    bool write = flags_is_write(flags);
    return blk_ahci_port_dma_async(queue->port, slot, block, base, length, write);
}

errval_t devq_dequeue(void *q,
                      regionid_t* region_id,
                      lpaddr_t* base,
                      size_t* length,
                      bufferid_t* buffer_id)
{
    assert(q != NULL);
    assert(region_id != NULL);
    assert(base != NULL);
    assert(length != NULL);
    assert(length != NULL);

    struct dev_queue *queue = q;

    for (size_t i=0; i<queue->port->ncs; i++) {
        struct dev_queue_request *dqr = &queue->requests[i];
        if (dqr->status == RequestStatus_Done) {
            *base = dqr->base;
            *length = dqr->length;
            *buffer_id = dqr->buffer_id;
            *region_id = dqr->region_id;

            dqr->status = RequestStatus_Unused;
            return dqr->error;
        }
    }

    return DEV_ERR_QUEUE_EMPTY;
}

errval_t devq_register(void *q,
                       struct capref cap,
                       regionid_t* region_id)
{
    errval_t err = DEV_ERR_REGISTER_BUFFER;
    assert(!capref_is_null(cap));
    struct dev_queue *queue = q;

    for (size_t i=0; i<MAX_BUFFERS; i++) {
        if (is_valid_buffer(q, i)) {
            printf("Don't overwrite existing buffer\n");
            continue;
        }

        struct dma_mem* mem = &queue->buffers[i];
        err = dma_mem_from_capref(cap, mem);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "call failed");
            return err_push(err, DEV_ERR_REGISTER_BUFFER);
        }

        *region_id = i;
        return SYS_ERR_OK;
    }

    return err;
}

errval_t devq_remove(void *q, regionid_t region_id)
{
    assert(region_id < MAX_BUFFERS);
    assert(q != NULL);
    struct dev_queue *queue = q;

    struct dma_mem* mem = &queue->buffers[region_id];
    assert(!capref_is_null(mem->frame));

    return dma_mem_free(mem);
}

errval_t devq_sync(void *q)
{
    return SYS_ERR_OK;
}

errval_t devq_control(void *q, uint64_t request, uint64_t value)
{
    return SYS_ERR_OK;
}
