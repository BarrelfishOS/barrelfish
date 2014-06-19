/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <virtio/virtio.h>
#include <virtio/virtio_ring.h>
#include <virtio/virtqueue.h>
#include <virtio/virtio_device.h>

#include "vbuffer.h"
#include "debug.h"

#define IS_POW2(num) (((num) != 0) && (((num) & (~(num) + 1)) == (num)))

#define VIRTQUEUE_FLAG_INDIRECT    1
#define VIRTQUEUE_FLAG_EVENT_IDX   2
#define VIRTQUEUE_FLAG_ADDED       13
#define VIRTQUEUE_FLAG_HAS_BUFFERS 14
#define VIRTQUEUE_FLAG_FREE_CAP    15

/**
 * this data structure stores additional information to the descriptors
 */
struct vring_desc_info
{
    struct virtio_buffer *buf;
    void *st;
    struct virtio_buffer_list *bl;
    uint8_t is_head;

};

/**
 * this data structure represents a VirtIO queue. It contains additional
 * information not stored with the vring structure
 */
struct virtqueue
{
    /* device information */
    struct virtio_device *device;   ///< pointer to to the virtio device
    uint16_t queue_index;           ///< index of this queue in the device
    char name[VIRTQUEUE_NAME_SIZE];  ///< name of the queue for debugging

    /* vring  information */
    struct vring vring;             ///< vring data structure
    struct capref vring_cap;        ///< capability of the vring data structure
    lvaddr_t vring_vaddr;           ///< virtual address of the vring in memory
    lpaddr_t vring_paddr;           ///< physical address of the vring
    lvaddr_t vring_align;           ///< the alignment of the vring

    uint16_t desc_num;              ///< number of descriptors of this vring
    uint16_t desc_num_max;          ///< maximum number of descriptors supported
    uint16_t desc_num_queued;       ///< number of queued used descriptors

    uint16_t free_head;             ///< head of the free descriptor chain
    uint16_t free_count;            ///< number of available free descriptors

    uint16_t used_tail;             ///< last consumed descriptor used table
    uint16_t used_head;             ///< caches the head of the used descriptors

    uint32_t flags;                 ///< flags


    /* interrupt handling */
    virtq_intr_hander_t intr_handler;   ///< interrupt handler
    void *intr_arg;                 ///< user argument for the handler

    struct virtio_buffer_allocator *buffer_alloc;
    uint8_t buffer_bits;
    uint8_t header_bits;

    struct vring_desc_info vring_di[0];  ///< array of additional desc information
#if 0
    /* indirect descriptors */
    uint16_t max_indirect;
    size_t indirect_size;
    struct vq_desc_extra {
        void *cookie; << virtual address?
        struct vring_desc *indirect;
        vm_paddr_t indirect_paddr;
        uint16_t ndescs;
    }vq_descx[0];
#endif
};

/**
 * \brief sets the interrupt threshold to num_desc processed descriptors
 *
 * \param vq        virtqueue to enable the interrupts
 * \param num_desc  the interrupt threshold
 *
 * \returns 1 if the interrupts have been enabled
 *          0 if the interrupts have not been enabled
 */
static bool virtqueue_interrupt_enable(struct virtqueue *vq,
                                       uint16_t num_desc)
{
    if (vq->flags & (1 << VIRTQUEUE_FLAG_EVENT_IDX)) {
        uint16_t *used_event = vring_get_used_event(&vq->vring);
        *used_event = vq->used_tail + num_desc;
    } else {
        vq->vring.avail->flags &= ~VIRTIO_RING_AVAIL_F_NO_INTERRUPT;
    }

    assert(!"NYI: memory barrier mb()");

    if (virtio_virtqueue_get_num_used(vq) > num_desc) {
        return 1;
    }

    return 0;
}

/**
 * \brief initializes the vring structure of the virtqueue
 *
 * \param vq virtqueue of the vring to initialize
 */
static void virtqueue_init_vring(struct virtqueue *vq)
{
    struct vring *vr = &vq->vring;

    assert(vq);
    assert(vq->desc_num);
    assert(vq->vring_vaddr);

    /*
     * initialize the vring structure in memory
     */
    vring_init(vr, vq->desc_num, vq->vring_align, (void *) vq->vring_vaddr);

    vr->num = vq->desc_num;

    /*
     * initialize the descriptor chains
     */
    uint32_t i;
    for (i = 0; i < vq->desc_num; ++i) {
        vr->desc[i].next = i + 1;
    }
    vr->desc[i].next = VIRTQUEUE_CHAIN_END;

}

/**
 * \brief initializes the indirect descriptors
 *
 * \param vq the virtqueue to initialize the indirect descriptors
 * \param size the number of indirect descriptors
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t virtqueue_init_indirect(struct virtqueue *vq,
                                        uint16_t size)
{
    struct virtio_device *vdev = vq->device;

    /*
     * check if the device supports indirect descriptors first
     */
    if (virtio_device_has_feature(vdev, VIRTIO_RING_F_INDIRECT_DESC)) {
        VIRTIO_DEBUG_VQ("Device does not support indirect descriptors\n");
        return SYS_ERR_OK;
    }

    assert(!"NYI: virtqueue_init_indirect");

    return SYS_ERR_OK;
}

static bool virtqueue_should_notify_host(struct virtqueue *vq)
{
    uint16_t new, prev, *event_idx;

    if (vq->flags & (1 << VIRTQUEUE_FLAG_EVENT_IDX)) {
        new = vq->vring.avail->idx;
        prev = new - vq->desc_num_queued;
        event_idx = vring_get_avail_event(&vq->vring);

        return (vring_need_event(*event_idx, new, prev) != 0);
    }

    return ((vq->vring.used->flags & VIRTIO_RING_USED_F_NO_NOTIFY) == 0);
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/*
 * ----------------------------------------------------------------------------
 *  Virtqueue Allocation / Deallocation
 */

/**
 * \brief allocates and initiates a new virtqueue structure
 *
 * \param setup  pointer to the setup information
 * \param vq     pointer where to store the new virtqueue pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_virtqueue_alloc(struct virtqueue_setup *setup,
                                struct virtqueue **vq)
{
    errval_t err;

    VIRTIO_DEBUG_VQ("Allocating VQ(%u) of size %u with buffer of %u bits\n",
                    setup->queue_id,
                    setup->vring_ndesc,
                    setup->buffer_bits);

    assert(vq);

    if (setup->vring_ndesc == 0 || !IS_POW2(setup->vring_ndesc)) {
        VIRTIO_DEBUG_VQ("ERROR: invalid size: %u\n", setup->vring_ndesc);
        return VIRTIO_ERR_SIZE_INVALID;
    }

    size_t size = vring_size(setup->vring_ndesc, setup->vring_align);
    size = ROUND_UP(size, BASE_PAGE_SIZE);

    if (setup->buffer_bits) {
        size += setup->vring_ndesc * (1UL << setup->buffer_bits);
    }

    if (setup->header_bits) {
        size += setup->vring_ndesc * (1UL << setup->header_bits);
    }

    struct capref vring_cap;
    size_t framesize;
    err = frame_alloc(&vring_cap, size, &framesize);
    if (err_is_fail(err)) {
        return err;
    }

    VIRTIO_DEBUG_VQ("Allocated memory for vring: [0x%lx & 0x%lx]\n",
                    (uint64_t )size,
                    (uint64_t )framesize);

    err = virtio_virtqueue_alloc_with_caps(setup, vring_cap, vq);
    if (err_is_fail(err)) {
        cap_destroy(vring_cap);
        return err;
    }

    /* set the flag that we have allocated the cap, so that it gets free'd */
    (*vq)->flags |= (1 << VIRTQUEUE_FLAG_FREE_CAP);

    return SYS_ERR_OK;
}

/**
 * \brief allocates and initiates a new virtqueue structure
 *
 * \param setup     pointer to the setup information
 * \param vring_cap capability to be used for the vring
 * \param vq        pointer where to store the new virtqueue pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_virtqueue_alloc_with_caps(struct virtqueue_setup *setup,
                                          struct capref vring_cap,
                                          struct virtqueue **ret_vq)
{
    errval_t err;

    assert(ret_vq);

    if (setup->vring_ndesc == 0 || !IS_POW2(setup->vring_ndesc)) {
        VIRTIO_DEBUG_VQ("ERROR: invalid size: %u\n", setup->vring_ndesc);
        return VIRTIO_ERR_SIZE_INVALID;
    }

    if (setup->max_indirect > VIRTIO_RING_MAX_INDIRECT) {
        VIRTIO_DEBUG_VQ("ERROR: too many indirect descriptors requested: [%u / %u]\n",
                        setup->vring_ndesc,
                        VIRTIO_RING_MAX_INDIRECT);
        return VIRTIO_ERR_MAX_INDIRECT;
    }

    setup->vring_align = VIRTQUEUE_ALIGNMENT;

    assert(!capref_is_null(vring_cap));

    struct frame_identity id;
    err = invoke_frame_identify(vring_cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    size_t vring_mem_size = vring_size(setup->vring_ndesc, setup->vring_align);
    vring_mem_size = ROUND_UP(vring_mem_size, BASE_PAGE_SIZE);

    if (setup->buffer_bits) {
        vring_mem_size += setup->vring_ndesc * (1UL << setup->buffer_bits);
    }

    if (setup->header_bits) {
        vring_mem_size += setup->vring_ndesc * (1UL << setup->header_bits);
    }

    if (vring_mem_size > (1UL << id.bits)) {
        VIRTIO_DEBUG_VQ("ERROR: supplied cap was too small %lx, needed %lx\n",
                        ((1UL << id.bits)),
                        (uint64_t )vring_mem_size);
        return VIRTIO_ERR_CAP_SIZE;
    }

    void *vring_addr;
    err = vspace_map_one_frame(&vring_addr, vring_mem_size, vring_cap, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    struct virtqueue *vq;

    vq = calloc(1,
                sizeof(struct virtqueue) + (setup->vring_ndesc
                                * sizeof(struct vring_desc_info)));
    if (vq == NULL) {
        vspace_unmap(vring_addr);
        return LIB_ERR_MALLOC_FAIL;
    }

    vq->device = setup->device;
    strncpy(vq->name, setup->name, sizeof(vq->name));
    vq->queue_index = setup->queue_id;
    vq->desc_num = setup->vring_ndesc;
    vq->vring_align = setup->vring_align;
    vq->vring_cap = vring_cap;
    vq->vring_paddr = id.base;
    vq->vring_vaddr = (lvaddr_t) vring_addr;
    vq->free_count = setup->vring_ndesc;

    vq->intr_handler = setup->intr_handler;
    vq->intr_arg = setup->intr_arg;

    if (0 && setup->max_indirect > 0) {
        /*
         * TODO: initialize indirect descriptors
         */
        virtqueue_init_indirect(vq, setup->max_indirect);
    }

    if (virtio_device_has_feature(setup->device, VIRTIO_RING_F_EVENT_IDX)) {
        vq->flags |= (1 << VIRTQUEUE_FLAG_EVENT_IDX);
    }

    if (setup->buffer_bits) {
        vq->buffer_bits = setup->buffer_bits;
        vq->flags |= (1 << VIRTQUEUE_FLAG_HAS_BUFFERS);
        lpaddr_t offset = vring_size(setup->vring_ndesc, setup->vring_align);
        offset = ROUND_UP(offset, BASE_PAGE_SIZE);
        lvaddr_t buf_start = ((lvaddr_t)vring_addr) +offset;
        VIRTIO_DEBUG_VQ("Allocating %u buffers at offset 0x%lx\n",
                        setup->vring_ndesc, offset);
        /* we initialize the first buffer_allocator here  */
        err = virtio_buffer_alloc_init_vq(&vq->buffer_alloc, vring_cap,buf_start , offset, (1UL<<vq->buffer_bits), setup->vring_ndesc);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to initiate the vbuf allocator");
        }
        assert(vq->buffer_bits);
        assert(vq->buffer_alloc);
    }

    if (vq->header_bits) {
        vq->flags |= (1 << VIRTQUEUE_FLAG_HAS_BUFFERS);
        vq->header_bits = setup->header_bits;
    }

    virtqueue_init_vring(vq);
    virtio_virtqueue_intr_disable(vq);

    if (ret_vq) {
        *ret_vq = vq;
    }

    return SYS_ERR_OK;
}

/**
 * \brief frees the resources of previously allocated virtqueues
 *
 * \param vq pointer to the virtqueue memory to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_virtqueue_free(struct virtqueue *vq)
{
    assert(!"NYI: virtio_virtqueue_free");

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 *  Virtqueue Getter Functions
 */

/**
 * \brief Returns the physical address of the vring.
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns the physical address of the vring
 */
lpaddr_t virtio_virtqueue_get_vring_paddr(struct virtqueue *vq)
{
    return vq->vring_paddr;
}

/**
 * \brief returns the alignment of the vring
 *
 * \param the virtqueue to get the alignment from
 *
 * \returns vring alignment
 */
lvaddr_t virtio_virtqueue_get_vring_align(struct virtqueue *vq)
{
    return vq->vring_align;
}

/**
 * \brief Returns the frame capability of the vring
 *
 * \param vq        pointer to the virtqueue structure
 * \param ret_cap   memory location where to store the capref
 */
void virtio_virtqueue_get_vring_cap(struct virtqueue *vq,
                                    struct capref *ret_cap)
{
    if (ret_cap) {
        *ret_cap = vq->vring_cap;
    }
}

/**
 * \brief returns the number of bits if there are alrady allocated buffers
 *        for this queue
 *
 * \param vq the virtqueue
 *
 * \returns size of allocated buffers
 *          0 if none
 */
uint8_t virtio_virtqueue_has_buffers(struct virtqueue *vq)
{
    if (vq->flags & (1 << VIRTQUEUE_FLAG_HAS_BUFFERS)) {
        return 1;
    }
    return 0;
}

/**
 * \brief returns the virtual base of the previously allocated buffer
 *
 * \param vq the virtqueue
 *
 * \returns virtual address of allocated buffers
 *          0 if no buffers are allocated
 */
lvaddr_t virtio_virtqueue_buffer_vbase(struct virtqueue *vq)
{
    if (vq->flags & (1 << VIRTQUEUE_FLAG_HAS_BUFFERS)) {
        size_t vring_mem_size = vring_size(vq->desc_num, vq->vring_align);
        vring_mem_size = ROUND_UP(vring_mem_size, BASE_PAGE_SIZE);

        return vq->vring_vaddr + vring_mem_size;
    }
    return 0;
}

/**
 * \brief Returns the number of elements (number of descriptors)in the vring of
 *        this virtqueue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of elements in the vring
 */
uint16_t virtio_virtqueue_get_num_desc(struct virtqueue *vq)
{
    return vq->desc_num;
}

/**
 * \brief Returns the queue index of the virtqueue of the device
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns queue index
 */
uint16_t virtio_virtqueue_get_queue_index(struct virtqueue *vq)
{
    return vq->queue_index;
}

/**
 * \brief Checks if the virtqueue is empty
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns 0 the queue is not empty
 *          1 the queue is empty
 */
bool virtio_virtqueue_is_empty(struct virtqueue *vq)
{
    return (vq->desc_num == vq->free_count);
}

/**
 * \brief Checks if the virtqueue is full
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns 0 the queue is not full
 *          1 the queue is full
 */
bool virtio_virtqueue_is_full(struct virtqueue *vq)
{
    return (vq->free_count == 0);
}

/**
 * \brief Calculates the number of used descriptors in this queue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of used descriptors
 */
uint16_t virtio_virtqueue_get_num_used(struct virtqueue *vq)
{
    uint16_t num_used;

    num_used = vq->vring.used->idx - vq->used_tail;

    /* sanity check */
    assert(num_used <= vq->desc_num);

    return num_used;
}

/*
 * ----------------------------------------------------------------------------
 *  Interrupt handling
 */

/**
 * \brief checks if the interrupts can be disabled
 *
 * \param vq virtual queue to check
 *
 * \returns 1 if the interrupts have been disabled
 *          0 if the interrupts are not changed
 *
 */
bool virtio_virtqueue_intr_filter(struct virtqueue *vq)
{
    if (vq->used_tail == vq->vring.used->idx) {
        return 0;
    }

    virtio_virtqueue_intr_disable(vq);

    return 1;
}

/**
 * \brief calls the interrupt handler for this virtqueue
 *
 * \param vq virtqueue to call the intr handler for
 */
void virtio_virtqueue_intr_handle(struct virtqueue *vq)
{
    if (vq->intr_handler == NULL) {
        VIRTIO_DEBUG_VQ("Notice: Interrupt handler is not set\n");
        return;
    }
    vq->intr_handler(vq, vq->intr_arg);
}

/**
 * \brief enables the interrupts on the next descriptor processed
 *
 * \param vq the virtqueue to enable the interrupts
 *
 * \returns 1 if the interrupts have been enabled
 *          0 if the interrupts have not been enabled
 */
bool virtio_virtqueue_intr_enable(struct virtqueue *vq)
{
    return virtqueue_interrupt_enable(vq, 0);
}

/**
 * \brief postpones the interrupt to a later point of time
 *
 * \param vq the virtqueue to enable the interrupts
 * \param
 *
 * \returns 1 if the interrupts have been enabled
 *          0 if the interrupts have not been enabled
 */
bool virtio_virtqueue_intr_postpone(struct virtqueue *vq,
                                    enum virtqueue_intr_postpone hint)
{
    uint16_t ndesc = vq->vring.avail->idx - vq->used_tail;

    switch (hint) {
        case VIRTQUEUE_INTR_POSTPONE_SHORT:
            ndesc = ndesc / 4;
            break;
        case VIRTQUEUE_INTR_POSTPONE_LONG:
            ndesc = (ndesc * 3) / 4;
            break;
        case VIRTQUEUE_INTR_POSTPONE_EMPTIED:
            break;
    }

    return virtqueue_interrupt_enable(vq, ndesc);
}

/**
 * \brief disables the interrupts for the given virtqueue
 *
 * \param vq        virtqueue to disable the interrupts
 */
void virtio_virtqueue_intr_disable(struct virtqueue *vq)
{
    if (vq->flags & (1 << VIRTQUEUE_FLAG_EVENT_IDX)) {
        uint16_t *used_event = vring_get_used_event(&vq->vring);
        *used_event = vq->used_tail - vq->desc_num - 1;
    } else {
        vq->vring.avail->flags |= VIRTIO_RING_AVAIL_F_NO_INTERRUPT;
    }
}

/**
 * \brief notifies the host about the new queued descriptors
 *
 * \param vq virtqueue to notify the host
 */
void virtio_virtqueue_notify_host(struct virtqueue *vq)
{
    /* TODO: memory barrier */
    if (virtqueue_should_notify_host(vq)) {
        virtio_device_notify_host(vq->device, vq->queue_index);
    }
    vq->desc_num_queued = 0;

}

/*
 * We layout the vring structure in memory as follows:
 *
 * struct vring {
 *      // The actual descriptors (16 bytes each)
 *      struct vring_desc desc[num];
 *
 *      // A ring of available descriptor heads with free-running index.
 *      uint16_t avail_flags;
 *      uint16_t avail_idx;
 *      uint16_t available[num];
 *      uint16_t used_event_idx;
 *
 *      // Padding to the next align boundary.
 *      char pad[];
 *
 *      // A ring of used descriptor heads with free-running index.
 *      uint16_t used_flags;
 *      uint16_t used_idx;
 *      struct vring_used_elem used[num];
 *      uint16_t avail_event_idx;
 * };
 */

/**
 * \brief Maps the given capability and initializes the vring on the memory
 *        backed by the supplied capability
 *
 * \param vr    pointer to the vring structure to be initialized
 * \param num   the number of elements in the ring
 * \param align alignment constraints for the vring
 * \param cap   frame capability used as backing memory for the structure
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_init_from_cap(struct vring *vr,
                             uint16_t num,
                             uintptr_t align,
                             struct capref cap)
{
    errval_t err;

    /* num must be a power of two */
    assert(((num != 0) && ((num & (~num + 1)) == num)));

    size_t size = vring_size(num, align);

    struct frame_identity id;
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }

    /* check if we have enough space in the given cap */
    if ((1UL << id.bits) < size) {
        return SYS_ERR_INVALID_SIZE_BITS;
    }

    void *addr;
    err = vspace_map_one_frame(&addr, (1UL << id.bits), cap, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    vring_init(vr, num, align, addr);

    return SYS_ERR_OK;
}

/**
 * \brief allocates a new vring structure
 *
 * \param vr        pointer to the vring structure
 * \param num       the number of queue elements
 * \param align     the alignment constraints for the vring
 * \param ret_frame returned frame capability
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_alloc(struct vring *vr,
                     uint16_t num,
                     uintptr_t align,
                     struct capref *ret_frame)
{
    errval_t err;

    /* num must be a power of two */
    assert(((num != 0) && ((num & (~num + 1)) == num)));

    size_t size = vring_size(num, align);

    struct capref frame;
    err = frame_alloc(&frame, size, &size);
    if (err_is_fail(err)) {
        return err;
    }

    err = vring_init_from_cap(vr, num, align, frame);
    if (err_is_fail(err)) {
        return err;
    }

    if (ret_frame) {
        *ret_frame = frame;
    }

    return SYS_ERR_OK;
}

/**
 * \brief frees the resources used by the vring structure
 *
 * \param vr the vring to be freed
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_free(struct vring *vr)
{
    errval_t err;

    err = vspace_unmap(vr->desc);
    if (err_is_fail(err)) {
        return err;
    }

    assert(!"NYI: returning the cap to the origin");
    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 *  Queue Management
 */

/**
 * \brief updates the available ring of the virtqueue by placing the descriptor
 *        into the availabe ring.
 *
 * \param vq    the virtqueue to update
 * \param idx   index of the new descriptor chain head
 */
static void virtqueue_update_available(struct virtqueue *vq,
                                       uint16_t idx)
{
    uint16_t avail_idx = vq->vring.avail->idx & (vq->desc_num - 1);
    vq->vring.avail->ring[avail_idx] = idx;

    /*
     * wmb();
     */

    VIRTIO_DEBUG_VQ("VQ(%u) avail index = %u, num_queued = %u\n",
                    vq->queue_index, vq->vring.avail->idx + 1, vq->desc_num_queued + 1);

    vq->vring.avail->idx++;
    vq->desc_num_queued++;
}

/**
 * \brief Performs the actual insertion and queue setup of the given buffer list
 *
 * \param vq        virtqueue to insert in
 * \param head      index of the head of the free queue
 * \param bl        buffer list to be enqueued
 * \param num_read  number of readable buffers
 * \param num_write number of writeable buffers
 * \param ret_idx   the returned new free head index
 *
 * \return SYS_ERR_OK on success
 *         VIRTIO_ERR_* on failulre
 */
static errval_t virtqueue_enqueue_bufs(struct virtqueue *vq,
                                       uint16_t head,
                                       struct virtio_buffer_list *bl,
                                       uint16_t num_read,
                                       uint16_t num_write,
                                       uint16_t *ret_idx)
{
    struct vring_desc *desc = vq->vring.desc;
    struct virtio_buffer *buf = bl->head;
    struct vring_desc *cd = desc;

    if (bl->state != VIRTIO_BUFFER_LIST_S_FILLED) {
        return VIRTIO_ERR_BUFFER_STATE;
    }



    uint16_t needed = num_read + num_write;
    uint16_t idx = head;

    VIRTIO_DEBUG_VQ("Enqueuing %u buffers to VQ(%u)\n", needed, vq->queue_index);

    for (uint16_t i = 0; i < needed; ++i) {
        if (buf->state == VIRTIO_BUFFER_S_QUEUED) {
            /*
             * XXX: assume here that read only descriptors can be queued multiple
             *      times, having the same buffer writable enqueued twices, this
             *      is clearly an error
             */
            if (i >= num_read) {
                /*
                 * do a clean up, reverse pointers and revert the fields
                 */
                idx = head;
                buf = bl->head;
                for (uint16_t j = 0; j < i; ++j) {
                    /* reset the buffer state */
                    buf->state = VIRTIO_BUFFER_S_ALLOCED;
                    vq->vring_di[idx].buf = NULL;
                    cd = &desc[idx];
                    cd->addr = buf->paddr;
                    cd->length = buf->length;
                    cd->flags = 0;

                    idx = cd->next;
                    buf = buf->next;
                }
                return VIRTIO_ERR_BUFFER_USED;
            }
        }

        VIRTIO_DEBUG_VQ("  using idx=%u\n", idx);

        vq->vring_di[idx].buf = buf;
        cd = &desc[idx];

        cd->addr = buf->paddr;
        cd->length = buf->length;
        cd->flags = 0;

        if (i < needed - 1) {
            cd->flags |= VIRTIO_RING_DESC_F_NEXT;
        }
        if (i >= num_read) {
            cd->flags |= VIRTIO_RING_DESC_F_WRITE;
        }
        idx = cd->next;
        buf = buf->next;
    }

    cd->next = VIRTQUEUE_CHAIN_END;

    bl->state = VIRTIO_BUFFER_LIST_S_ENQUEUED;

    if (ret_idx) {
        *ret_idx = idx;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Enqueues a new descriptor chain into the virtqueue
 *
 * \param vq     the virtqueue the descriptor chain gets enqueued in
 * \param bl     list of buffers to enqueue into the virtqueue
 * \param st     state associated with this descriptor chain
 * \param num_wr number of writable descriptors
 * \param num_rd number of readable descriptors
 *
 * \returns SYS_ERR_OK on success
 *          VIRTIO_ERR_* on failure
 */
errval_t virtio_virtqueue_desc_enqueue(struct virtqueue *vq,
                                       struct virtio_buffer_list *bl,
                                       void *st,
                                       uint16_t num_wr,
                                       uint16_t num_rd)
{
    errval_t err;

    uint16_t needed = num_rd + num_wr;

    if (needed != bl->length || needed < 0) {
        return VIRTIO_ERR_SIZE_INVALID;
    }

    if (vq->free_count < needed) {
        return VIRTIO_ERR_QUEUE_EMPTY;
    }

    /*
     * TODO: check if we should use indirect descriptors or not
     */

    uint16_t free_head = vq->free_head;
    debug_printf("enq using info [%u]\n", free_head);
    struct vring_desc_info *info = &vq->vring_di[free_head];

    info->is_head = 0x1;
    info->st = st;
    info->bl = bl;

    uint16_t idx;
    err = virtqueue_enqueue_bufs(vq, free_head, bl, num_rd, num_wr, &idx);
    if (err_is_fail(err)) {
        return err;
    }

    /* update free values */
    vq->free_head = idx;
    vq->free_count -= needed;

    virtqueue_update_available(vq, free_head);

    return SYS_ERR_OK;
}

static errval_t virtqueue_free_desc_chain(struct virtqueue *vq,
                                          uint16_t desc_idx)
{
    struct vring_desc *desc;
    struct vring_desc_info *info;

    desc = &vq->vring.desc[desc_idx];
    info = &vq->vring_di[desc_idx];

    uint16_t ndesc = info->bl->length;

    vq->free_count += ndesc;
    ndesc--;

    if ((desc->flags & VIRTIO_RING_DESC_F_INDIRECT) == 0) {
        while (desc->flags & VIRTIO_RING_DESC_F_NEXT) {
            desc = &vq->vring.desc[desc->next];
            ndesc--;
        }
    }

    if (ndesc) {
        VIRTIO_DEBUG_VQ("ERROR: descriptor chain ended at %u\n", ndesc);
        return VIRTIO_ERR_DEQ_CHAIN;
    }

    /* append it to the free list of descriptors */
    desc->next = vq->free_head;
    vq->free_head = desc_idx;

    return SYS_ERR_OK;
}

/**
 * \brief dequeues a descriptor chain form the virtqueue
 *
 * \param vq     the virtqueue to dequeue descriptors from
 * \param ret_bl returns the associated buffer list structure
 * \param ret_st returns the associated state of the queue list
 *
 * \returns SYS_ERR_OK when the dequeue is successful
 *          VIRTIO_ERR_NO_DESC_AVAIL when there was no descriptor to dequeue
 *          VIRTIO_ERR_* if there was an error
 */
errval_t virtio_virtqueue_desc_dequeue(struct virtqueue *vq,
                                       struct virtio_buffer_list **ret_bl,
                                       void **ret_st)
{
    errval_t err;

    struct vring_used_elem *elem;

    uint16_t used_idx, desc_idx;

    /*
     * check if there is a descriptor available
     */
    if (vq->used_tail == vq->vring.used->idx) {
        return VIRTIO_ERR_NO_DESC_AVAIL;
    }

    used_idx = vq->used_tail++ & (vq->desc_num - 1);
    elem = &vq->vring.used->ring[used_idx];

    VIRTIO_DEBUG_VQ("Dequeuing element [%u] on the used ring: [%u, %u]\n",
                        used_idx, elem->id, elem->length);

    /*
     * TODO: read memory barrier
     * rmb();
     * */
    desc_idx = (uint16_t) elem->id;

    /* get the descritpor information */
    struct vring_desc_info *info = &vq->vring_di[desc_idx];
    debug_printf("deq using info [%u]\n", desc_idx);

    assert(info->is_head);
    assert(info->bl);

    struct virtio_buffer_list *bl = info->bl;

    err = virtqueue_free_desc_chain(vq, desc_idx);
    if (err_is_fail(err)) {
        used_idx = vq->used_tail-- & (vq->desc_num - 1);
        return err;
    }

    bl->state = VIRTIO_BUFFER_LIST_S_FILLED;

    if (ret_bl) {
        *ret_bl = bl;
    }

    if (ret_st) {
        *ret_st = info->st;
    }

    return SYS_ERR_OK;
}

/**
 * \brief polls the virtqueue
 *
 * \param vq         the virtqueue to dequeue descriptors from
 * \param ret_bl     returns the associated buffer list structure
 * \param ret_st     returns the associated state of the queue list
 * \param handle_msg flag to have messages handled
 *
 * \returns SYS_ERR_OK when the dequeue is successful
 *          VIRTIO_ERR_* if there was an error
 */
errval_t virtio_virtqueue_poll(struct virtqueue *vq,
                               struct virtio_buffer_list **ret_bl,
                               void **ret_st,
                               uint8_t handle_msg)
{
    errval_t err;

    err = virtio_virtqueue_desc_dequeue(vq, ret_bl, ret_st);

    while (err_no(err) == VIRTIO_ERR_NO_DESC_AVAIL) {
        if (handle_msg) {
            err = event_dispatch_non_block(get_default_waitset());
            if (err_is_fail(err)) {
                if (err_no(err) == LIB_ERR_NO_EVENT) {
                    thread_yield();
                }
            }
        } else {
            thread_yield();
        }
        err = virtio_virtqueue_desc_dequeue(vq, ret_bl, ret_st);
    }

    return err;
}


/**
 * \brief returns a buffer allocator based on the buffers with the virtqueue
 *        (if any)
 *
 * \param vq the virtqueue to get the buffer allocator
 * \param alloc returns the pointer to the allocator
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          VIRTIO_ERR_BUFFER_SIZE if there are no buffers allocated
 */
errval_t virtio_virtqueue_get_buf_alloc(struct virtqueue *vq,
                                        struct virtio_buffer_allocator **alloc)
{
    assert(alloc);

    if (vq->buffer_bits) {
        if (vq->buffer_alloc != NULL) {
            *alloc = vq->buffer_alloc;
            return SYS_ERR_OK;
        }
        // XXX: this is actually an error and should not happen
        return VIRTIO_ERR_NO_BUFFER;
    }

    return VIRTIO_ERR_NO_BUFFER;
}
