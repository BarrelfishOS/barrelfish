/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <virtio/virtio.h>
#include <virtio/virtio_ring.h>
#include <virtio/virtqueue.h>
#include <virtio/virtqueue_host.h>
#include <virtio/virtio_device.h>
#include <virtio/virtio_host.h>

#include "debug.h"

#define IS_POW2(num) (((num) != 0) && (((num) & (~(num) + 1)) == (num)))

#define VIRTQUEUE_FLAG_INDIRECT  0x0001
#define VIRTQUEUE_FLAG_EVENT_IDX 0x0002
#define VIRTQUEUE_FLAG_FREE_CAP  0x8000

/**
 * this data structure stores additional information to the descriptors
 */
struct vring_mem_info
{
    struct capref cap;
    lpaddr_t cap_offset;
    lpaddr_t guest_paddr;
    lvaddr_t vaddr;
    size_t size;
    struct vring_mem_info *next;
};

/**
 * this data structure represents a VirtIO queue. It contains additional
 * information not stored with the vring structure
 */
struct virtqueue_host
{
    /* device information */
    struct virtio_device *device;       ///< pointer to to the virtio device
    uint16_t queue_index;               ///< index of this queue in the device
    char name[VIRTQUEUE_NAME_SIZE];     ///< name of the queue for debugging

    /* vring information */
    struct vring vring;                ///< vring data structure
    struct capref vring_cap;            ///< capability of the vring data structure
    lvaddr_t vring_vaddr;          ///< virtual address of the vring in memory
    lpaddr_t vring_paddr;          ///< physical address of the vring
    lvaddr_t vring_align;          ///< the alignment of the vring

    uint16_t desc_num;                  ///< number of descriptors of this vring
    uint16_t desc_num_max;              ///< maximum number of descriptors supported
    uint16_t desc_num_queued;           ///< number of queued used descriptors

    uint32_t flags;                     ///< flags

    uint16_t avail_tail;                ///< last available index
    uint16_t avail_head;                ///< cache of the available head index
    uint16_t used_head;                 ///< index into the used head

    virtq_work_handler_t worker_fn;     ///< callback when new work arrives
    void *worker_arg;                   ///< argument for the worker function

    struct virtio_host_buf *host_buffers;
    struct vring_mem_info *mem;   ///< array of additional desc information
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


static inline struct virtio_host_buf *vqh_host_buf_deq(struct virtio_host_buf **queue)
{
    if(*queue == NULL) {
        return NULL;
    }
    struct virtio_host_buf *buf = *queue;
    *queue = buf->next;

    return buf;
}
static inline void vqh_host_buf_enq(struct virtio_host_buf **queue,
                                     struct virtio_host_buf *buf)
{
    assert(buf);
    buf->next = *queue;
    *queue = buf;
}
static inline uint16_t vqh_host_buf_enq_chain(struct virtio_host_buf **queue,
                                              struct virtio_host_buf *buf)
{
    assert(buf);
    struct virtio_host_buf *last = buf;
    uint16_t count = 1;
    while(last->next) {
        last = last->next;
        count++;
    }
    last->next = *queue;
    *queue = buf;
    return count;
}

static inline struct virtio_host_buf *vqh_host_buf_alloc(struct virtqueue_host *vq)
{
    return vqh_host_buf_deq(&vq->host_buffers);
}
static inline uint16_t vqh_host_buf_free_chain(struct virtqueue_host *vq,
                                           struct virtio_host_buf *buf)
{
    return vqh_host_buf_enq_chain(&vq->host_buffers, buf);
}

#if 0

static inline void vqh_host_buf_free(struct virtqueue_host *vq,
                                     struct virtio_host_buf *buf)
{
    vqh_host_buf_enq(&vq->host_buffers, buf);
}
#endif



static errval_t virtio_vq_host_add_mem_range(struct virtqueue_host *vq,
                                      struct vring_mem_info *meminfo)
{
    if (vq->mem == NULL) {
        vq->mem = meminfo;
        return SYS_ERR_OK;
    }

    struct vring_mem_info *prev, *current = vq->mem;

    if (meminfo->guest_paddr < vq->mem->guest_paddr) {
        assert((meminfo->guest_paddr+meminfo->size) < vq->mem->guest_paddr);
        meminfo->next = vq->mem;
        vq->mem = meminfo;
        return SYS_ERR_OK;
    }

    prev = vq->mem;
    current = current->next;
    while(current) {
        if (meminfo->guest_paddr < current->guest_paddr) {
            assert((meminfo->guest_paddr+meminfo->size) < current->guest_paddr);
            meminfo->next = current;
            prev->next = meminfo;
        }
        prev = current;
        current = current->next;
    }
    return SYS_ERR_OK;
}

static lvaddr_t virtio_vq_host_guest2virt(struct virtqueue_host *vq,
                                          lpaddr_t guest_phys)
{
    struct vring_mem_info *mi = vq->mem;
    while(mi) {
        if (mi->guest_paddr > guest_phys) {
            return 0;
        }
        if (mi->guest_paddr <= guest_phys) {
            if ((mi->guest_paddr + mi->size) > guest_phys) {
                return mi->vaddr + (guest_phys - mi->guest_paddr);
            }
        }
        mi = mi->next;
    }

    return 0;
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
 * \brief allocates and initiates a new virtqueue structure with no vring mem
 *
 * \param vq     pointer to the array of virtqueue pointers
 * \param setup  pointer to the setup information
 * \param vq_num the number of virtqueues structures to allocate
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_vq_host_alloc(struct virtqueue_host ***vq,
                              struct virtqueue_setup *setup,
                              uint16_t vq_num)
{
    if (vq_num < 0 || vq == NULL) {
        return VIRTIO_ERR_ARG_INVALID;
    }

    struct virtqueue_host **qa = calloc(vq_num, sizeof(void *));
    if (qa == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct virtqueue_host *queue = calloc(vq_num, sizeof(struct virtqueue_host));
    if (queue == NULL) {
        free(qa);
        return LIB_ERR_MALLOC_FAIL;
    }

    // the first setup contains the device
    struct virtio_device *vdev = setup->device;

    for (uint32_t i = 0; i < vq_num; ++i) {
        queue->desc_num_max = setup->vring_ndesc;
        queue->device = vdev;
        /*
        queue->intr_arg = setup->intr_arg;
        queue->intr_handler = setup->intr_handler;
        */
        queue->queue_index = i;
        queue->vring_align = VIRTQUEUE_ALIGNMENT;
        queue->worker_arg = setup->worker_arg;
        queue->worker_fn = setup->worker_fn;
        // queue->desc_ind_max = setup->max_indirect;
        qa[i] = queue;
        queue++;
    }

    *vq = qa;

    return SYS_ERR_OK;
}

/**
 * \brief allocates and initiates a new virtqueue structure
 *
 * \param vdev      the VirtIO device
 * \param vring_cap capability to be used for the vring
 * \param vq_id     id of the queue to initialize
 * \param ndesc     the number of descriptors in this queue
 * \param buf_bits  size of the buffers (0 for none)
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_vq_host_init_vring(struct virtio_device *vdev,
                                   struct capref vring_cap,
                                   uint16_t vq_id,
                                   uint16_t ndesc,
                                   uint8_t has_buffers)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(vring_cap, &id);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_VQ("failed to identify vring cap.\n");
        return err;
    }

    VIRTIO_DEBUG_VQ("Assigning vring [0x%016lx] to virtq %u %s buffers\n",
                    id.base,
                    vq_id,
                    (has_buffers ? "with" : "w/o"));

    struct virtqueue_host *vqh = virtio_device_get_host_virtq(vdev, vq_id);
    if (vqh == NULL) {
        return VIRTIO_ERR_QUEUE_INVALID;
    }

    void *vring_base;
    err = vspace_map_one_frame_attr(&vring_base,
                                    (1UL << id.bits),
                                    vring_cap,
                                    VIRTIO_VREGION_FLAGS_RING,
                                    NULL,
                                    NULL);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_VQ("failed to map vring cap.\n");
        return err;
    }

    VIRTIO_DEBUG_VQ("initializing vring of size %u\n", ndesc);
    vring_init(&vqh->vring, ndesc, vqh->vring_align, vring_base);

    vqh->vring_cap = vring_cap;
    vqh->desc_num = ndesc;
    vqh->vring_vaddr = (lvaddr_t)vring_base;
    vqh->vring_paddr = id.base;

    vqh->host_buffers = calloc(ndesc, sizeof(struct virtio_host_buf));
    struct virtio_host_buf *buf = vqh->host_buffers;
    assert(vqh->host_buffers);
    for (uint32_t i = 0; i < ndesc-1; ++i) {
        buf->next = (buf + 1);
        buf++;
    }

    if (has_buffers) {
        lpaddr_t offset = vring_size(ndesc, vqh->vring_align);
        offset = ROUND_UP(offset, BASE_PAGE_SIZE);
        struct vring_mem_info *mi = calloc(1, sizeof(struct vring_mem_info));
        assert(mi);
        mi->cap = vring_cap;
        mi->cap_offset =offset;
        mi->guest_paddr = virtio_host_translate_host_addr(id.base) + offset;
        mi->vaddr = (lvaddr_t)(vring_base) + offset;
        mi->size = (1UL<<id.bits);
        virtio_vq_host_add_mem_range(vqh, mi);
    }
    return SYS_ERR_OK;
}

/**
 * \brief allocates and initiates a new virtqueue structure
 *
 * \param setup  pointer to the setup information
 * \param vq     pointer where to store the new virtqueue pointer
 *
 * \returns SYS_ERR_OK on success
 */

#if 0
/**
 * \brief allocates and initiates a new virtqueue structure
 *
 * \param setup     pointer to the setup information
 * \param vring_cap capability to be used for the vring
 * \param vq        pointer where to store the new virtqueue pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_vq_host_alloc_with_caps(struct virtqueue_setup *setup,
                struct capref vring_cap,
                struct virtqueue_host **ret_vq)
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

    assert(!capref_is_null(vring_cap));

    struct frame_identity id;
    err = invoke_frame_identify(vring_cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    size_t vring_mem_size = vring_size(setup->vring_ndesc, setup->vring_align);
    vring_mem_size = ROUND_UP(vring_mem_size, BASE_PAGE_SIZE);

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

    struct virtqueue_host *vq = calloc(1,
                    sizeof(struct virtqueue_host) + (setup->vring_ndesc
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

    vq->intr_handler = setup->intr_handler;
    vq->intr_arg = setup->intr_arg;

    if (virtio_device_has_feature(setup->device, VIRTIO_RING_F_EVENT_IDX)) {
        vq->flags |= (1 << VIRTQUEUE_FLAG_EVENT_IDX);
    }

    vring_init(&vq->vring, vq->desc_num, vq->vring_align, (void *) vq->vring_vaddr);

    if (ret_vq) {
        *ret_vq = vq;
    }

    return SYS_ERR_OK;
}
#endif

/**
 * \brief frees the resources of previously allocated virtqueues
 *
 * \param vq pointer to the virtqueue memory to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_vq_host_free(struct virtqueue_host *vq)
{
    assert(!"NYI: virtio_vq_host_free");

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
lpaddr_t virtio_vq_host_get_vring_paddr(struct virtqueue_host *vq)
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
lvaddr_t virtio_vq_host_get_vring_align(struct virtqueue_host *vq)
{
    return vq->vring_align;
}

/**
 * \brief Returns the frame capability of the vring
 *
 * \param vq        pointer to the virtqueue structure
 * \param ret_cap   memory location where to store the capref
 */
void virtio_vq_host_get_vring_cap(struct virtqueue_host *vq,
                                  struct capref *ret_cap)
{
    if (ret_cap) {
        *ret_cap = vq->vring_cap;
    }
}

/**
 * \brief Returns the number of elements (number of descriptors)in the vring of
 *        this virtqueue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of elements in the vring
 */
uint16_t virtio_vq_host_get_num_desc(struct virtqueue_host *vq)
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
uint16_t virtio_vq_host_get_queue_index(struct virtqueue_host *vq)
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
bool virtio_vq_host_is_empty(struct virtqueue_host *vq)
{
    return (vq->vring.avail->idx == vq->avail_tail);
}

/**
 * \brief Calculates the number of available descriptors in this queue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of used descriptors
 */
uint16_t virtio_vq_host_get_num_avail(struct virtqueue_host *vq)
{
    uint16_t num_used;

    num_used = vq->vring.avail->idx - vq->avail_tail;

    /* sanity check */
    assert(num_used <= vq->desc_num);

    return num_used;
}

/*
 * ----------------------------------------------------------------------------
 *  Interrupt handling
 */

/**
 * \brief sends an interrupt to the guest that an event has happened on the
 *        queue
 *
 * \param vq virtqueue to send the interrupt on
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_vq_host_intr_send(struct virtqueue_host *vq)
{
    assert("NYI");
    return SYS_ERR_OK;
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
 * \brief updates the used ring of the virtqueue by placing the descriptor
 *        into the used ring.
 *
 * \param vq    the virtqueue to update
 * \param idx   index of the new descriptor chain head
 */
static void virtqueue_update_used(struct virtqueue_host *vq,
                                  uint16_t idx,
                                  uint16_t length)
{
    uint16_t used_head = vq->vring.used->idx & (vq->desc_num - 1);

    vq->vring.used->ring[used_head].length = length;
    vq->vring.used->ring[used_head].id = idx;

    /*
     * wmb();
     */

    vq->vring.used->idx++;
    vq->desc_num_queued++;
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
errval_t virtio_vq_host_desc_enqueue(struct virtqueue_host *vq,
                                     struct virtio_host_buf *buf,
                                     uint16_t idx)
{


    uint16_t count = vqh_host_buf_free_chain(vq, buf);

    VIRTIO_DEBUG_VQ("Enqueue idx=%u, count=%u\n", idx, count);

    /*
     * TODO: check if we should use indirect descriptors or not
     */

    /* update free values */
    vq->used_head++;

    virtqueue_update_used(vq, idx, count);


    return SYS_ERR_OK;
}

/**
 * \brief dequeues a descriptor chain form the virtqueue
 *
 * \param vq     the virtqueue to dequeue descriptors from
 *
 * \returns SYS_ERR_OK when the dequeue is successful
 *          VIRTIO_ERR_NO_DESC_AVAIL when there was no descriptor to dequeue
 *          VIRTIO_ERR_* if there was an error
 */
errval_t virtio_vq_host_desc_dequeue(struct virtqueue_host *vq)
{

    uint16_t avail_idx;

    struct vring_desc *desc;

    /*
     * check if there is a descriptor available
     */
    if (vq->avail_tail == vq->vring.avail->idx) {
        return VIRTIO_ERR_NO_DESC_AVAIL;
    }

    debug_printf("Avail Tail = %u\n", vq->avail_tail);

    avail_idx = vq->avail_tail++ & (vq->desc_num - 1);

    uint16_t desc_idx = vq->vring.avail->ring[avail_idx];

    desc = &vq->vring.desc[desc_idx];

    struct virtio_host_buf *buf_chain = NULL;
    struct virtio_host_buf *buf = NULL;

    uint16_t count = 1;
    while(desc->flags & VIRTIO_RING_DESC_F_NEXT) {
        buf = vqh_host_buf_alloc(vq);
        assert(buf);
        buf->size = desc->length;
        buf->flags = desc->flags;
        buf->vaddr = virtio_vq_host_guest2virt(vq, desc->addr);
        vqh_host_buf_enq(&buf_chain, buf);
        desc = &vq->vring.desc[desc->next];
        count++;
    }

    /* handle the last one */
    buf = vqh_host_buf_alloc(vq);
    assert(buf);
    buf->size = desc->length;
    buf->flags = desc->flags;
    buf->vaddr = virtio_vq_host_guest2virt(vq, desc->addr);
    vqh_host_buf_enq(&buf_chain, buf);

    VIRTIO_DEBUG_VQ("Dequeuing element on the available [%u] ring: [%u, %u]\n",
                    avail_idx, desc_idx, count);

    /*
     * TODO: read memory barrier
     * rmb();
     * */

    if (vq->worker_fn) {
        vq->worker_fn(vq, vq->worker_arg, buf, desc_idx);
    } else {
        virtio_vq_host_desc_enqueue(vq, buf_chain, desc_idx);
    }


    return SYS_ERR_OK;
}

/**
 * \brief polls the virtqueue
 *
 * \param vq         the virtqueue array to dequeue descriptors from
 * \param vq_num     the number of entries in the vq array
 *
 * \returns SYS_ERR_OK when the dequeue is successful
 *          VIRTIO_ERR_* if there was an error
 */
errval_t virtio_vq_host_poll(struct virtqueue_host **vqh,
                             uint16_t vq_num)
{
    errval_t err;

    /* XXX: handle the case where the queues have not been allocated */
    if (vq_num == 0) {
        return SYS_ERR_OK;
    }

    assert(vqh);

    for (uint32_t i = 0; i < vq_num; ++i) {
        struct virtqueue_host *vq = vqh[i];
        if (vq->vring_vaddr == 0) {
            continue;
        }
        err = virtio_vq_host_desc_dequeue(vq);
        if (err_is_fail(err) && err_no(err) != VIRTIO_ERR_NO_DESC_AVAIL) {
            return err;
        }
    }

    return SYS_ERR_OK;
}
