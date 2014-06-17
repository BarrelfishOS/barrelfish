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
    lpaddr_t paddr;
    lvaddr_t vaddr;
    size_t size;
    struct virtio_buffer *bufs;
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

    /* vring memory information */

    /* interrupt handling */
    virtq_intr_hander_t intr_handler;   ///< interrupt handler
    void *intr_arg;       ///< user argument for the handler

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


static errval_t virtio_vq_host_add_mem_range(struct virtqueue_host *vq,
                                      struct vring_mem_info *meminfo)
{
    if (vq->mem == NULL) {
        vq->mem = meminfo;
        return SYS_ERR_OK;
    }

    struct vring_mem_info *prev, *current = vq->mem;

    if (meminfo->paddr < vq->mem->paddr) {
        assert((meminfo->paddr+meminfo->size) < vq->mem->paddr);
        meminfo->next = vq->mem;
        vq->mem = meminfo;
        return SYS_ERR_OK;
    }

    prev = vq->mem;
    current = current->next;
    while(current) {
        if (meminfo->paddr < current->paddr) {
            assert((meminfo->paddr+meminfo->size) < current->paddr);
            meminfo->next = current;
            prev->next = meminfo;
        }
        prev = current;
        current = current->next;
    }
    return SYS_ERR_OK;
}

static struct virtio_buffer *virtio_vq_host_guest2virt(struct virtqueue_host *vq,
                                          lpaddr_t guest_phys)
{
    struct vring_mem_info *mi = vq->mem;
    while(mi) {
        if (mi->paddr > guest_phys) {
            return 0;
        }
        if (mi->paddr <= guest_phys) {
            if ((mi->paddr + mi->size) > guest_phys) {
                return mi->bufs + (guest_phys - mi->paddr);
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
        queue->intr_arg = setup->intr_arg;
        queue->intr_handler = setup->intr_handler;
        queue->queue_index = i;
        queue->vring_align = VIRTQUEUE_ALIGNMENT;
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
                                   uint8_t buf_bits)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(vring_cap, &id);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_VQ("failed to identify vring cap.\n");
        return err;
    }

    VIRTIO_DEBUG_VQ("Assigning vring [0x%016lx] to virtq %u containing " "buffers of size %u\n",
                    id.base,
                    vq_id,
                    buf_bits);

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


    if (buf_bits) {
        lpaddr_t offset = vring_size(ndesc, vqh->vring_align);
        offset = ROUND_UP(offset, BASE_PAGE_SIZE);
        struct vring_mem_info *mi = calloc(1, sizeof(struct vring_mem_info *));
        assert(mi);
        mi->bufs = calloc(ndesc, sizeof(*mi->bufs));
        mi->cap = vring_cap;
        mi->cap_offset =offset;
        mi->paddr = virtio_host_translate_host_addr(id.base) + offset;
        mi->vaddr = (lvaddr_t)(vring_base) + offset;
        mi->size = (1UL<<id.bits);
        size_t buf_size = (1UL<< buf_bits);
        struct virtio_buffer *buf;
        lpaddr_t paddr = virtio_host_translate_host_addr(id.base) + offset;
        lvaddr_t vaddr = (lvaddr_t)(vring_base) + offset;
        for (uint32_t i = 0; i < ndesc; ++i) {
            buf = mi->bufs + i;
            buf->length = buf_size;
            buf->paddr = paddr;
            buf->buf = (void*)(vaddr);

            vaddr += buf_size;
            paddr += buf_size;
        }
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
                                     struct virtio_buffer_list *bl,
                                     void *st,
                                     uint16_t num_wr,
                                     uint16_t num_rd)
{
    errval_t err;

    uint16_t idx = 0, length = 0;

    assert(!"NYI");

    /*
     * TODO: check if we should use indirect descriptors or not
     */

    /* update free values */
    vq->avail_tail++;

    virtqueue_update_used(vq, idx, length);

    err = SYS_ERR_OK;

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
errval_t virtio_vq_host_desc_dequeue(struct virtqueue_host *vq,
                                     struct virtio_buffer_list **ret_bl,
                                     void **ret_st)
{
    errval_t err;

    struct vring_used_elem *elem;

    uint16_t avail_idx, desc_idx;

    /*
     * check if there is a descriptor available
     */
    if (vq->avail_tail == vq->vring.avail->idx) {
        return VIRTIO_ERR_NO_DESC_AVAIL;
    }

    avail_idx = vq->avail_tail++ & (vq->desc_num - 1);
    elem = &vq->vring.used->ring[avail_idx];

    assert(!"NYI");

    virtio_vq_host_guest2virt(vq, 0x1234);
    /*
     * TODO: read memory barrier
     * rmb();
     * */
    desc_idx = (uint16_t) elem->id;

    /* TODO: get the queue lists */

    err = SYS_ERR_OK;

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
errval_t virtio_vq_host_poll(struct virtqueue_host *vq,
                             struct virtio_buffer_list **ret_bl,
                             void **ret_st,
                             uint8_t handle_msg)
{
    errval_t err;

    err = virtio_vq_host_desc_dequeue(vq, ret_bl, ret_st);

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
        err = virtio_vq_host_desc_dequeue(vq, ret_bl, ret_st);
    }

    return err;
}
