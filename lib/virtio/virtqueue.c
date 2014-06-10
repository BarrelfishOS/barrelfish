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

#include <virtio/virtio.h>
#include <virtio/virtio_ring.h>
#include <virtio/virtqueue.h>

#include "debug.h"


#define IS_POW2(num) (((num) != 0) && (((num) & (~(num) + 1)) == (num)))


#define VIRTQUEUE_FLAG_INDIRECT  0x0001
#define VIRTQUEUE_FLAG_EVENT_IDX 0x0002

/**
 * this data structure stores additional information to the descriptors
 */
struct vring_desc_info
{
    void *buf;          ///< virtual address of this descriptor
    struct capref cap;  ///< capability of this descriptor
    size_t size;        ///< the size of the capability in bytes
    lpaddr_t paddr;     ///< physical address of this descriptor
    lpaddr_t offset;    ///< offset into the capability for mapping
};

/**
 * this data structure represents a VirtIO queue. It contains additional
 * information not stored with the vring structure
 */
struct virtqueue
{
    /* device information */
    struct virtio_device *device;       ///< pointer to to the virtio device
    uint16_t              queue_index;  ///< index of this queue in the device
    char name[VIRTQUEUE_NAME_SIZE];     ///< name of the queue for debugging

    /* vring information */
    struct vring vring;                 ///< vring data structure
    uint16_t vring_ndesc;               ///< number of descriptors of this vring
    uint32_t flags;                     ///< flags
    uint16_t free_head;                 ///< head of the free descriptor chain
    uint16_t free_count;                ///< number of available free descriptors
    uint16_t used_tail;                 ///< last consumed descriptor used table
    uint16_t used_count;                ///< number of queued used descriptors

    /* vring memory information */
    struct capref vring_cap;            ///< capability of the vring data structure
    lvaddr_t vring_vaddr;               ///< virtual address of the vring in memory
    lpaddr_t vring_paddr;               ///< physical address of the vring
    size_t vring_size;                  ///< the size of the vring in memory
    lvaddr_t vring_align;               ///< the alignment of the vring

    /* interrupt handling */
    virtq_intr_hander_t intr_handler;   ///< interrupt handler
    void               *intr_arg;       ///< user argument for the handler

    struct vring_desc_info vring_di[0]; ///< array of additional desc information
#if 0
    /* indirect descriptors */
    uint16_t max_indirect;
    size_t   indirect_size;
    struct vq_desc_extra {
            void          *cookie;  << virtual address?
            struct vring_desc *indirect;
            vm_paddr_t     indirect_paddr;
            uint16_t       ndescs;
        } vq_descx[0];
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
    if (vq->flags & VIRTQUEUE_FLAG_EVENT_IDX) {
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
    assert(vq->vring_ndesc);
    assert(vq->vring_vaddr);

    /*
     * initialize the vring structure in memory
     */
    vring_init(vr, vq->vring_ndesc, vq->vring_align, (void *)vq->vring_vaddr);

    /*
     * initialize the descriptor chains
     */
    uint32_t i;
    for (i = 0; i < vq->vring_ndesc; ++i) {
        vr->desc[i].next = i+1;
    }
    vr->desc[i].next = VIRTQUEUE_CHAIN_END;

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

    assert(vq);

    if (setup->vring_ndesc == 0 || !IS_POW2(setup->vring_ndesc)) {
        VIRTIO_DEBUG_VQ("ERROR: invalid size: %u\n", setup->vring_ndesc);
        return VIRTIO_ERR_SIZE_INVALID;
    }

    size_t size = vring_size(setup->vring_ndesc, setup->vring_align);
    size = ROUND_UP(size, BASE_PAGE_SIZE);

    struct capref vring_cap;
    size_t framesize;
    err = frame_alloc(&vring_cap, size, &framesize);
    if (err_is_fail(err)) {
        return err;
    }

    VIRTIO_DEBUG_VQ("Allocated memory for vring: [%lx & %lx]",
                    (uint64_t)size, (uint64_t)framesize);

    err = virtio_virtqueue_alloc_with_caps(setup, vring_cap, vq);
    if (err_is_fail(err)) {
        cap_destroy(vring_cap);
        return err;
    }

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
        VIRTIO_DEBUG_VQ("ERROR: too many indirect descriptors requested: [%u / %u]\n", setup->vring_ndesc, VIRTIO_RING_MAX_INDIRECT);
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
                        ((1UL << id.bits)), (uint64_t)vring_mem_size);
        return VIRTIO_ERR_CAP_SIZE;
    }

    void *vring_addr;
    err = vspace_map_one_frame(&vring_addr, vring_mem_size, vring_cap, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    struct virtqueue *vq = calloc(1, sizeof(struct virtqueue)
                                  + (setup->vring_ndesc * sizeof(struct vring_desc_info)));
    if (vq == NULL) {
        vspace_unmap(vring_addr);
        return LIB_ERR_MALLOC_FAIL;
    }

    vq->device = setup->device;
    strncpy(vq->name, setup->name, sizeof(vq->name));
    vq->queue_index = setup->queue_id;
    vq->vring_ndesc = setup->vring_ndesc;
    vq->vring_align = setup->vring_align;
    vq->vring_size = vring_mem_size;
    vq->vring_cap = vring_cap;
    vq->vring_paddr = id.base;
    vq->vring_vaddr = (lvaddr_t)vring_addr;
    vq->free_count = setup->vring_ndesc;


    vq->intr_handler = setup->intr_handler;
    vq->intr_arg = setup->intr_arg;

    if (setup->max_indirect > 0) {
        /*
         * TODO: initialize indirect descriptors
         */
    }

#if 0
    if (VIRTIO_BUS_WITH_FEATURE(dev, VIRTIO_RING_F_EVENT_IDX) != 0)
        vq->vq_flags |= VIRTQUEUE_FLAG_EVENT_IDX;
#endif

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
 * \brief Returns the number of elements (number of descriptors)in the vring of
 *        this virtqueue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of elements in the vring
 */
uint16_t virtio_virtqueue_get_num_desc(struct virtqueue *vq)
{
    return vq->vring_ndesc;
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
    return (vq->vring_ndesc == vq->free_count);
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
    assert(num_used <= vq->vring_ndesc);

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
    if (vq->flags & VIRTQUEUE_FLAG_EVENT_IDX) {
        uint16_t *used_event = vring_get_used_event(&vq->vring);
        *used_event = vq->used_tail - vq->vring_ndesc - 1;
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
    assert(!"NYI: host notify");
}


/*
 * ----------------------------------------------------------------------------
 *  Queue Management
 */

#if 0

/**
 *
 */
errval_t virtio_virtqueue_desc_alloc(struct virtqueue *vq,
                                     size_t )

errval_t virtio_virtqueue_desc_enq(struct virtqueue *vq,
                              )
{
    assert(!"NYI: virtio_virtqueue_enq");
    return SYS_ERR_OK;
}

void *virtio_virtqueue_desc_deq(struct virtqueue *vq)
{
    return NULL;
}



void *virtio_virtqueue_poll(struct virtqueue *vq)
{
    return NULL;
};

#endif

