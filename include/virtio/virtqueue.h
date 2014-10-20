/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTQUEUE_H
#define VIRTIO_VIRTQUEUE_H

#include <barrelfish/barrelfish.h>

#ifdef __VIRTIO_HOST__
#include <virtio/virtio_host.h>
#endif

/*
 * Extracted from the Virtio Specification 1.0
 * http://docs.oasis-open.org/virtio/virtio/v1.0/virtio-v1.0.pdf
 *
 * VIRQUEUES:
 *
 * Size is determined by a 16bit integer.
 * The queue consists of a descriptor table, available ring, used ring
 * Each of the three parts are physically-contiguous in guest memory.
 *
 */

// forward definition
struct virtqueue;

/// interrupt handler for virtqueue interrupts
typedef void (*virtq_intr_hander_t)(struct virtqueue *, void *);

/// virtqueue default alignment
#define VIRTQUEUE_ALIGNMENT 4096

/// the maximum length of the name field
#define VIRTQUEUE_NAME_SIZE 32

/// the maximum number of elements in a vring
#define VIRTQUEUE_SIZE_MAX (1<<15)

/// this value marks the end of a descriptor chain
#define VIRTQUEUE_CHAIN_END VIRTQUEUE_SIZE_MAX

/// Feature flag indicating that the ring supports indirect descriptors
#define VIRTIO_RING_F_INDIRECT_DESC 28

/// Feature flag indicating that the ring supports interrupt suppression
#define VIRTIO_RING_F_EVENT_IDX     29


/**
 * this structure holds necessary data to allocate a new virtqueue
 *
 * XXX: this may be a bit revised and split into two different structs
 *      one for the host and one for the guest
 */
struct virtqueue_setup {
    char name[VIRTQUEUE_NAME_SIZE];     ///< the name of the queue
    struct virtio_device *device;       ///< device this queue belongs to
    uint16_t queue_id;                  ///< the id of this queue
    uint16_t vring_ndesc;               ///< size of the vring
    lvaddr_t vring_align;               ///< alignment of the vring
    uint16_t max_indirect;              ///< maximum indirect descriptors
#ifdef __VIRTIO_HOST__
    virtq_work_handler_t worker_fn;     ///< callback when new work arrives
    void *worker_arg;                   ///< argument for the worker function
#else
    uint8_t buffer_bits;                ///< when non zero, will allocate buffer
    uint8_t header_bits;                ///< allocate additional space for headers
    uint8_t auto_add;                   ///< adds this virtqueue to the device
    void *intr_arg;                     ///< argument for the interrupt handler
    virtq_intr_hander_t intr_handler;   ///< interrupt handler function
#endif
};

/**
 *
 */
enum virtqueue_intr_postpone {
   VIRTQUEUE_INTR_POSTPONE_SHORT,
   VIRTQUEUE_INTR_POSTPONE_LONG,
   VIRTQUEUE_INTR_POSTPONE_EMPTIED,
};


/**
 * \brief allocates and initiates a new virtqueue structure
 *
 * \param setup  pointer to the setup information
 * \param vq     pointer where to store the new virtqueue pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_virtqueue_alloc(struct virtqueue_setup *setup,
                                struct virtqueue **vq);

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
                                          struct virtqueue **vq);

/**
 * \brief frees the resources of previously allocated virtqueues
 *
 * \param vq pointer to the virtqueue memory to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_virtqueue_free(struct virtqueue *vq);

/*
void    *virtqueue_drain(struct virtqueue *vq, int *last);
int  virtqueue_reinit(struct virtqueue *vq, uint16_t size);
*/


/*
 * ===========================================================================
 * Getter functions for certain values of the virtqueue structure
 */

/**
 * \brief Returns the physical address of the vring.
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns the physical address of the vring
 */
lpaddr_t virtio_virtqueue_get_vring_paddr(struct virtqueue *vq);


/**
 * \brief Returns the frame capability of the vring
 *
 * \param vq        pointer to the virtqueue structure
 * \param ret_cap   memory location where to store the capref
 */
void virtio_virtqueue_get_vring_cap(struct virtqueue *vq,
                                    struct capref *ret_cap);

/**
 * \brief Returns the queue index of the virtqueue of the device
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns queue index
 */
uint16_t virtio_virtqueue_get_queue_index(struct virtqueue *vq);

/**
 * \brief returns the alignment of the vring
 *
 * \param the virtqueue to get the alignment from
 *
 * \returns vring alignment
 */
lvaddr_t virtio_virtqueue_get_vring_align(struct virtqueue *vq);

/**
 * \brief Returns the number of elements (number of descriptors)in the vring of
 *        this virtqueue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of elements in the vring
 */
uint16_t virtio_virtqueue_get_num_desc(struct virtqueue *vq);

/**
 * \brief Checks if the virtqueue is empty
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns 0 the queue is not empty
 *          1 the queue is empty
 */
bool virtio_virtqueue_is_empty(struct virtqueue *vq);

/**
 * \brief Checks if the virtqueue is full
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns 0 the queue is not full
 *          1 the queue is full
 */
bool virtio_virtqueue_is_full(struct virtqueue *vq);

/**
 * \brief Calculates the number of used descriptors in this queue
 *
 * \param vq pointer to the virtqueue structure
 *
 * \returns number of used descriptors
 */
uint16_t virtio_virtqueue_get_num_used(struct virtqueue *vq);

/**
 * \brief returns the number of bits if there are alrady allocated buffers
 *        for this queue
 *
 * \param vq the virtqueue
 *
 * \returns size of allocated buffers
 *          0 if none
 */
uint8_t virtio_virtqueue_has_buffers(struct virtqueue *vq);

/**
 * \brief returns the virtual base of the previously allocated buffer
 *
 * \param vq the virtqueue
 *
 * \returns virtual address of allocated buffers
 *          0 if no buffers are allocated
 */
lvaddr_t virtio_virtqueue_buffer_vbase(struct virtqueue *vq);

/*
 * ===========================================================================
 * Interrupt handling
 */

/**
 * \brief enables the interrupts on the next descriptor processed
 *
 * \param vq the virtqueue to enable the interrupts
 *
 * \returns 1 if the interrupts have been enabled
 *          0 if the interrupts have not been enabled
 */
bool virtio_virtqueue_intr_enable(struct virtqueue *vq);

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
                                    enum virtqueue_intr_postpone hint);

/**
 * \brief checks if the interrupts can be disabled
 *
 * \param vq virtual queue to check
 *
 * \returns 1 if the interrupts have been disabled
 *          0 if the interrupts are not changed
 *
 */
bool virtio_virtqueue_intr_filter(struct virtqueue *vq);

/**
 * \brief calls the interrupt handler for this virtqueue
 *
 * \param vq virtqueue to call the intr handler for
 */
void virtio_virtqueue_intr_handle(struct virtqueue *vq);

/**
 * \brief disables the interrupts for the given virtqueue by giving a hint
 *        to the host
 *
 * \param vq        virtqueue to disable the interrupts
 */
void virtio_virtqueue_intr_disable(struct virtqueue *vq);


/**
 * \brief notifies the host about the new queued descriptors
 *
 * \param vq virtqueue to notify the host
 */
void virtio_virtqueue_notify_host(struct virtqueue *vq);


/**
 * \brief masks the ring features out of a features bit mask
 *
 * \param features  the features to mask
 *
 * \returns bitmask of masked features
 */
static inline uint64_t virtio_virtqueue_mask_features(uint64_t features)
{
    uint64_t mask;

    mask = (1 << VIRTIO_TRANSPORT_F_START) - 1;
    mask |= (1 << VIRTIO_RING_F_INDIRECT_DESC);
    mask |= (1 <<VIRTIO_RING_F_EVENT_IDX);

    return (features & mask);
}

/*
 * ===========================================================================
 * Virtqueue Queue Management
 */

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
                                       uint16_t writeable,
                                       uint16_t readable);

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
                                       void **ret_st);


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
                               uint8_t handle_msg);

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
                                        struct virtio_buffer_allocator **alloc);

#endif // VIRTIO_VIRTQUEUE_H
