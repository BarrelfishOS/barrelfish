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

/// the maximum length of the name field
#define VIRTQUEUE_NAME_SIZE 32

/// the maximum number of elements in a vring
#define VIRTQUEUE_SIZE_MAX (1<<15)

/// this value marks the end of a descriptor chain
#define VIRTQUEUE_CHAIN_END VIRTQUEUE_SIZE_MAX

/// Feature flag indicating that the ring supports indirect descriptors
#define VIRTIO_RING_F_INDIRECT_DESC (1 << 28)

/// Feature flag indicating that the ring supports interrupt suppression
#define VIRTIO_RING_F_EVENT_IDX     (1 << 29)


/**
 * this structure holds necessary data to allocate a new virtqueue
 */
struct virtqueue_setup {
    char name[VIRTQUEUE_NAME_SIZE];     ///< the name of the queue
    struct virtio_device *device;       ///< device this queue belongs to
    uint16_t queue_id;                  ///< queue id of this queue
    uint16_t vring_ndesc;               ///< size of the vring
    lvaddr_t vring_align;               ///< alignment of the vring
    virtq_intr_hander_t intr_handler;   ///< interrupt handler function
    void *intr_arg;                     ///< argument for the interrupt handler
    uint16_t max_indirect;              ///< maximum indirect descriptors
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
    mask |= VIRTIO_RING_F_INDIRECT_DESC;
    mask |= VIRTIO_RING_F_EVENT_IDX;

    return (features & mask);
}

/*
 * ===========================================================================
 * Virtqueue Queue Management
 */

errval_t virtio_virtqueue_desc_enqueue(struct virtqueue *vq,
                                       struct virtio_buffer *buf,
                                       void *vaddr,
                                       uint16_t writeable,
                                       uint16_t readable);

#if 0
int  virtqueue_enqueue(struct virtqueue *vq, void *cookie,
         struct sglist *sg, int readable, int writable);
void    *virtqueue_dequeue(struct virtqueue *vq, uint32_t *len);
void    *virtqueue_poll(struct virtqueue *vq, uint32_t *len);



uint64_t virtqueue_filter_features(uint64_t features);

void     virtqueue_dump(struct virtqueue *vq);
#endif


#endif // VIRTIO_VIRTQUEUE_H
