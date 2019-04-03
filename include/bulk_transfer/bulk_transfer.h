/**
 * \file
 * \brief Generic bulk data transfer mechanism
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_TRANSFER_H
#define BULK_TRANSFER_H

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

/**
 * Specifies the direction of data flow over a channel.
 */
enum bulk_channel_direction {
    BULK_DIRECTION_TX,  ///< This side of the channel is the data source
    BULK_DIRECTION_RX   ///< This side of the channel is the data sink
};

/**
 * The role of the domain with respect to the channel.
 *
 * 1) Creation: upon channel creation the role can either be given or generic
 * 2) Binding: The roles are given either Master-Slave or Slave-Master
 */
enum bulk_channel_role {
    BULK_ROLE_GENERIC,  ///< the role of this endpoint depends on the binding side
    BULK_ROLE_MASTER,   ///< this endpoint is the channel master
    BULK_ROLE_SLAVE     ///< this endpoint is the channel slave
};

/**
 * the trust levels of the channel
 */
enum bulk_trust_level {
    BULK_TRUST_UNINITIALIZED, ///< trust level is not initialized
    BULK_TRUST_NONE,          ///< untrusted case, policies are enforced
    BULK_TRUST_HALF,          ///< same as untrusted, but no revocation of caps
    BULK_TRUST_FULL           ///< complete trust, no unmapping
};

/**
 *
 */
enum bulk_channel_state {
    BULK_STATE_UNINITIALIZED,  ///< channel not initialized, no endpoint assigned
    BULK_STATE_INITIALIZED,    ///< local endpoint assigned, ready for binding
    BULK_STATE_BINDING,        ///< binding is in progress
    BULK_STATE_BIND_NEGOTIATE, ///< channel properties are negotiated (role, trust)
    BULK_STATE_CONNECTED,      ///< binding is completed and ready for use
    BULK_STATE_TEARDOWN,       ///< teardown is initiated
    BULK_STATE_CLOSED          ///< the channel has been closed
};

/* forward declarations */
struct bulk_channel;
struct bulk_channel_constraints;
struct bulk_pool;
struct bulk_pool_list;
struct bulk_buffer;


/**
 * continuation to make the interface asynchronous
 */
struct bulk_continuation {
    void (*handler)(void *arg, errval_t err, struct bulk_channel *channel);
    void *arg;
};

#define MK_BULK_CONT(h,a) ((struct bulk_continuation) {.handler=(h), .arg=(a)})
#define BULK_CONT_NOP     MK_BULK_CONT(NULL, NULL)

/**
 * Helper function to call a bulk continuation with given arguments.
 */
static inline void bulk_continuation_call(struct bulk_continuation cont,
                                          errval_t                 err,
                                          struct bulk_channel      *channel)
{
    if (cont.handler) {
        cont.handler(cont.arg, err, channel);
    }
}


/**
 * Function pointers provided by an implementation of the bulk transfer
 * mechanism over a specific backend. Functions correspond closely to the
 * public interface.
 *
 * XXX: do we want to give a pointer to the closure or the closure itself?
 *      the event_closure just has two fields, so it may be reasonable to do so.
 *      - RA
 */
struct bulk_implementation {
    errval_t (*channel_create)(struct bulk_channel  *channel);

    errval_t (*channel_bind)(struct bulk_channel  *channel,
                             struct bulk_continuation cont);

    errval_t (*channel_destroy)(struct bulk_channel  *channel);

    errval_t (*assign_pool)(struct bulk_channel *channel,
                            struct bulk_pool    *pool,
                            struct bulk_continuation cont);

    errval_t (*remove_pool)(struct bulk_channel *channel,
                            struct bulk_pool    *pool,
                            struct bulk_continuation cont);

    errval_t (*move)(struct bulk_channel  *channel,
                     struct bulk_buffer   *buffer,
                     void                 *meta,
                     struct bulk_continuation cont);

    errval_t (*copy)(struct bulk_channel  *channel,
                     struct bulk_buffer   *buffer,
                     void                 *meta,
                     struct bulk_continuation cont);

    errval_t (*release)(struct bulk_channel  *channel,
                        struct bulk_buffer   *buffer,
                        struct bulk_continuation cont);

    errval_t (*pass)(struct bulk_channel  *channel,
                     struct bulk_buffer   *buffer,
                     void                 *meta,
                     struct bulk_continuation cont);
    /* XXX: remove ? */
    errval_t (*request)(struct bulk_channel  *channel,
                        size_t                count,
                        struct bulk_continuation cont);
};

/**
 * specifies constraints on the channel. This involves limiting the supported
 * memory range or alignment requirements.
 */
struct bulk_channel_constraints {
    uintptr_t mem_range_min;    ///< minimum physical address supported
    uintptr_t mem_range_max;    ///< maximum physical address supported
    uintptr_t men_align;        ///< minimum memory alignment constraint
};

/** Callbacks for events */
struct bulk_channel_callbacks {
    /**
     * For exporting side: other endpoint successfully bound
     */
    errval_t (*bind_received)(struct bulk_channel *channel);

    /**
     * the other side wants to teardown the channel
     * For initiating side: teardown completed
     * For other side: teardown initiated
     */
    void (*teardown_received)(struct bulk_channel *channel);

    /**
     * The other endpoint requests to assign a new pool to this channel.
     * @return If an error value is returned, the pool is not assigned and the
     *         error code is sent to the other side (veto).
     */
    errval_t (*pool_assigned)(struct bulk_channel *channel,
                              struct bulk_pool    *pool);

    /**
     * The other endpoint wants to remove a pool from this channel
     */
    errval_t (*pool_removed)(struct bulk_channel *channel,
                             struct bulk_pool    *pool);

    /** Incoming moved buffer (sink) */
    void (*move_received)(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta);

    /** Incoming passed buffer (source) */
    void (*buffer_received)(struct bulk_channel *channel,
                            struct bulk_buffer  *buffer,
                            void                *meta);

    /** Incoming copied buffer (sink) */
    void (*copy_received)(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta);

    /** Released copied buffer (source) */
    void (*copy_released)(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer);

    /** the other endpoint ran out of buffers and requests more buffers */
     /*
      * XXX: Its a point of argument to have this on the bulk interface or
      *      to push it to the service level. Also: maybe need to specify the
      *      pool id here.
      *      - RA
      * */
    void (*request_received)(struct bulk_channel *channel,
                             size_t               count);
};


/** Handle/Representation for one end of a bulk transfer channel */
struct bulk_channel {
    /** callbacks for the channel events */
    struct bulk_channel_callbacks    *callbacks;
    /** the local endpoint for this channel */
    struct bulk_endpoint_descriptor *ep;
    /** the current channel state */
    enum bulk_channel_state          state;
    /** pool allocators */
    // struct bulk_pool_allocator      *pool_allocators;
    /** orderd list of assigned pools to this channel */
    struct bulk_pool_list           *pools;
    /** the direction of data flow */
    enum bulk_channel_direction      direction;
    /** role of this side of the channel */
    enum bulk_channel_role           role;
    /** the trust level of this channel */
    enum bulk_trust_level            trust;
    /** constraints of this channel */
    struct bulk_channel_constraints  constraints;
    /** the size of the transmitted meta information per bulk transfer */
    size_t                           meta_size;
    /** the waitset for this channel */
    struct waitset                  *waitset;
    /** pointer to user specific state for this channel */
    void                            *user_state;
    /** implementation specific data */
    /*
     * XXX: maybe we want to have an abstract channel and specific channel
     *      as with the endpoints here aswell ?
     *      - RA
     */
    void                            *impl_data;
};

/**
 * generic bulk endpoint
 *
 * This serves as an abstract representation of an endpoint. This data structure
 * must be part of the implementation specific endpoint struct.
 */
struct bulk_endpoint_descriptor {
    /** Pointer to backend-function pointers for this endpoint */
    struct bulk_implementation *f;
    /** TODO: are there more generic endpoint information ? */
};


/**
    this struct represents the pool id which consists of the domain id of the
    allocator and the domain local allocation counter
    TODO: ensure system wide uniquenes also between different machines
 */
struct bulk_pool_id {
    uint32_t    machine;
    uint32_t    dom;//warning: disp_get_domain_id() is core-local
    uint32_t    local;
};


/**
 * represents the state of a buffer
 */
enum bulk_buffer_state {
    BULK_BUFFER_INVALID,    ///< the buffer is not present XXX: name?
    BULK_BUFFER_READ_ONLY,  ///< the buffer is mapped read only
    BULK_BUFFER_RO_OWNED,   ///< the buffer is copied first
    BULK_BUFFER_READ_WRITE  ///< the buffer is mapped read write
};

/**
 * The bulk pool is a continuous region in (virtual) memory that consists of
 * equally sized buffers.
 */
struct bulk_pool {
    /** TODO: find a unique id*/
    struct bulk_pool_id     id;
    /** the base address of the pool */
    lvaddr_t                 base_address;
    /** the size of a single buffer in bytes */
    size_t                   buffer_size;
    /**  pool trust level depending on first assignment */
    enum bulk_trust_level    trust;
    /** capability for the entire pool */
    struct capref            pool_cap;
    /** the maximum number of buffers in this pool */
    size_t                   num_buffers;
    /** array of the buffers for this pool (pre allocated) */
    struct bulk_buffer     **buffers;
};

/**
 * a list of bulk pools assigned to a channel, keep the list ordered by the id
 */
struct bulk_pool_list {
    struct bulk_pool_list *next;    ///< pointer to the next element
    struct bulk_pool      *pool;    ///< the pool
};

/**
 * a bulk buffer is the base unit for bulk data transfer in the system
 */
struct bulk_buffer {
    /** the virtual address of the buffer */
    void                     *address;
    /** the physical address */
    uintptr_t                 phys;
    /** XXX: maybe we have to use the pool_id here */
    struct bulk_pool         *pool;
    /** index of this buffer within the pool's array of buffers */
    uint32_t                  bufferid;
    /** capability for this buffer */
    struct capref             cap;
    /** offset in the capability  */
    lpaddr_t                  cap_offset;
    /** state of the buffer */
    enum bulk_buffer_state    state;
    /** local refrence counting */
    uint32_t                  local_ref_count;
};


/*
 * ---------------------------------------------------------------------------
 * Channel Management >>>
 */

/**
 * setup parameters for creating a new bulk channel
 */
struct bulk_channel_setup {
    /** Channel direction (RX/TX) */
    enum bulk_channel_direction       direction;
    /** Endpoint role (master/slave) */
    enum bulk_channel_role            role;
    /** trust level for this channel */
    enum bulk_trust_level             trust;
    /** */
    struct bulk_channel_constraints   constraints;
    /** Size of metadata to be passed along with transfers and passed buffers. */
    size_t                            meta_size;
    /** Waitset on which events for this channel will be dispatched */
    struct waitset                   *waitset;
    /** */
    void                             *user_state;
};

/**
 * parameters used on binding ot a channel
 */
struct bulk_channel_bind_params {
    /** Endpoint role (master/slave) */
    enum bulk_channel_role            role;
    /** trust level for this channel */
    enum bulk_trust_level             trust;
    /** the channel constraints */
    struct bulk_channel_constraints   constraints;
    /** Waitset on which events for this channel will be dispatched */
    struct waitset                   *waitset;
    /** user state for the channel */
    void                             *user_state;
};


/**
 * Create a new channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param callbacks Callbacks for events on this channel
 * @param setup     struct containing the setup information
 */
errval_t bulk_channel_create(struct bulk_channel              *channel,
                             struct bulk_endpoint_descriptor  *ep_desc,
                             struct bulk_channel_callbacks    *callbacks,
                             struct bulk_channel_setup        *setup);

/**
 * Bind to an existing unbound channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param callbacks Callbacks for events on this channel
 * @param params    parameters for the binding process
 *
 * There is the bind done callback that serves as a continuation for this.
 */
errval_t bulk_channel_bind(struct bulk_channel              *channel,
                           struct bulk_endpoint_descriptor  *remote_ep_desc,
                           struct bulk_channel_callbacks    *callbacks,
                           struct bulk_channel_bind_params  *params,
                           struct bulk_continuation cont);


/**
 * Assign a pool to a channel.
 *
 * @param channel Channel
 * @param pool    Pool to assign (must not be assigned to this channel yet)
 *
 * * There is the pool assigned callback that serves as a continuation for this.
 */
errval_t bulk_channel_assign_pool(struct bulk_channel *channel,
                                  struct bulk_pool    *pool,
                                  struct bulk_continuation cont);

/**
 * Remove a pool from a channel
 *
 * @param channel Channel
 * @param pool    Pool to remove (must be previously assigned to the channel)
 *
 */
errval_t bulk_channel_remove_pool(struct bulk_channel       *channel,
                                  struct bulk_pool          *pool,
                                  struct bulk_continuation   cont);

/**
 * Free a channel
 *
 * @param channel        Channel to be freed
 */
errval_t bulk_channel_destroy(struct bulk_channel      *channel,
                              struct bulk_continuation cont);

/*
 * ---------------------------------------------------------------------------
 * <<< Channel Management
 */



/**
 * Move buffer on the channel. Data and ownership are passed to the other
 * endpoint. After the other endpoint is done with the respective buffer, it can
 * pass it back.
 *
 * @param channel Channel, this endpoint must be source
 * @param buffer  Buffer, must hold ownership and belong to a pool on this
 *                channel
 * @param meta    Pointer to metadata to be passed along with the data
 *                (channel-wide meta_size is used).
 * @param cont    event continuation
 */
errval_t bulk_channel_move(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont);

/**
 * Pass buffer ownership to the other endpoint, the buffer contents are not
 * guaranteed to be transported.
 *
 * @param channel Channel
 * @param buffer  Buffer, must hold ownership and belong to a pool on this
 *                channel
 * @param meta    Pointer to metadata to be passed along with the buffer
 *                (channel-wide meta_size is used).
 * @param cont    event continuation
 */
errval_t bulk_channel_pass(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont);

/**
 * Copy buffer to other endpoint.
 *
 * @param channel Channel, this endpoint must be source
 * @param buffer  Buffer, must belong to a pool on this channel. Must hold
 *                ownersihp, or hold a copy of this buffer.
 * @param meta    Pointer to metadata to be passed along with the buffer
 *                (channel-wide meta_size is used).
 * @param cont    event continuation
 */
errval_t bulk_channel_copy(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont);
/**
 * Release copy received over channel. Must only be called after all outgoing
 * copies from this domain of the same buffer have been released.
 *
 * @param channel Channel, this endpoint must be sink
 * @param buffer  Buffer, must have received it as a copy over this channel, all
 *                outgoing copies must have been released.
 * @param cont    event continuation
 */
errval_t bulk_channel_release(struct bulk_channel       *channel,
                              struct bulk_buffer        *buffer,
                              struct bulk_continuation   cont);



#endif /* BULK_TRANSFER_H */

