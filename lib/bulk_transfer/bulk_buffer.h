/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_TRANSFER_BUFFER_H
#define BULK_TRANSFER_BUFFER_H

#include <bulk_transfer/bulk_transfer.h>

/**
 * checks if a given buffer is read only
 *
 * @param buffer    the buffer to check the read only access
 *
 * @return  true    if the buffer is read only
 *          false   otherwise (rw or invalid)
 */
static inline uint8_t bulk_buffer_is_read_only(struct bulk_buffer *buffer) {
    return ((buffer->state == BULK_BUFFER_READ_ONLY)
                    || (buffer->state == BULK_BUFFER_RO_OWNED));
}

/**
 * checks if the given buffer copy can be released
 *
 * @param buffer    the buffer to check the read only access
 *
 * @return  true    if the buffer copy can be released
 *          false   otherwise (there are still references out there)
 */
static inline uint8_t bulk_buffer_can_release(struct bulk_buffer *buffer) {
    return (buffer->local_ref_count == 0);
}

/**
 * checks if the supplied size is valid for a buffer that is:
 * - at least of size BASE_PAGE_SIZE
 * - a power of two
 */
static inline uint8_t bulk_buffer_check_size (size_t x)
{
  return ((x != 0) && !(x & (x - 1)) && (x >= BASE_PAGE_SIZE));
}

/**
 * does the mapping of the buffer according to the base address, capability
 * and offset specified in the buffer struct.
 *
 * @param buf   the buffer to map
 */
errval_t bulk_buffer_map(struct bulk_buffer *buf);


/**
 * does the unmapping of a single buffer according to the trust level,
 * - if the channel is fully trusted, this results in a no-op.
 * - otherwise, the mapping is removed
 *
 * This function does not revoke or delete any capabilities
 *
 * @param buf   the buffer to unmap
 */
errval_t bulk_buffer_unmap(struct bulk_buffer *buf);


/**
 * changes the state of the buffer
 *
 * @param buffer    the buffer to change the state
 * @param state     new state to transition the buffer to
 */
errval_t bulk_buffer_change_state(struct bulk_buffer       *buffer,
                                  enum bulk_buffer_state    state);

/**
 * checks if the buffer is owned by the calling domain
 *
 * @param buffer   buffer to check for ownership
 */
uint8_t bulk_buffer_is_owner(struct bulk_buffer *buf);


/**
 * checks if the buffer is a read only copy
 *
 * @param buffer    the buffer to check
 *
 * @return true     if the buffer is a read only copy
 *         false    if the buffer is not a copy
 */
uint8_t bulk_buffer_is_copy(struct bulk_buffer *buffer);


/**
 * checks if the buffer is valid
 *
 * @param buffer    the buffer to check
 *
 * @return true     if the buffer is valid
 *         false    if the buffer is not valid
 */
uint8_t bulk_buffer_is_valid(struct bulk_buffer *buffer);

/**
 * Sets a cap + offset pair for a buffer.
 *
 * @param buffer     the buffer
 * @param cap        cap to assign
 * @param cap_offset offset in the cap
 */
errval_t bulk_buffer_assign_cap(struct bulk_buffer *buffer,
                                struct capref       cap,
                                size_t              cap_offset);



#endif // ndef BULK_TRANSFER_BUFFER_H

