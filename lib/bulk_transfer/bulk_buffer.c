/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>

#include "bulk_pool.h"
#include "bulk_buffer.h"

/**
 * does the mapping of the buffer by filling the backing memobj with the frame
 * and faulting on it. This is a no-op in full trusted mode.
 *
 * @param buf   the buffer to map
 *
 * Note: The new state of the buffer as well as the backing capability must be
 *       set in the buffer struct.
 */
errval_t bulk_buffer_map(struct bulk_buffer *buffer)
{
    assert(buffer);

    errval_t err;
    struct bulk_pool_internal *pool_int;

    if (buffer->pool->trust == BULK_TRUST_FULL) {
        /* mapping in trusted case is a no-op */
        return SYS_ERR_OK;
    }

    /* sanity check */
    if (buffer->state == BULK_BUFFER_INVALID || capref_is_null(buffer->cap)) {
        return BULK_TRANSFER_BUFFER_INVALID;
    }

    pool_int = (struct bulk_pool_internal *) buffer->pool;

    struct vregion *vregion = pool_int->vregion;
    struct memobj *memobj = vregion_get_memobj(vregion);

    size_t size = buffer->pool->buffer_size;
    size_t offset = size * buffer->bufferid;
    //if we have never seen this buffer before, it's address will not be set yet
    buffer->address = (void *) vspace_genvaddr_to_lvaddr(
                                        buffer->pool->base_address + offset);

    /* the capability was revoked thus we have to insert it again */
    err = memobj->f.fill(memobj, offset, buffer->cap, buffer->cap_offset);
    if (err_is_fail(err)) {
        /* TODO: error handling */
        debug_printf("offset = %p, base=%p", (void *) offset,
                     (void *) buffer->pool->base_address);
        return err_push(err, LIB_ERR_MEMOBJ_FILL);
    }

    /* there is a frame cap that backs the buffer, we can do the mapping */
    err = memobj->f.pagefault(memobj, vregion, offset, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
    }

    if (buffer->state != BULK_BUFFER_READ_WRITE) {
        err = memobj->f.protect(memobj, vregion, offset, size,
        VREGION_FLAGS_READ);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_PROTECT);
        }
    }

    return SYS_ERR_OK;
}

/**
 * does the unmapping of a single buffer according to the trust level,
 * - if the channel is fully trusted, this results in a no-op.
 * - otherwise, the mapping is removed
 *
 * This function does not revoke or delete any capabilities
 *
 * @param buf   the buffer to unmap
 */
errval_t bulk_buffer_unmap(struct bulk_buffer *buffer)
{
    assert(buffer);
    assert(buffer->state == BULK_BUFFER_INVALID);

    errval_t err;
    struct bulk_pool_internal *pool_int;

    /* if there is a full trusted channel, then this is a no-op */
    if (buffer->pool->trust == BULK_TRUST_FULL) {
        return SYS_ERR_OK;
    }

    pool_int = (struct bulk_pool_internal *) buffer->pool;

    struct vregion *vregion = pool_int->vregion;
    struct memobj *memobj = vregion_get_memobj(vregion);

    size_t offset = (lvaddr_t) buffer->address - buffer->pool->base_address;

    struct capref buf_cap;
    genvaddr_t ret_offset;
    /*
     * we have to remove the capability from the memory object,
     * this will be revoked by the other side anyway. This does also
     * the unmapping of the frame.
     */
    err = memobj->f.unfill(memobj, offset, &buf_cap, &ret_offset);
    if (err_is_fail(err)) {
        /* TODO: ERROR handling */
        return err;
    }
    if (ret_offset != offset || buf_cap.slot != buffer->cap.slot) {
        /* there is something wrong... */
        /* TODO: error handling */
    }

    /* TODO: do we want to update the state of the buffer to INVALID here ? */

    return SYS_ERR_OK;
}

/**
 * changes the protection bits of an already mapped buffer according to the
 * current buffer state.
 *
 * @param buffer    the buffer to modify the protection
 */
static errval_t bulk_buffer_remap(struct bulk_buffer *buffer)
{
    assert(buffer);
    assert(buffer->state != BULK_BUFFER_INVALID);
    errval_t err;

    if (buffer->pool->trust == BULK_TRUST_FULL) {
        return SYS_ERR_OK;
    }

    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) buffer
                    ->pool;

    struct vregion *vregion = pool_int->vregion;
    struct memobj *memobj = vregion_get_memobj(vregion);

    size_t offset = (lvaddr_t) buffer->address - buffer->pool->base_address;
    size_t size = buffer->pool->buffer_size;

    vs_prot_flags_t flags = VREGION_FLAGS_READ;
    if (buffer->state == BULK_BUFFER_READ_WRITE) {
        flags = VREGION_FLAGS_READ_WRITE;
    }
    err = memobj->f.protect(memobj, vregion, offset, size, flags);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_PROTECT);
    }

    return SYS_ERR_OK;
}

/**
 * checks if the buffer is owned by the calling domain
 *
 * @param buf   buffer to check for ownership
 */
uint8_t bulk_buffer_is_owner(struct bulk_buffer *buffer)
{
    return ((buffer->state == BULK_BUFFER_RO_OWNED)
                    || buffer->state == BULK_BUFFER_READ_WRITE);
}

/**
 * checks if the buffer is a read only copy
 *
 * @param buffer    the buffer to check
 *
 * @return true     if the buffer is a read only copy
 *         false    if the buffer is not a copy
 */
uint8_t bulk_buffer_is_copy(struct bulk_buffer *buffer)
{
    return ((buffer->state == BULK_BUFFER_RO_OWNED)
                    || buffer->state == BULK_BUFFER_READ_ONLY);
}

/**
 * checks if the buffer is valid
 *
 * @param buffer    the buffer to check
 *
 * @return true     if the buffer is valid
 *         false    if the buffer is not valid
 */
uint8_t bulk_buffer_is_valid(struct bulk_buffer *buffer)
{
    return !(buffer->state == BULK_BUFFER_INVALID);
}

/**
 * changes the state of the buffer and adjust the mappings accordingly
 *
 * @param buffer    the buffer to change the state
 * @param state     new state to transition the buffer to
 */
errval_t bulk_buffer_change_state(struct bulk_buffer *buffer,
                                  enum bulk_buffer_state new_state)
{
    assert(buffer);

    errval_t err = SYS_ERR_OK;

    enum bulk_buffer_state st = buffer->state;

    if (st == new_state) {
        /* no change in state */
        return SYS_ERR_OK;
    }

    if (st == BULK_BUFFER_READ_WRITE) {
        buffer->state = new_state;
        switch (new_state) {
            case BULK_BUFFER_RO_OWNED:
            case BULK_BUFFER_READ_ONLY:
                err = bulk_buffer_remap(buffer);
                break;
            case BULK_BUFFER_INVALID:
                err = bulk_buffer_unmap(buffer);
                break;
            default:
                /* NO-OP */
                break;
        }
    } else if (bulk_buffer_is_read_only(buffer)) {
        buffer->state = new_state;
        switch (new_state) {
            case BULK_BUFFER_READ_WRITE:
                err = bulk_buffer_remap(buffer);
                break;
            case BULK_BUFFER_INVALID:
                err = bulk_buffer_unmap(buffer);
                break;
            default:
                /* NO-OP */
                break;
        }
    } else if (st == BULK_BUFFER_INVALID) {
        buffer->state = new_state;
        err = bulk_buffer_map(buffer);
    }

    if (err_is_fail(err)) {
        /* TODO: Error handling */
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * Sets a cap + offset pair for a buffer.
 *
 * @param buffer     the buffer
 * @param cap        cap to assign
 * @param cap_offset offset in the cap
 */
errval_t bulk_buffer_assign_cap(struct bulk_buffer *buffer,
                                struct capref       cap,
                                size_t              cap_offset)
{
    errval_t err;
    struct frame_identity fid = { 0, 0 };

    buffer->cap = cap;
    buffer->cap_offset = cap_offset;

    err = invoke_frame_identify(cap, &fid);
    buffer->phys = fid.base + cap_offset;

    return err;
}

