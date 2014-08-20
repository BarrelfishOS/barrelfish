/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>
#include "bulk_pool.h"
#include "bulk_buffer.h"

/**
 * compares two bulk pool ids
 *
 * @return  -1  if id1 is less than id2
 *           0  if both ids are equal
 *           1  if id1 is bigger than id2
 */
int8_t bulk_pool_cmp_id(struct bulk_pool_id *id1, struct bulk_pool_id *id2)
{

    if (id1->machine < id2->machine) {
        return -1;
    }

    if (id1->machine > id2->machine) {
        return 1;
    }

    if (id1->dom < id2->dom) {
        return -1;
    }

    if (id1->dom > id2->dom) {
        return 1;
    }

    if (id1->local < id2->local) {
        return -1;
    }

    if (id1->local > id2->local) {
        return 1;
    }

    assert(id1->machine == id2->machine);
    assert(id1->dom == id2->dom);
    assert(id1->local == id1->local);

    return 0;
}

/**
 * checks if a pool already has been assigned to that channel
 *
 * @param pool      the bulk pool to check for assignment
 * @param channel   the channel to check for assignment
 *
 * @return true:    the pool is assigned to this channel
 *         false:   the pools has not been assigned to the channel
 */
uint8_t bulk_pool_is_assigned(struct bulk_pool *pool,
                              struct bulk_channel *channel)
{
    assert(channel);

    struct bulk_pool_list *list = channel->pools;

    while (list) {
        switch ((bulk_pool_cmp_id(&list->pool->id, &pool->id) == 0)) {
            case -1:
                /* the ID of the pool in the list is lower, check next */
                continue;
            case 0:
                /* we have a match */
                return 1;
            case 1:
                /* we have a lower id than the pool in the list */
                return 0;
            default:
                break;
        }

        list = list->next;
    }

    return 0;
}

/**
 * checks if the pool is already remapped i.e. the per-buffer caps have been
 * created
 *
 * @return false    if there are no caps per buffer
 *         true     if the caps have been created
 */
static uint8_t bulk_pool_is_remapped(struct bulk_pool *pool)
{
    struct capref *pool_cap = &pool->pool_cap;
    struct capref *buf_cap = &pool->buffers[0]->cap;

    if (pool_cap->slot == buf_cap->slot) {
        /**
         * if the pool is remapped a new cnode is created with the buffer caps,
         * thus the first buffer capability should have another cnode address
         * as the pool has indicating the pool has been remapped.
         */
        if (pool_cap->cnode.address == buf_cap->cnode.address) {
            return 0;
        }
        return 1;
    }
    return 1;
}
/**
 * does the remapping of the pool if the trust level changes from fully trusted
 * to a lower one. This function is called by the backend.
 *
 * @param pool  the pool to remap
 */
errval_t bulk_pool_remap(struct bulk_pool *pool)
{

    /* TODO: REMOVE this function */
    assert(!"DEPRECATED: should not be used anymore");

    errval_t err;

    /* check if the pool is already remapped  */
    if (bulk_pool_is_remapped(pool)) {
        return BULK_TRANSFER_POOL_ALREADY_REMAPPED;
    }

    /* get the vspace / vregions / memobj pointers */
    struct vspace *vspace = get_current_vspace();
    struct vregion *vregion = vspace_get_region(vspace,
                    (void *) pool->base_address);
    struct memobj *memobj = vregion_get_memobj(vregion);

    /*
     * remove the pool capability from the memobj by unfilling it,
     * this also does the unmap of the vregions
     */
    struct capref pool_cap;
    genvaddr_t ret_offset;

    err = memobj->f.unfill(memobj, 0, &pool_cap, &ret_offset);
    if (err_is_fail(err)) {
        /*
         * XXX: this error should basically not happen here...
         */
        assert(err != LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET);
        return err_push(err, LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET);
    }

    /* sanity check that the removed offset is in fact zero */
    assert(ret_offset == 0);

    err = memobj->f.unfill(memobj, 0, &pool_cap, &ret_offset);
    if (err != LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET) {
        /*
         * Note that a pool that has not been remapped only contains one
         * frame. Thus this call to unfill has to return with the error
         * code. Otherwise there is something wrong with the pool's memobj...
         */
        return BULK_TRANSFER_POOL_INVALD;
    }

    /*
     * the pool cap has been successfully removed from the pool's memobj
     * and we can start creating the buffer caps in a new cnode.
     */
    struct capref cnode_cap;
    struct capref buf_cap = { .slot = 0 };

    err = cnode_create(&cnode_cap, &buf_cap.cnode, pool->num_buffers, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }
    /* determine the size bits for the buffer */
    size_t size = 12;
    size_t buf_size = pool->buffer_size >> 12;
    while (buf_size >>= 1) {
        ++size;
    }
    /* retype the pool cap into the buffer caps of the new cnode */
    err = cap_retype(buf_cap, pool->pool_cap, ObjType_Frame, size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    for (int i = 0; i < pool->num_buffers; ++i) {
        struct bulk_buffer *buf = pool->buffers[i];

        size_t offset = i * pool->buffer_size;

        /* update capability information of the buffer */
        buf_cap.slot = i;
        buf->cap_offset = 0;
        buf->cap = buf_cap;

        err = memobj->f.fill(memobj, offset, buf->cap, pool->buffer_size);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }

        vregion = vspace_get_region(vspace, buf->address);
        err = memobj->f.map_region(memobj, vregion);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_MAP_REGION);
        }

        /* create the actual mapping by faulting on it */
        err = memobj->f.pagefault(memobj, vregion, offset, 0);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }
        /*
         * TODO: do we want to abort the processing if one of the operations
         *       fails for one of the buffers or just mark the buffer as
         *       invalid and go on?
         */
    }

    return SYS_ERR_OK;
}

/**
 * unmaps the entire pool and frees up the entire memory region of the pool.
 *
 * @param pool  the pool to unmap
 */
errval_t bulk_pool_unmap(struct bulk_pool *pool)
{
    assert(pool);
    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) pool;

    if (!pool_int->vregion) {
        /* there is no vregion associated with the pool, so its not mapped */
        return SYS_ERR_OK;
    }

    errval_t err = SYS_ERR_OK;

    /* get the vspace / vregions / memobj pointers */
    struct vregion *vregion = pool_int->vregion;
    struct memobj *memobj = vregion_get_memobj(vregion);

    struct capref ret_cap;
    genvaddr_t ret_addr;

    /* unfill and unmap the frames */
    for (int i=0; i < pool->num_buffers; ++i) {
        genvaddr_t offset = i * pool->buffer_size;

        err = memobj->f.unfill(memobj, offset, &ret_cap, &ret_addr);
        if (err_is_fail(err)) {
            if (err == LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET) {
                break;
            }
            /* TODO: Error handling */
            return err;
        }
    }

    err = vregion_destroy(vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VREGION_DESTROY);
    }

    return SYS_ERR_OK;
}

/**
 * Does the mapping of a pool depending on the trust level. If it is not trusted,
 * only the memory range is allocated and the vregions for the buffers created,
 * In the trusted case, the pool is backed with the pool cap and mapped.
 *
 * @param pool  the pool to map
 */
errval_t bulk_pool_map(struct bulk_pool *pool)
{
    assert(pool);

    errval_t err;

    if (pool->base_address != 0) {
        /* the pool already has an base address thus is mapped */

        /* XXX: maybe return an already mapped error ? */
        return SYS_ERR_OK;
    }

    if (!bulk_buffer_check_size(pool->buffer_size)) {
        return BULK_TRANSFER_ALLOC_BUFFER_SIZE;
    }

    size_t pool_size = pool->buffer_size * pool->num_buffers;

    struct vspace *vspace = get_current_vspace();

    struct memobj_fixed *memobj_fixed = malloc(sizeof(struct memobj_fixed));
    if (!memobj_fixed) {
        return BULK_TRANSFER_MEM;
    }
    struct memobj *memobj = &(memobj_fixed->m);

    // Create a memobj
    err = memobj_create_fixed(memobj_fixed, pool_size, 0, pool->num_buffers,
                    pool->buffer_size);

    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
    }

    struct pmap *pmap = vspace_get_pmap(vspace);

    /* allocate some virtual address space */
    genvaddr_t address;
    err = pmap->f.determine_addr(pmap, memobj, 4096, &address);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_DETERMINE_ADDR);
    }
    pool->base_address = vspace_genvaddr_to_lvaddr(address);

    if (pool->trust == BULK_TRUST_FULL
                    || pool->trust == BULK_TRUST_UNINITIALIZED) {
        if (capref_is_null(pool->pool_cap)) {
            return SYS_ERR_CAP_NOT_FOUND;
        }

        /*
         * the pool cap has been successfully removed from the pool's memobj
         * and we can start creating the buffer caps in a new cnode.
         */
        struct capref cnode_cap;
        struct capref buf_cap = { .slot = 0 };

        err = cnode_create(&cnode_cap, &buf_cap.cnode, pool->num_buffers, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }
        /* determine the size bits for the buffer */
        size_t size = 12;
        size_t buf_size = pool->buffer_size >> 12;
        while (buf_size >>= 1) {
            ++size;
        }
        /* retype the pool cap into the buffer caps of the new cnode */
        err = cap_retype(buf_cap, pool->pool_cap, ObjType_Frame, size);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CAP_RETYPE);
        }

        for (int i = 0; i < pool->num_buffers; ++i) {
            struct bulk_buffer *buf = pool->buffers[i];

            /* update capability information of the buffer */
            buf_cap.slot = i;
            buf->cap_offset = 0;
            buf->cap = buf_cap;

            err = memobj->f.fill(memobj, i * pool->buffer_size, buf->cap,
                            pool->buffer_size);
            if (err_is_fail(err)) {
                /* TODO: error handling */
                return err_push(err, LIB_ERR_MEMOBJ_FILL);
            }
        }
    }

    /* we have the address range, now we have to */
    struct vregion *vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        return BULK_TRANSFER_MEM;
    }

    err = vregion_map_fixed(vregion, get_current_vspace(), memobj, 0, pool_size,
                    address, VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_MAP_REGION);
    }

    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) pool;
    pool_int->vregion = vregion;

    for (int i = 0; i < pool->num_buffers; ++i) {

        size_t offset = (i * pool->buffer_size);

        if (pool->trust == BULK_TRUST_FULL
                        || pool->trust == BULK_TRUST_UNINITIALIZED) {
            err = memobj->f.pagefault(memobj, vregion, offset, 0);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
            }
            struct bulk_buffer *buf = pool->buffers[i];
            buf->state = BULK_BUFFER_READ_WRITE;
            buf->address = (void *) vspace_genvaddr_to_lvaddr(address + offset);
        }
    }
    return SYS_ERR_OK;
}

/**
 * initializes the buffers for a pool given the struct pool is allocated and
 * filled with the num bufs
 *
 * @param pool  pointer to a pool with the information
 */
errval_t bulk_pool_init_bufs(struct bulk_pool *pool)
{
    size_t buffer_count = pool->num_buffers;

    /* allocate memory for buffers */
    struct bulk_buffer *bufs = malloc(
                    buffer_count * sizeof(struct bulk_buffer));
    if (!bufs) {
        return BULK_TRANSFER_MEM;
    }
    memset(bufs, 0, buffer_count * sizeof(struct bulk_buffer));

    pool->buffers = malloc(buffer_count * sizeof(void *));
    if (!pool->buffers) {
        return BULK_TRANSFER_MEM;
    }

    for (int i = 0; i < buffer_count; ++i) {
        (bufs + i)->state = BULK_BUFFER_INVALID;
        (bufs + i)->pool = pool;
        pool->buffers[i] = bufs + i;
    }

    return SYS_ERR_OK;
}
