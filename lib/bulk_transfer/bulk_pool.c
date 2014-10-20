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

static void bulk_pool_generate_id(struct bulk_pool_id* id)
{
    /*
     * todo: get a system wide unique domain identifier
     *       get a domain local sequence id
     */

    static uint32_t local_id = 0;

    assert(id);

    id->machine = 0;
    //XXX: disp_get_domain_id() is core-local, but we don't want
    //to put the core ID into the machine part
    id->dom = (disp_get_core_id() << 16) | disp_get_domain_id();
    id->local = local_id++;
}

static struct bulk_pool_list *pool_list = NULL;

/**
 * returns a pointer to the pool with the given id
 *
 * @param   id  the id of the pool to look up
 *
 * @return  NULL if the pool is not present in the domain
 */
struct bulk_pool *bulk_pool_domain_list_get(struct bulk_pool_id *id)
{
    struct bulk_pool_list *list = pool_list;
    while (list) {
        if (bulk_pool_cmp_id(&list->pool->id, id) == 0) {
            return list->pool;
        }
        list = list->next;
    }
    return NULL;
}

/**
 * inserts a pool into the domain global bulk pool list
 *
 * @param   pool    the pool to insert
 */
errval_t bulk_pool_domain_list_insert(struct bulk_pool *pool)
{
    struct bulk_pool_list *new_pool = malloc(sizeof(struct bulk_pool_list));
    if (!new_pool) {
        return BULK_TRANSFER_MEM;
    }

    new_pool->next = NULL;
    new_pool->pool = pool;

    if (pool_list == NULL) {
        pool_list = new_pool;
        return SYS_ERR_OK;
    }

    struct bulk_pool_list *list = pool_list;
    struct bulk_pool_list *prev = NULL;

    while (list) {
        switch (bulk_pool_cmp_id(&list->pool->id, &pool->id)) {
            case -1:
                /* the ID of the pool in the list is lower, check next */
                prev = list;
                list = list->next;
                continue;
            case 0:
                /* we have a match */
                free(new_pool);
                return SYS_ERR_OK;
            case 1:
                /* the ID of the pool in the list is bigger, insert before */
                if (prev) {
                    new_pool->next = prev->next;
                    prev->next = new_pool;
                } else {
                    new_pool->next = pool_list;
                    pool_list = new_pool;
                }
                return SYS_ERR_OK;
            default:
                break;
        }
    }

    /* insert at the end */
    prev->next = new_pool;

    return SYS_ERR_OK;
}


/**
 * removes the pool from the channel's pool list
 *
 * @param pool      the poo to remove
 */
errval_t bulk_pool_domain_list_remove(struct bulk_pool *pool)
{
    struct bulk_pool_list *list = pool_list;
    struct bulk_pool_list *prev = NULL;

    while (list) {
        switch (bulk_pool_cmp_id(&list->pool->id, &pool->id)) {
            case -1:
                /* the ID of the pool in the list is lower, check next */
                continue;
            case 0:
                /* we have a match, remove it */
                if (prev) {
                    prev->next = list->next;
                } else {
                    pool_list = list->next;
                }
                free(list);
                return SYS_ERR_OK;
            case 1:
                /* the ID of the pool in the list is bigger, insert before */
                return BULK_TRANSFER_POOL_NOT_ASSIGNED;
            default:
                break;
        }
        prev = list;
        list = list->next;
    }

    return BULK_TRANSFER_POOL_NOT_ASSIGNED;
}

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
        switch (bulk_pool_cmp_id(&list->pool->id, &pool->id)) {
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
 * gets a pointer to the pool on this channel
 *
 * @param id        the poolid we want the pool
 * @param channel   the channel to look for the pools
 */
struct bulk_pool *bulk_pool_get(struct bulk_pool_id *id,
                                struct bulk_channel *channel)
{
    struct bulk_pool_list *list = channel->pools;
    while (list) {
        if (bulk_pool_cmp_id(&list->pool->id, id) == 0) {
            return list->pool;
        }
        list = list->next;
    }
    return NULL;
}

/**
 * adds a pool to a channel's pool list
 *
 * @param pool      the pool to assing to the channel
 * @param channel   the channel to assign the the pool to
 */
errval_t bulk_pool_assign(struct bulk_pool *pool, struct bulk_channel *channel)
{
    assert(channel);
    struct bulk_pool_list *new_pool = malloc(sizeof(struct bulk_pool_list));
    if (!new_pool) {
        return BULK_TRANSFER_MEM;
    }

    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) pool;

    new_pool->next = NULL;
    new_pool->pool = pool;

    if (!channel->pools) {
        channel->pools = new_pool;
        return SYS_ERR_OK;
    }

    struct bulk_pool_list *list = channel->pools;
    struct bulk_pool_list *prev = NULL;

    while (list) {
        switch (bulk_pool_cmp_id(&list->pool->id, &pool->id)) {
            case -1:
                /* the ID of the pool in the list is lower, check next */
                prev = list;
                list = list->next;
                continue;
            case 0:
                /* we have a match */
                return BULK_TRANSFER_POOL_ALREADY_ASSIGNED;
            case 1:
                /* the ID of the pool in the list is bigger, insert before */
                if (prev) {
                    new_pool->next = prev->next;
                    prev->next = new_pool;
                } else {
                    new_pool->next = channel->pools;
                    channel->pools = new_pool;
                }
                pool_int->refcnt++;
                return SYS_ERR_OK;
            default:
                break;
        }
    }

    /* insert at the end */
    prev->next = new_pool;
    pool_int->refcnt++;
    return SYS_ERR_OK;
}

/**
 * removes the pool from the channel's pool list
 *
 * @param pool      the poo to remove
 * @param channel   the channel to remove the pool from
 */
errval_t bulk_pool_remove(struct bulk_pool *pool, struct bulk_channel *channel)
{
    assert(channel);

    struct bulk_pool_list *list = channel->pools;
    struct bulk_pool_list *prev = NULL;

    while (list) {
        switch (bulk_pool_cmp_id(&list->pool->id, &pool->id)) {
            case -1:
                /* the ID of the pool in the list is lower, check next */
                continue;
            case 0:
                /* we have a match, remove it */
                if (prev) {
                    prev->next = list->next;
                } else {
                    channel->pools = list->next;
                }
                free(list);
                return SYS_ERR_OK;
            case 1:
                /* the ID of the pool in the list is bigger, insert before */
                return BULK_TRANSFER_POOL_NOT_ASSIGNED;
            default:
                break;
        }
        prev = list;
        list = list->next;
    }

    return BULK_TRANSFER_POOL_NOT_ASSIGNED;
}

/**
 * unmaps the entire pool and frees up the entire memory region of the pool.
 *
 * @param pool  the pool to unmap
 */
// XXX Are caps deleted?
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

    struct bulk_buffer *buf;

    /* unfill and unmap the frames */
    for (int i = 0; i < pool->num_buffers; ++i) {
        genvaddr_t offset = i * pool->buffer_size;
        buf = pool->buffers[i];
        err = memobj->f.unfill(memobj, offset, &ret_cap, &ret_addr);
        if (err_is_fail(err)) {
            if (err == LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET) {
                break;
            }
            /* TODO: Error handling */
            return err;
        }
        cap_delete(buf->cap);
        buf->cap = NULL_CAP;
    }

    /* delete the pool cap and the cnode cap */
    cap_destroy(pool->pool_cap);
    cap_destroy(pool_int->cnode_cap);

    err = vregion_destroy(vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VREGION_DESTROY);
    }

    return SYS_ERR_OK;
}

/**
 * Does the mapping of a pool depending on the trust level.
 * Reserves virtual memory, and allocates a memobj for the pool.
 * In the trusted case, the pool is backed with the pool cap and mapped.
 * In the nontrusted case, the pool cap is split into seperate buffer caps and
 * mapped.
 * If there is no pool_cap, only the virtual memory is allocated.
 *
 * XXX : trust_uninitialized is currently treated like the trusted case,
 * which is probably not the best idea. should we treat it as an error or just
 * ignore it?
 *
 * @param pool  the pool to map
 */
errval_t bulk_pool_map(struct bulk_pool *pool)
{
    assert(pool);
    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) pool;
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

    /* we have the address range, now we have to associate a vregion with it*/
    struct vregion *vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        return BULK_TRANSFER_MEM;
    }

    err = vregion_map_fixed(vregion, get_current_vspace(), memobj, 0, pool_size,
                            address, VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_MAP_REGION);
    }

    pool_int->vregion = vregion;

    if (pool->trust == BULK_TRUST_FULL
                    || pool->trust == BULK_TRUST_UNINITIALIZED) {
        if (capref_is_null(pool->pool_cap)) {
            return SYS_ERR_CAP_NOT_FOUND;
        }
        //XXX: treating uninitialized just like full trust does not sound like a
        //      good idea...

        /* start creating caps for each buffer */
        struct capref buf_cap = {
            .slot = 0 };

        err = cnode_create(&pool_int->cnode_cap, &buf_cap.cnode,
                           pool->num_buffers, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }

        /* copy the pool cap for each buffer into the new cnode and set
         * appropriate offset */
        for (int i = 0; i < pool->num_buffers; ++i) {
            struct bulk_buffer *buf = pool->buffers[i];
            buf_cap.slot = i;
            size_t offset = (i * pool->buffer_size);

            err = cap_copy(buf_cap, pool->pool_cap);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_CAP_COPY);
            }

            err = bulk_buffer_assign_cap(buf, buf_cap, offset);
            assert(err_is_ok(err)); /* this should not fail */

            err = memobj->f.fill(memobj, offset, buf->cap,
                                 buf->cap_offset);
            if (err_is_fail(err)) {
                /* TODO: error handling */
                return err_push(err, LIB_ERR_MEMOBJ_FILL);
            }
            buf->address = (void *) vspace_genvaddr_to_lvaddr(address + offset);
            err = memobj->f.pagefault(memobj, vregion, offset, 0);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
            }

            buf->state = BULK_BUFFER_READ_WRITE;
        }
    } else if (pool->trust == BULK_TRUST_NONE && !capref_is_null(pool->pool_cap)) {
        /* start creating caps for each buffer */
        struct capref buf_cap = {
            .slot = 0 };

        err = cnode_create(&pool_int->cnode_cap, &buf_cap.cnode,
                           pool->num_buffers, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }
        /* determine the size bits per buffer */
        size_t size_bits = 12;
        size_t buf_size = pool->buffer_size >> 12;
        while (buf_size >>= 1) {
            ++size_bits;
        }
        //split pool cap into smaller caps for each buffer
        err = cap_retype(buf_cap, pool->pool_cap, ObjType_Frame, size_bits);
        assert(err_is_ok(err));//TODO: handle error instead

        /* set the capref for each buffer into the new cnode and set
         * appropriate offset in memobj */
        for (int i = 0; i < pool->num_buffers; ++i) {
            struct bulk_buffer *buf = pool->buffers[i];
            buf_cap.slot = i;
            size_t offset = (i * pool->buffer_size);

            err = bulk_buffer_assign_cap(buf, buf_cap, 0);
            assert(err_is_ok(err)); /* this should not fail */

            err = memobj->f.fill(memobj, offset, buf->cap,
                                 buf->cap_offset);
            if (err_is_fail(err)) {
                /* TODO: error handling - delete all our new caps? */
                return err_push(err, LIB_ERR_MEMOBJ_FILL);
            }
            buf->address = (void *) vspace_genvaddr_to_lvaddr(address + offset);
            err = memobj->f.pagefault(memobj, vregion, offset, 0);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
            }

            buf->state = BULK_BUFFER_READ_WRITE;
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
        (bufs + i)->bufferid = i;
        pool->buffers[i] = bufs + i;
    }

    return SYS_ERR_OK;
}

/**
 * allocates the data structures for the pool.
 *
 * @param   pool            storage for pointer to newly allocated pool
 * @param   buffer_count    the number of buffers in the pool
 * @param   buffer_size     the size of a single buffer
 * @param   id              pool id
 */
errval_t bulk_pool_alloc_with_id(struct bulk_pool **pool,
                                 size_t buffer_count,
                                 size_t buffer_size,
                                 struct bulk_pool_id id)
{
    errval_t err;
    struct bulk_pool_internal *pool_int;

    /* allocate memory for the pool struct */

    pool_int = malloc(sizeof(struct bulk_pool_internal));

    if (pool_int == NULL) {
        return BULK_TRANSFER_MEM;
    }

    memset(pool_int, 0, sizeof(struct bulk_pool_internal));

    pool_int->pool.id = id;

    pool_int->pool.buffer_size = buffer_size;
    pool_int->pool.num_buffers = buffer_count;
    pool_int->pool.trust = BULK_TRUST_UNINITIALIZED;

    err = bulk_pool_init_bufs(&pool_int->pool);
    if (err_is_fail(err)) {
        return err;
    }

    bulk_pool_domain_list_insert(&pool_int->pool);
    *pool = &pool_int->pool;
    return SYS_ERR_OK;
}

/**
 * allocates the data structures for the pool with new id.
 *
 * @param   pool            storage for pointer to newly allocated pool
 * @param   buffer_count    the number of buffers in the pool
 * @param   buffer_size     the size of a single buffer
 */
errval_t bulk_pool_alloc(struct bulk_pool **pool,
                         size_t buffer_count,
                         size_t buffer_size)
{
    struct bulk_pool_id id;
    bulk_pool_generate_id(&id);
    return bulk_pool_alloc_with_id(pool, buffer_count, buffer_size, id);
}

/**
 * frees up the resources needed by the pool note
 *
 * @param pool  the pool to dealloc
 */
errval_t bulk_pool_dealloc(struct bulk_pool *pool)
{
    /* all the buffers were malloced once */
    free(pool->buffers[0]);
    free(pool->buffers);
    free(pool);

    return SYS_ERR_OK;
}
