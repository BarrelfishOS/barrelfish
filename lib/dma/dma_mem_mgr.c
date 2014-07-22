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

#include <dma_internal.h>
#include <dma/dma_mem_mgr.h>

#include <debug.h>

/*
 * XXX: we are using a ordered linked list here instead of a proper tree structure
 *      assumption: the number of registered memory regions per binding is low
 *      and thus the lookup times acceptable low.
 */

/**
 * represents a tree of registered memory regions
 */
struct dma_mem_tree
{
    struct dma_mem_node *root;
    size_t num_entries;
};

/**
 * represents a registered memory region
 */
struct dma_mem_node
{
    struct dma_mem_node *prev;  ///< the previous region
    struct dma_mem_node *next;  ///< the next region
    struct capref cap;          ///< capability backing this region
    lpaddr_t paddr;             ///< converted physical address of the region
    size_t size;                ///< cached size in bytes of the region
};

/**
 * represents the state of a DMA memory region manager
 */
struct dma_mem_mgr
{
    struct dma_mem_tree regions;  ///< registered memory regions
    lpaddr_t range_min;          ///< minimum allowed memory address
    lpaddr_t range_max;          ///< maximum allowed memory address
    dma_mem_convert_fn convert;  ///< converts the registered address to a dma usable
    void *convert_arg;           ///< argument for the convert function
};

/*
 * ----------------------------------------------------------------------------
 * Memory Region Management
 * ----------------------------------------------------------------------------
 */
static errval_t mem_tree_init(struct dma_mem_tree *tree)
{
    tree->root = NULL;
    tree->num_entries = 0;

    return SYS_ERR_OK;
}

/*
 * returns the closest element to a given address.
 *
 * \returns dma_mem_node the previous element
 *          NULL if the closest would be before the first element
 */
static struct dma_mem_node *mem_tree_find_nearest(struct dma_mem_tree *tree,
                                                  lpaddr_t base)
{
    struct dma_mem_node *node = tree->root;
    struct dma_mem_node *ret = tree->root;
    assert(node);
    while (node) {
        if (node->paddr > base) {
            return node->prev;
        }
        ret = node;
        node = node->next;
    }
    assert(ret);
    return ret;
}

static struct dma_mem_node *mem_tree_lookup(struct dma_mem_tree *tree,
                                            lpaddr_t base,
                                            size_t size)
{
    if (tree->root == NULL) {
        return NULL;
    }

    struct dma_mem_node *node = mem_tree_find_nearest(tree, base);
    if (node == NULL) {
        return NULL;
    }
    if (node->paddr <= base) {
        /* the base may be within the region of this node. check size */
        if ((node->paddr + node->size) >= (base + size)) {
            /* its within the registered region. Found!*/
            return node;
        }
    }
    return NULL;
}

static errval_t mem_tree_insert(struct dma_mem_tree *tree,
                                struct dma_mem_node *entry)
{
    if (tree->root == NULL) {
        assert(tree->num_entries == 0);
        entry->next = NULL;
        entry->prev = NULL;
        tree->root = entry;
        tree->num_entries = 1;
        return SYS_ERR_OK;
    }

    struct dma_mem_node *node = mem_tree_find_nearest(tree, entry->paddr);

    if (node == NULL) {
        /* insert at the beginning */
        entry->prev = NULL;
        entry->next = tree->root;
        tree->root->prev = entry;
        tree->num_entries++;
        return SYS_ERR_OK;
    }
    if (node->paddr <= entry->paddr) {
        /* the base may be within the region of this node. check size */
        if ((node->paddr + node->size) >= (entry->paddr + entry->size)) {
            /* this would lie within a already registered region */
            return DMA_ERR_MEM_OVERLAP;
        }
    }

    if (node->next) {
        node->next->prev = entry;
    }
    entry->prev = node;
    entry->next = node->next;
    node->next = entry;
    tree->num_entries++;

    return SYS_ERR_OK;
}

static struct dma_mem_node *mem_tree_remove(struct dma_mem_tree *tree,
                                            lpaddr_t base)
{
    if (tree->root == NULL) {
        assert(tree->num_entries == 0);
        return NULL;
    }
    struct dma_mem_node *node = mem_tree_lookup(tree, base, 0);
    if (node == NULL) {
        return NULL;
    }

    if (node->prev) {
        node->prev->next = node->next;
    } else {
        tree->root = node->next;
    }

    if (node->next) {
        node->next->prev = node->prev;
    }

    assert(tree->root || (tree->num_entries == 1));

    tree->num_entries--;

    node->next = NULL;
    node->prev = NULL;

    return node;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the DMA memory region manager
 *
 * \param mem_mgr   returned pointer to the mem manager structure
 * \param range_min minimum allowed memory address
 * \param range_max maximum allowed memory address
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_mem_mgr_init(struct dma_mem_mgr **mem_mgr,
                          lpaddr_t range_min,
                          lpaddr_t range_max)
{
    errval_t err;

    struct dma_mem_mgr *mgr = calloc(1, sizeof(*mgr));
    if (mgr == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    err = mem_tree_init(&mgr->regions);
    if (err_is_fail(err)) {
        free(mgr);
        return err;
    }

    mgr->range_max = range_max;
    mgr->range_min = range_min;

    *mem_mgr = mgr;

    return SYS_ERR_OK;
}

/**
 * \brief sets the address conversion function to be used for translating the
 *        addresses
 *
 * \param mem_mgr   DMA memory manager
 * \param fn        convert function to be called
 * \param arg       argument supplied for the convert function
 */
void dma_mem_mgr_set_convert_fn(struct dma_mem_mgr *mem_mgr,
                                dma_mem_convert_fn fn,
                                void *arg)
{
    mem_mgr->convert = fn;
    mem_mgr->convert_arg = arg;
}

/**
 * \brief registers a memory region to be used for DMA transfers
 *
 * \param mem_mgr   DMA memory manager
 * \param cap       frame capability of the memory region to register
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_mem_register(struct dma_mem_mgr *mem_mgr,
                          struct capref cap)
{
    errval_t err;

    struct frame_identity frame_id;
    err = invoke_frame_identify(cap, &frame_id);
    if (err_is_fail(err)) {
        return err;
    }

    DMAMEM_DEBUG("registering DMA memory range [0x%016lx, 0x%016lx]\n",
                 frame_id.base, frame_id.base + (1UL << frame_id.bits));

    struct dma_mem_node *entry = calloc(1, sizeof(*entry));
    if (entry == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    entry->cap = cap;
    entry->paddr = frame_id.base;
    entry->size = (1UL << frame_id.bits);

    if (mem_mgr->convert) {
        entry->paddr = mem_mgr->convert(mem_mgr->convert_arg, frame_id.base,
                                        entry->size);
        DMAMEM_DEBUG("converted base address [0x%016lx] -> [0x%016lx]\n",
                     frame_id.base, entry->paddr);
    }

    if ((entry->paddr == 0) || (entry->paddr < mem_mgr->range_min)
        || ((entry->paddr + entry->size) > mem_mgr->range_max)) {
        free(entry);
        return DMA_ERR_MEM_OUT_OF_RANGE;
    }

    err = mem_tree_insert(&mem_mgr->regions, entry);
    if (err_is_fail(err)) {
        free(entry);
        return err;
    }

    return SYS_ERR_OK;

}

/**
 * \brief deregisters a memory region that it cannot longer be used for the
 *        memory manager
 *
 * \param mem_mgr   DMA memory manager
 * \param cap       frame capability of the memory region to register
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_mem_deregister(struct dma_mem_mgr *mem_mgr,
                            struct capref cap)
{
    errval_t err;

    struct frame_identity frame_id;
    err = invoke_frame_identify(cap, &frame_id);
    if (err_is_fail(err)) {
        return err;
    }

    DMAMEM_DEBUG("deregister DMA memory range [0x%016lx, 0x%016lx]\n",
                 frame_id.base, frame_id.base + (1UL << frame_id.bits));

    lpaddr_t addr = frame_id.base;
    if (mem_mgr->convert) {
        addr = mem_mgr->convert(mem_mgr->convert_arg, frame_id.base,
                                (1UL << frame_id.bits));
        DMAMEM_DEBUG("converted base address [0x%016lx] -> [0x%016lx]\n",
                     frame_id.base, addr);
    }

    if (addr == 0) {
        return DMA_ERR_MEM_OUT_OF_RANGE;
    }

    struct dma_mem_node *entry = mem_tree_remove(&mem_mgr->regions, addr);
    if (entry) {
        free(entry);
        return SYS_ERR_OK;
    }

    return DMA_ERR_MEM_NOT_REGISTERED;
}

/**
 * \brief verifies if a addres-length pair lies completely within a
 *        registered memory region and translates the address
 *
 * \param mem_mgr   DMA memory manager
 * \param addr      address to be looked up
 * \param bytes     length of the transfer in bytes
 * \param dma_addr  translated base address (if change in address space)
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_MEM_NOT_REGISTERED if the memory region is not registered
 *          DMA_ERR_OUT_OF_RANGE if the memory region is out of range
 */
errval_t dma_mem_verify(struct dma_mem_mgr *mem_mgr,
                        lpaddr_t addr,
                        size_t bytes,
                        lpaddr_t *dma_addr)
{
    lpaddr_t daddr = addr;

    if (mem_mgr->convert) {
        daddr = mem_mgr->convert(mem_mgr->convert_arg, addr, bytes);
        DMAMEM_DEBUG("converted base address [0x%016lx] -> [0x%016lx]\n", addr,
                     daddr);
    }

    DMAMEM_DEBUG("Verify DMA memory range [0x%016lx, 0x%016lx]\n", daddr,
                  daddr + bytes);

    if ((daddr == 0) || (daddr < mem_mgr->range_min)
        || ((daddr + bytes) > mem_mgr->range_max)) {
        return DMA_ERR_MEM_OUT_OF_RANGE;
    }

    struct dma_mem_node *entry = mem_tree_lookup(&mem_mgr->regions, daddr, bytes);
    if (entry) {
        *dma_addr = daddr;
        return SYS_ERR_OK;
    }

    return DMA_ERR_MEM_NOT_REGISTERED;
}
