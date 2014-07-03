/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <xeon_phi/xeon_phi.h>

#include <if/xeon_phi_dma_defs.h>

#include "xeon_phi_internal.h"
#include "dma_mem.h"

struct xdma_mem
{
    struct xeon_phi *phi;
    struct xeon_phi_dma_binding *b;
    struct xdma_mem_entry *head;
    uint32_t num_entries;
};

struct xdma_mem_entry
{
    struct capref cap;
    lpaddr_t paddr;
    size_t size;
    enum xdma_mem_type type;
    struct xdma_mem_entry *next;
    struct xdma_mem_entry *prev;
};

static lpaddr_t xdma_mem_verify_base_address(struct xeon_phi *phi,
                                             lpaddr_t addr,
                                             size_t size)
{
#ifdef __k1om__
    if ((addr + size) > (1UL << 40)) {
        return 0;
    }
    return addr;
#else
    lpaddr_t apt_lo = phi->apt.pbase;
    lpaddr_t apt_hi = phi->apt.pbase + phi->apt.bytes;
    if ((addr >= apt_lo) && ((addr + size) <= apt_hi)) {
        /* we are within the GDDR range */
        return addr - apt_lo;
    } else if ((addr + size)< XEON_PHI_SYSMEM_SIZE) {
        /*
         * Xeon Phi does not support more host memory,
         * assume host memory starts at 0x0
         */
        return addr + XEON_PHI_SYSMEM_BASE;
    } else {
        return 0;
    }
#endif
}

/**
 * \brief initializes the memory manager for the range checks
 */
errval_t xdma_mem_init(struct xeon_phi_dma_binding *binding,
                       struct xeon_phi *phi)
{
    struct xdma_mem *xmem = calloc(1, sizeof(struct xdma_mem));
    if (xmem == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    xmem->b = binding;
    xmem->phi = phi;
    binding->st = xmem;

    return SYS_ERR_OK;
}

static struct xdma_mem_entry* xdma_mem_find_entry(struct xdma_mem *xmem,
                                                  lpaddr_t addr,
                                                  size_t size)
{
    struct xdma_mem_entry *head = xmem->head;

    if (head == NULL) {
        return NULL;
    }

    struct xdma_mem_entry *current = head;
    while (current) {
        if (current->paddr > addr) {
            return NULL;
        }
        if (current->paddr <= addr && (current->paddr + current->size)
                        >= (addr + size)) {
            return current;
        }
        current = current->next;
    }

    return NULL;
}

static uint8_t xdma_mem_insert_entry(struct xdma_mem *xmem,
                                     struct xdma_mem_entry *entry)
{
    if (xmem->num_entries == 0) {
        xmem->head = entry;
        entry->next = NULL;
        entry->prev = NULL;
        xmem->num_entries++;
        return 0x1;
    }

    struct xdma_mem_entry *current = xmem->head;
    while (current) {
        /*
         * insert the entry before the current one.
         */
        if (entry->paddr < current->paddr) {
            if (entry->paddr + entry->size > current->paddr) {
                /*
                 * XXX: we may want to coalesce the two regions here...
                 *      returning false for now.
                 */
                return 0;
            }
            /* do the insert */
            current->prev = entry;
            entry->next = current;
            entry->prev = current->prev;
            xmem->num_entries++;

            if (current == xmem->head) {
                xmem->head = entry;
            }
            return 0x1;
        }

        /*
         * insert the new element at the end
         */
        if (current->next == NULL) {
            current->next = entry;
            entry->prev = current;
            entry->next = NULL;
            xmem->num_entries++;
            return 0x1;
        }
        current = current->next;
    }
    assert(!"This should not happen");
    return 0x0;
}

static struct xdma_mem_entry *xdma_mem_remove_entry(struct xdma_mem *xmem,
                                                    lpaddr_t addr,
                                                    size_t size)
{
    struct xdma_mem_entry *entry = xdma_mem_find_entry(xmem, addr, size);
    if (entry == NULL) {
        return NULL;
    }
    xmem->num_entries--;

    if (xmem->num_entries == 0) {
        xmem->head = NULL;
        return entry;
    }

    if (entry->prev) {
        entry->prev->next = entry->next;
    } else {
        xmem->head = entry->next;
    }

    if (entry->next) {
        entry->next->prev = entry->prev;
    }

    entry->next = NULL;
    entry->prev = NULL;
    return entry;
}

/**
 * \brief registers a new block of memory to be used with the DMA engine
 */
errval_t xdma_mem_register(struct xeon_phi_dma_binding *binding,
                           struct capref cap)
{
    errval_t err;

    struct xdma_mem *xmem = binding->st;
    assert(xmem);

    struct frame_identity frame_id;
    err = invoke_frame_identify(cap, &frame_id);
    if (err_is_fail(err)) {
        return err;
    }



    XDMA_DEBUG("Registring DMA memory range [0x%016lx, 0x%016lx]\n",
               frame_id.base,
               frame_id.base + (1UL << frame_id.bits));

    struct xdma_mem_entry *entry = calloc(1, sizeof(struct xdma_mem_entry));
    if (entry == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /*
     * the DMA engine supports only 40bit addresses of the Xeon Phi address space.
     * thus we need to do a case distinction here and translate the addresses
     * appropriately
     */
    lpaddr_t addr = xdma_mem_verify_base_address(xmem->phi,
                                                 frame_id.base,
                                                 1UL << frame_id.bits);
    if (addr == 0) {
        XDMA_DEBUG("Memory address out of range or on other Xeon Phi\n");
        free(entry);
        return XEON_PHI_ERR_DMA_MEM_OUT_OF_RANGE;
    }
    if (addr > XEON_PHI_SYSMEM_BASE) {
        entry->type = XDMA_MEM_TYPE_HOST;
    } else {
        entry->type = XDMA_MEM_TYPE_CARD;
    }
    entry->cap = cap;
    entry->paddr = addr;
    entry->size = 1UL << frame_id.bits;

    if (!xdma_mem_insert_entry(xmem, entry)) {
        free(entry);
        return XEON_PHI_ERR_DMA_MEM_OVERLAP;
    }

    return SYS_ERR_OK;
}

/**
 * \brief removes a block of memory from the usable regions
 */
errval_t xdma_mem_deregister(struct xeon_phi_dma_binding *binding,
                             struct capref cap)
{
    errval_t err;

    struct xdma_mem *xmem = binding->st;
    assert(xmem);

    struct frame_identity frame_id;
    err = invoke_frame_identify(cap, &frame_id);
    if (err_is_fail(err)) {
        return err;
    }

    XDMAV_DEBUG("De-registring DMA memory range [0x%016lx, 0x%016lx]\n",
               frame_id.base,
               frame_id.base + (1UL << frame_id.bits));

    assert(!"NYI");

    /*
     * the DMA engine supports only 40bit addresses of the Xeon Phi address space.
     * thus we need to do a case distinction here and translate the addresses
     * appropriately
     */
    lpaddr_t addr = xdma_mem_verify_base_address(xmem->phi,
                                                 frame_id.base,
                                                 1UL << frame_id.bits);

    struct xdma_mem_entry *entry = xdma_mem_remove_entry(xmem,
                                                         addr,
                                                         (1UL << frame_id.bits));

    if (entry == NULL) {
        return XEON_PHI_ERR_DMA_MEM_REGISTERED;
    }

    return SYS_ERR_OK;
}

/**
 * \brief verifies the memory range and translates into the DMA usable format
 *
 * \param binding the Xeon Phi DMA binding the request originated
 * \param size    size of the requested range
 * \param addr    base address of the range
 *
 * \returns DMA address if the address has previously been registered
 *          0 otherwise
 */
lpaddr_t xdma_mem_verify(struct xeon_phi_dma_binding *binding,
                         lpaddr_t addr,
                         size_t size)
{
    struct xdma_mem *xmem = binding->st;
    assert(xmem);

    XDMAV_DEBUG("Verify DMA memory range [0x%016lx, 0x%016lx]\n", addr, addr + size);

    lpaddr_t dma_addr = xdma_mem_verify_base_address(xmem->phi, addr, size);
    if (dma_addr == 0) {
        return 0;
    }

    struct xdma_mem_entry *entry = xdma_mem_find_entry(xmem, dma_addr, size);
    if (entry) {
        return dma_addr;
    } else {
        return 0;
    }
}


struct xeon_phi *xdma_mem_get_phi(struct xeon_phi_dma_binding *binding)
{
    return ((struct xdma_mem *)binding->st)->phi;
}
