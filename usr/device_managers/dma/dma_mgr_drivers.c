/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <dma/dma_manager_client.h>

#include "dma_mgr.h"
#include "debug.h"

struct dma_service
{
    struct dma_mgr_driver_info info;
    struct dma_service *prev;
    struct dma_service *next;
};

static struct dma_service *dma_services = NULL;

static void service_insert(struct dma_service *svc)
{
    if (dma_services == NULL) {
        svc->next = NULL;
        svc->prev = NULL;
        dma_services = svc;
        return;
    }

    struct dma_service *current = dma_services;
    while (current) {
        if (current->info.mem_low > svc->info.mem_low) {
            if (current->prev) {
                svc->prev = current->prev;
                current->prev->next = svc;
            } else {
                svc->prev = NULL;
                dma_services = svc;
            }
            svc->next = current;
            current->prev = svc;
            break;
        }
        if (current->next == NULL) {
            current->next = svc;
            svc->prev = current;
            break;
        }
        current = current->next;
    }
}

static inline uint8_t get_diff(uint8_t a,
                               uint8_t b)
{
    if (a > b) {
        return a - b;
    } else {
        return b - a;
    }
}

static struct dma_service *service_lookup(lpaddr_t mem_low,
                                          size_t size,
                                          uint8_t numa_node)
{

    struct dma_service *current = dma_services;
    struct dma_service *best = NULL;
    while (current) {
        if (current->info.mem_low <= mem_low) {
            if (current->info.mem_high >= (mem_low + size)) {
                /* we have a match */
                if (best == NULL) {
                    best = current;
                } else {
                    uint8_t best_numa_diff = get_diff(numa_node,
                                                      best->info.numa_node);
                    uint8_t curr_numa_diff = get_diff(numa_node,
                                                      current->info.numa_node);
                    if (curr_numa_diff < best_numa_diff) {
                        best = current;
                    }
                }
            }
        }
        current = current->next;
    }
    return best;
}

static struct dma_service *service_lookup_iref(iref_t iref)
{
    struct dma_service *current = dma_services;
    while(current) {
        if (current->info.iref == iref) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

/*
 * ============================================================================
 *
 * ============================================================================
 */

errval_t driver_store_init(void)
{
    return SYS_ERR_OK;
}

errval_t driver_store_insert(lpaddr_t mem_low,
                             lpaddr_t mem_high,
                             uint8_t numa_node,
                             uint8_t type,
                             iref_t iref)
{
    struct dma_service *driver = calloc(1, sizeof(*driver));
    if (driver == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    DS_DEBUG("insert: {%016lx, %016lx, %u, %u} @ %x\n", mem_low, mem_high, numa_node,
             type, iref);

    driver->info.mem_low = mem_low;
    driver->info.mem_high = mem_high;
    driver->info.numa_node = numa_node;
    driver->info.type = type;
    driver->info.iref = iref;

    service_insert(driver);

    return SYS_ERR_OK;
}

errval_t driver_store_lookup_by_iref(iref_t iref,
                                     struct dma_mgr_driver_info **info)
{
    DS_DEBUG("lookup: iref:%"PRIxIREF"\n", iref);

    struct dma_service *svc = service_lookup_iref(iref);
    if (svc == NULL) {
        DS_DEBUG("no such service: iref:%"PRIxIREF"\n", iref);
        return DMA_ERR_SVC_VOID;
    }

    *info = &svc->info;

    return SYS_ERR_OK;
}

errval_t driver_store_lookup(lpaddr_t mem_low,
                             size_t size,
                             uint8_t numa_node,
                             struct dma_mgr_driver_info **info)
{

    DS_DEBUG("lookup: {%016lx, %016lx, %u}\n", mem_low, size, numa_node);

    struct dma_service *svc = service_lookup(mem_low, size, numa_node);
    if (svc == NULL) {
        return DMA_ERR_SVC_VOID;
    }

    DS_DEBUG("lookup: {%016lx, %016lx, %u} @ %x\n", mem_low, size, numa_node,
             svc->info.iref);

    *info = &svc->info;

    return SYS_ERR_OK;
}
