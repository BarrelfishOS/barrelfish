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
#include <barrelfish/nameservice_client.h>

#include <dma/dma_manager_client.h>
#include <dma_internal.h>
#include <dma_client_internal.h>
#include <debug.h>

/**
 * state enumeration for the Flounder connection to the service
 */
enum dma_svc_state
{
    DMA_CLIENT_STATE_NS_LOOKUP,
    DMA_CLIENT_STATE_BINDING,
    DMA_CLIENT_STATE_BIND_OK,
    DMA_CLIENT_STATE_BIND_FAIL,
    DMA_CLIENT_STATE_CONNECTED
};

struct dma_client
{
    struct dma_binding *svc_b;
    enum dma_svc_state svc_st;       ///<
    struct dma_mgr_driver_info info;
    errval_t err;                    ///<
    struct dma_client *next;         ///<
    struct dma_client *prev;         ///<
};

//static uint8_t dma_client_initialized = 0x0;

/*
 * ---------------------------------------------------------------------------
 * Connection List Management
 * ---------------------------------------------------------------------------
 */
static struct dma_client *dma_svc_conn = NULL;

static void dma_svc_conn_insert(struct dma_client *client)
{
    if (dma_svc_conn == NULL) {
        client->next = NULL;
        client->prev = NULL;
        dma_svc_conn = client;
        return;
    }

    struct dma_client *current = dma_svc_conn;
    while (current) {
        if (current->info.mem_low > client->info.mem_high) {
            if (current->prev) {
                client->prev = current->prev;
                current->prev->next = client;
            } else {
                client->prev = NULL;
                dma_svc_conn = client;
            }
            client->next = current;
            current->prev = client;
            break;
        }
        current = current->next;
    }
}

static struct dma_client *dma_svc_conn_get_by_addr(lpaddr_t base,
                                                   size_t size)
{
    errval_t err;

    DMACLIENT_DEBUG("Looking up connection name for address: [%016lx]\n", base);

    struct dma_client *current = dma_svc_conn;
    struct dma_client *best = NULL;
    while (current) {
        if (current->info.mem_low <= base) {
            if (current->info.mem_high >= (base + size)) {
                /* we have a match */
                if (best == NULL) {
                    best = current;
                    break;
                }
            }
        }
        current = current->next;
    }
    if (best == NULL) {
        best = calloc(1, sizeof(*best));
        assert(best);
        err = dma_manager_lookup_driver(base, size, &best->info);
        if (err_is_fail(err)) {
            free(best);
            return NULL;
        }
        dma_svc_conn_insert(best);
    }
    return best;
}

/*
 * ---------------------------------------------------------------------------
 * Service Binding
 * ---------------------------------------------------------------------------
 */
#if 0
static void bind_cb(void *st,
                    errval_t err,
                    struct dma_binding *b)
{
    assert(st);
    struct dma_client *client = st;

    client->err = err;

    if (err_is_fail(err)) {
        DMACLIENT_DEBUG("svc bind: connection to {%s} failed.\n", client->svc_name);
        client->svc_st = DMA_CLIENT_STATE_BIND_FAIL;
        return;
    }

    client->svc_b = b;

    DMACLIENT_DEBUG("svc bind: connection to {%s} established.\n", client->svc_name);

    client->svc_st = DMA_CLIENT_STATE_BIND_OK;
}



static errval_t dma_service_bind(struct dma_client *client)
{
    errval_t err;

    if (client->svc_b != NULL) {
        return SYS_ERR_OK;
    }

    if (!dma_client_initialized) {
        err = dma_client_init();
        if (err_is_fail(err)) {
            return err;
        }
    }

    client->svc_st = DMA_CLIENT_STATE_NS_LOOKUP;

    client->svc_st = DMA_CLIENT_STATE_BINDING;

    DMACLIENT_DEBUG("svc bind: binding to iref [%"PRIxIREF"]\n", client->info.iref);
    struct waitset *ws = get_default_waitset();
    err = dma_bind(client->info.iref, bind_cb, client, ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (client->svc_st == DMA_CLIENT_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    if (client->svc_st == DMA_CLIENT_STATE_BIND_FAIL) {
        return client->err;
    }

    client->svc_st = DMA_CLIENT_STATE_CONNECTED;

    dma_svc_conn_insert(client);

    return SYS_ERR_OK;
}
#endif
/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 *
 */
errval_t dma_client_init(void)
{
    return SYS_ERR_OK;
}

/**
 * \brief gets a connection to the DMA service based on the src and dst addresses
 *        of the DMA request
 *
 * \param src   source address of the operation
 * \param dst   destination address of the operation
 * \param size  transfer size in bytes
 *
 * \returns dma_client connection
 *          NULL if the addresses are not supported
 */
struct dma_client *dma_client_get_connection_by_addr(lpaddr_t src,
                                                     lpaddr_t dst,
                                                     size_t size)
{
    DMACLIENT_DEBUG("getting connection for [%016lx] -> [%016lx]\n", src, dst);

    struct dma_client *src_client = dma_svc_conn_get_by_addr(src, size);
    if (src_client == NULL) {
        DMACLIENT_DEBUG("could not connect for source\n");
        return NULL;
    }

    struct dma_client *dst_client = dma_svc_conn_get_by_addr(dst, size);
    if (dst_client == NULL) {
        DMACLIENT_DEBUG("could not connect for destination\n");
        return NULL;
    }

    if (dst_client->info.type == DMA_DEV_TYPE_XEON_PHI) {
        return dst_client;
    } else if (src_client->info.type == DMA_DEV_TYPE_XEON_PHI) {
        return src_client;
    }
    /* TODO: check NUMA */
    return src_client;
}
