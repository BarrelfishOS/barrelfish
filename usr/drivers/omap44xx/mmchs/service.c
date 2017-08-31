/**
 * \file
 * \brief Implementation of ata_rw28.if interface (to enable working vfs_fat)
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <barrelfish/nameservice_client.h>

#include <if/ata_rw28_defs.h>
#include <thc/thc.h>

#include "mmchs.h"
#include "mmchs_debug.h"

#define SECTION_SIZE 512
#define SECTION_ROUND_UP(x) ( ((x) + (SECTION_SIZE-1))  & (~(SECTION_SIZE-1)) )

static void read_dma(struct ata_rw28_binding *sv,
                     uint32_t read_size, uint32_t start_lba)
{
    struct mmchs_driver_state* st = (struct mmchs_driver_state*)sv->st;

    size_t buffer_size = SECTION_ROUND_UP(read_size);
    MMCHS_DEBUG("%s:%d read_size=%d buffer_size=%d\n", __FUNCTION__, __LINE__, read_size, buffer_size);
    void *buffer = malloc(buffer_size);
    assert(buffer != NULL);

    uint8_t *bufptr = (uint8_t *)buffer;
    for (size_t i = 0; i < (buffer_size / SECTION_SIZE); i++) {
        MMCHS_DEBUG("%s:%d: i=%d start_lba=%d\n", __FUNCTION__, __LINE__, i, start_lba);
        errval_t err = mmchs_read_block(st, start_lba+i, bufptr);
        assert(err_is_ok(err));
        bufptr += SECTION_SIZE;
    }

    errval_t err = ata_rw28_read_dma_response__tx(sv, MKCLOSURE(free, buffer), buffer, buffer_size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
}

static void read_dma_block(struct ata_rw28_binding *sv, uint32_t lba)
{
    MMCHS_DEBUG("%s:%d lba=%d\n", __FUNCTION__, __LINE__, lba);

    struct mmchs_driver_state* st = (struct mmchs_driver_state*)sv->st;

    void *buffer = malloc(SECTION_SIZE);
    assert(buffer != NULL);

    errval_t err = mmchs_read_block(st, lba, buffer);
    assert(err_is_ok(err));

    err = ata_rw28_read_dma_block_response__tx(sv, MKCLOSURE(free, buffer), buffer, SECTION_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
}

/*static void write_dma(struct ata_rw28_binding *sv,
                      uint8_t *buffer, size_t buffer_size, uint32_t lba)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    errval_t err = ata_rw28_write_dma_response__tx(sv, MKCLOSURE(free, buffer), LIB_ERR_NOT_IMPLEMENTED);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
}*/

static void identify_device(struct ata_rw28_binding *sv)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    errval_t err = ata_rw28_identify_device_response__tx(sv, NOP_CONT, NULL, 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
}

static void flush_cache(struct ata_rw28_binding *sv)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    errval_t err = ata_rw28_flush_cache_response__tx(sv, NOP_CONT, SYS_ERR_OK);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
}

static struct export_state {
    struct ata_rw28_binding* b;
    bool is_done;
    errval_t err;
    iref_t iref;
} service_export;

static void export_cb(void *st, errval_t err, iref_t iref)
{
    service_export.is_done = true;
    service_export.err = err;
    service_export.iref = iref;

    if (err_is_ok(err)) {
        MMCHS_DEBUG("Exported ddomain service with iref: %"PRIu32"\n", iref);
        err = nameservice_register("mmchs", iref);
        assert(err_is_ok(err));
    }
}
static const struct ata_rw28_rx_vtbl rx_vtbl = {
    .read_dma_call = read_dma,
    .read_dma_block_call = read_dma_block,
    //.write_dma_call = write_dma,
    .identify_device_call = identify_device,
    .flush_cache_call = flush_cache,
};

static errval_t client_connect(void *st, struct ata_rw28_binding *b)
{
    service_export.b = b;
    b->st = st;
    return SYS_ERR_OK;
}


void mmchs_init_service(struct mmchs_driver_state* st, iref_t* iref)
{
    errval_t err;
    MMCHS_DEBUG("%s:%d: Starting server\n", __FUNCTION__, __LINE__);
    err = ata_rw28_export(st, export_cb, client_connect, get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }

    while(!service_export.is_done) {
        messages_wait_and_handle_next();
    }
    *iref = service_export.iref;
    MMCHS_DEBUG("Service mmchs exported.\n");
}
