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

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/xeon_phi/xeon_phi_dma_dev.h>

#include "xeon_phi_internal.h"
#include "dma.h"
#include "dma_channel.h"
#include "dma_descriptor_ring.h"
#include "debug.h"

struct dma_info
{
    struct xdma_channel channels[XEON_PHI_DMA_CHAN_NUM];
    xeon_phi_dma_t dma_dev;
    uint8_t chan_alloc_next;
};

/**
 * \brief Initializes the DMA structure for the Xeon Phi
 *
 * \param phi the xeon phi DMA structure
 *
 * \return SYS_ERR_OK on success,
 */
errval_t dma_init(struct xeon_phi *phi)
{
    errval_t err;

    /* check if already initialized */
    if (phi->dma) {
        return SYS_ERR_OK;
    }

    struct dma_info *info = calloc(1, sizeof(struct dma_info));
    if (info == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

#ifdef __k1om__
    xeon_phi_dma_initialize(&info->dma_dev, (mackerel_addr_t) (phi->mmio.vbase));
#else
    xeon_phi_dma_initialize(&info->dma_dev, XEON_PHI_MMIO_TO_SBOX(phi));
#endif
    XDMA_DEBUG("initializing %u channels\n", XEON_PHI_DMA_CHAN_NUM);
    for (uint32_t i = 0; i < XEON_PHI_DMA_CHAN_NUM; ++i) {

        struct xdma_channel *chan = &info->channels[i];
        err = xdma_channel_init(chan,
                                XEON_PHI_DMA_DESC_NUM,
                                &info->dma_dev, i + XEON_PHI_DMA_CHAN_OFFSET,
                                XEON_PHI_DMA_USE_POLLING);
        if (err_is_fail(err)) {
            free(info);
            return err;
        }
    }

    phi->dma = info;

    return dma_service_init(phi);
}

/**
 * \brief polls the owned channels for competed transfers
 *
 * \param phi the xeon phi to poll
 *
 * \returns SYS_ERR_OK if there was a transfer completed and executed correctly
 *          XEON_PHI_ERR_DMA_IDLE if there was no transfer completed
 *          errno otherwise
 */
errval_t dma_poll_channels(struct xeon_phi *phi)
{
    errval_t err, ret_err = XEON_PHI_ERR_DMA_IDLE;
    for (uint32_t i = 0; i < XEON_PHI_DMA_CHAN_NUM; ++i) {
        err = xdma_channel_poll(phi->dma->channels + i);
        if (err_is_fail(err)) {
            if (err_no(err) == XEON_PHI_ERR_DMA_IDLE) {
                continue;
            }
            return err;
        } else {
            ret_err = SYS_ERR_OK;
        }
    }
    return ret_err;
}

/**
 * \brief issues a new DMA request
 *
 * \param phi   Xeon Phi to execute the request on
 * \param setup information about the request
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on failure
 */
errval_t dma_do_request(struct xeon_phi *phi,
                        struct dma_req_setup *setup)
{
    errval_t err = SYS_ERR_OK;

    struct xdma_channel *chan = &phi->dma->channels[phi->dma->chan_alloc_next++];

    /*
     * XXX: choosing the channel in a round robin fashion. One may want to select
     *      the one which is least busy.
     */

    if (phi->dma->chan_alloc_next >= XEON_PHI_DMA_CHAN_NUM) {
        phi->dma->chan_alloc_next = 0;
    }

    switch (setup->type) {
        case XDMA_REQ_TYPE_NOP:
            assert(!"NYI");
            break;
        case XDMA_REQ_TYPE_MEMCPY:
            err = xdma_channel_req_memcpy(chan, setup, setup->info.mem.dma_id);
            break;
        case XDMA_REQ_TYPE_STATUS:
            assert(!"NYI");
            break;
        case XDMA_REQ_TYPE_GENERAL:
            assert(!"NYI");
            break;
        case XDMA_REQ_TYPE_KEYNON:
            assert(!"NYI");
            break;
        case XDMA_REQ_TYPE_KEY:
            assert(!"NYI");
            break;
    }

    return err;
}

#define XDMA_TEST_BUFFER_SIZE (1024*1024)
#define XDMA_TEST_FRAME_SIZE (2*XDMA_TEST_BUFFER_SIZE)
errval_t dma_impl_test(struct xeon_phi *phi)
{
    errval_t err;
    struct capref frame;

    err = frame_alloc(&frame, XDMA_TEST_FRAME_SIZE, NULL);
    assert(err_is_ok(err));

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    void *buf;
    err = vspace_map_one_frame(&buf, XDMA_TEST_FRAME_SIZE, frame, NULL, NULL);
    assert(err_is_ok(err));

    lpaddr_t src = id.base;
    lpaddr_t dst = id.base + XDMA_TEST_BUFFER_SIZE;

    void *src_vbase = buf;
    void *dst_vbase = (uint8_t *) buf + XDMA_TEST_BUFFER_SIZE;
    debug_printf(" DMA-TEST | testing write of buffersize %u MB\n",
    XDMA_TEST_BUFFER_SIZE >> 20);
    debug_printf(" DMA-TEST | setting src buffer to 0xFF, target buffer to 0x0\n");
    memset(src_vbase, 0xFF, XDMA_TEST_BUFFER_SIZE);
    memset(dst_vbase, 0, XDMA_TEST_BUFFER_SIZE);

    debug_printf(" DMA-TEST | setup transfer\n");

    struct xdma_channel *chan = &phi->dma->channels[0];
    struct dma_req_setup setup = {
        .type = XDMA_REQ_TYPE_MEMCPY
    };
    setup.info.mem.dst = dst;
    setup.info.mem.src = src;
    setup.info.mem.bytes = XDMA_TEST_BUFFER_SIZE;
    xeon_phi_dma_id_t did;
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }

    debug_printf(" DMA-TEST | wait for completion\n");
    uint16_t idlecount = 0xFFFF;
    while (1) {
        err = xdma_channel_poll(chan);
        if (err_is_fail(err)) {
            if (err_no(err) == XEON_PHI_ERR_DMA_IDLE) {
                if (!idlecount--) {
                    break;
                }
            }
        }
    }

    debug_printf(" DMA-TEST | verify...\n");
    uint32_t *test = dst_vbase;
    for (uint32_t i = 0; i < XDMA_TEST_BUFFER_SIZE / sizeof(uint32_t); ++i) {
        if (*test != 0xFFFFFFFF) {
            USER_PANIC("expected %x, was %x @ [%u]\n", 0xFFFFFFFF, *test, i);
        }
        test++;
    }
    debug_printf(" DMA-TEST | SUCCESS!\n");

    debug_printf(" DMA-TEST | reset memory\n");
    memset(src_vbase, 0xAB, XDMA_TEST_BUFFER_SIZE);

    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }

    debug_printf(" DMA-TEST | wait for completion\n");
    idlecount = 0xFFFF;
    while (1) {
        err = xdma_channel_poll(chan);
        if (err_is_fail(err)) {
            if (err_no(err) == XEON_PHI_ERR_DMA_IDLE) {
                if (!idlecount--) {
                    break;
                }
            }
        }
    }

    debug_printf(" DMA-TEST | verify...\n");
    test = dst_vbase;
    for (uint32_t i = 0; i < XDMA_TEST_BUFFER_SIZE / sizeof(uint32_t); ++i) {
        if (*test != 0xABABABAB) {
            USER_PANIC("expected %x, was %x @ [%u]\n", 0xABABABAB, *test, i);
        }
        test++;
    }
    debug_printf(" DMA-TEST | SUCCESS!\n");

    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    err = xdma_channel_req_memcpy(chan, &setup, &did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "outcome of debug");
    }
    idlecount = 0xFFFF;
    while (1) {
        err = xdma_channel_poll(chan);
        if (err_is_fail(err)) {
            if (err_no(err) == XEON_PHI_ERR_DMA_IDLE) {
                if (!idlecount--) {
                    break;
                }
            }
        }
    }
    return SYS_ERR_OK;
}

#if 0
#define SBOX_SICR0_DMA(x)  (((x) >> 8) & 0xff)


/*
 * TODO;
 * Maybe move the logic into slow interrupt handler
 */
void
host_dma_interrupt_handler(mic_dma_handle_t dma_handle, uint32_t sboxSicr0reg)
{
    struct mic_dma_ctx_t *dma_ctx = (struct mic_dma_ctx_t *) dma_handle;
    uint32_t dma_chan_id;
    struct dma_channel *ch;

    for (dma_chan_id = 0; dma_chan_id < 8; dma_chan_id++) {
        if (SBOX_SICR0_DMA(sboxSicr0reg) & (0x1 << dma_chan_id)) {
            ch = &dma_ctx->dma_channels[dma_chan_id];
            if (ch->desc_ring)
            host_dma_lib_interrupt_handler(ch);
        }
    }
}

void
host_dma_lib_interrupt_handler(struct dma_channel *chan)
{
    ack_dma_interrupt(chan);
    mic_dma_lib_interrupt_handler(chan);
}

#endif

