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
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>

#include <dma/dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#define DMA_BUFFER_SIZE  16
#define DMA_BUFFER_COUNT 4

#define EXPECT_SUCCESS(err, msg) if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}

static struct capref frame;
static size_t frame_size;
static lpaddr_t frame_addr;
static void *frame_virt;
static uint8_t *buffers[DMA_BUFFER_COUNT];
static lpaddr_t phys[DMA_BUFFER_COUNT];

static void done_cb(errval_t err, dma_req_id_t id, void *arg)
{
    debug_printf("done_cb: %016lx, %s\n",id, err_getstring(err));
    assert(memcmp(buffers[0], buffers[1], (DMA_BUFFER_SIZE << 20)) == 0);
}

static void prepare(void)
{
    errval_t err;

    debug_printf("Preparing resources...\n");

    err = frame_alloc(&frame, DMA_BUFFER_COUNT * (DMA_BUFFER_SIZE << 20),
                      &frame_size);
    EXPECT_SUCCESS(err, "allocating frame");

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    EXPECT_SUCCESS(err, "Frame identify");

    assert(frame_size == (1UL << id.bits));
    frame_addr = id.base;

    err = vspace_map_one_frame(&frame_virt, frame_size, frame, NULL, NULL);
    EXPECT_SUCCESS(err, "Mapping of frame");

    uint8_t *b = frame_virt;
    lpaddr_t p = frame_addr;
    for (uint32_t i = 0; i < DMA_BUFFER_COUNT; ++i) {
        buffers[i] = b;
        phys[i] = p;
        b += ((DMA_BUFFER_SIZE << 20));
        p += ((DMA_BUFFER_SIZE << 20));
    }

    debug_printf("preparation done.\n");
}

int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("DMA Test domain started\n");

    prepare();

#if 0
    char svc_name[30];
    uint8_t numa_node = (disp_get_core_id() >= 20);
    snprintf(svc_name, 30, "ioat_dma_svc.%u", numa_node);
    iref_t iref;
    err = nameservice_blocking_lookup(svc_name, &iref);
#endif

    err = dma_manager_wait_for_driver(DMA_DEV_TYPE_IOAT, 0);
    EXPECT_SUCCESS(err, "waiting for driver");

    struct dma_client_info info = {
        .type = DMA_CLIENT_INFO_TYPE_NAME,
        .device_type = DMA_DEV_TYPE_IOAT,
        .args = {
            .name = "ioat_dma_svc.0"
        }
    };

    struct dma_client_device *dev;
    err = dma_client_device_init(&info, &dev);

    err = dma_register_memory((struct dma_device *)dev, frame);
    EXPECT_SUCCESS(err, "registering memory\n");

    memset(buffers[0], 0x5A, (DMA_BUFFER_SIZE << 20));
    memset(buffers[1], 0, (DMA_BUFFER_SIZE << 20));
    assert(memcmp(buffers[0], buffers[1], (DMA_BUFFER_SIZE << 20)) != 0);

    dma_req_id_t id;

    struct dma_req_setup setup = {
        .done_cb = done_cb,
        .cb_arg = NULL,
        .args =  {
            .memcpy = {
                .src = phys[0],
                .dst = phys[1],
                .bytes = (DMA_BUFFER_SIZE << 20)
            }
        }
    };


    uint32_t count = 0x1F;
    while(count--) {

        printf("\n\n=============================================\n\n");

        err = dma_request_memcpy((struct dma_device *)dev, &setup, &id);
        EXPECT_SUCCESS(err, "memcpy memory");
        err = event_dispatch_non_block(get_default_waitset());
        if (err_no(err) == LIB_ERR_NO_EVENT) {
            err = SYS_ERR_OK;
        }
        EXPECT_SUCCESS(err, "event dispatch");
    }
    while (1) {
        messages_wait_and_handle_next();
    }

    debug_printf("I/O AT DMA driver terminated\n");

    return 0;
}

