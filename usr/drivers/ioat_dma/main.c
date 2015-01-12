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

#include <pci/devids.h>

#include <dma/dma.h>
#include <dma/dma_device.h>
#include <dma/dma_service.h>
#include <dma/dma_manager_client.h>
#include <dma/ioat/ioat_dma.h>
#include <dma/dma_bench.h>
#include "device.h"
#include "dma_service.h"
#include "debug.h"

#include <barrelfish/waitset.h>

#define IOAT_BENCHMARK_CORE 20

#define IOAT_IDLE_COUNTER 0xFFF

static struct dma_service_cb dma_svc_cb = {
    .connect = dma_svc_connect_cb,
    .addregion = dma_svc_addregion_cb,
    .removeregion = dma_svc_removeregion_cb,
    .memcpy = dma_svc_memcpy_cb
};

#if 0
#define BUFFER_SIZE (1<<22)

static void impl_test_cb(errval_t err, ioat_dma_req_id_t id, void *arg)
{
    debug_printf("impl_test_cb\n");
    assert(memcmp(arg, arg + BUFFER_SIZE, BUFFER_SIZE) == 0);
    debug_printf("test ok\n");
}

static void impl_test(void)
{
    errval_t err;

    debug_printf("Doing an implementation test\n");

    struct capref frame;
    err = frame_alloc(&frame, 2 * BUFFER_SIZE, NULL);
    assert(err_is_ok(err));

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    void *buf;
    err = vspace_map_one_frame(&buf, 1UL << id.bits, frame, NULL, NULL);
    assert(err_is_ok(err));

    memset(buf, 0, 1UL << id.bits);
    memset(buf, 0xA5, BUFFER_SIZE);

    struct ioat_dma_req_setup setup = {
        .type = IOAT_DMA_REQ_TYPE_MEMCPY,
        .src = id.base,
        .dst = id.base + BUFFER_SIZE,
        .bytes = BUFFER_SIZE,
        .done_cb = impl_test_cb,
        .arg = buf
    };
    int reps = 10;
    do {
        debug_printf("!!!!!! NEW ROUND\n");
        err = ioat_dma_request_memcpy(dma_ctrl.devices, &setup);
        assert(err_is_ok(err));

        uint32_t i = 10;
        while(i--) {
            ioat_dma_device_poll_channels(dma_ctrl.devices);
        }
    }while(reps--);
}
#endif

int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("I/O AT DMA driver started\n");

    /*
     * Parsing of cmdline arguments.
     *
     * When started by Kaluga, the last element of the cmdline will contain
     * the basic PCI information of the device.
     * VENDORID:DEVICEID:BUS:DEV:FUN
     */
    uint32_t vendor_id, device_id;

    struct pci_addr addr = {
        .bus = PCI_ADDR_DONT_CARE,
        .device = PCI_ADDR_DONT_CARE,
        .device = PCI_ADDR_DONT_CARE
    };

    enum device_type devtype = IOAT_DEVICE_INVAL;

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1], "%x:%x:%x:%x:%x", &vendor_id,
                                 &device_id, &addr.bus, &addr.device,
                                 &addr.function);
        if (parsed != 5) {
            DEBUGPRINT("WARNING: cmdline parsing failed. Using PCI Address [0,0,0]");
        } else {
            if (vendor_id != 0x8086) {
                USER_PANIC("unexpected vendor [%x]", vendor_id);
            }
            switch ((device_id & 0xFFF0)) {
                case PCI_DEVICE_IOAT_IVB0:
                    devtype = IOAT_DEVICE_IVB;
                    break;
                case PCI_DEVICE_IOAT_HSW0:
                    devtype = IOAT_DEVICE_HSW;
                    break;
                default:
                    USER_PANIC("unexpected device [%x]", device_id)
                    ;
                    break;
            }

            DEBUGPRINT("Initializing I/O AT DMA device with PCI address [%u,%u,%u]\n",
                       addr.bus, addr.device, addr.function);
        }
    } else {
        DEBUGPRINT("WARNING: Initializing I/O AT DMA device with unknown PCI address "
                   "[0,0,0]\n");
    }

    err = ioat_device_discovery(addr, devtype, IOAT_DMA_OPERATION);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "DMA Device discovery failed");
    }

#if DMA_BENCH_RUN_BENCHMARK
    if (disp_get_core_id() < IOAT_BENCHMARK_CORE) {
        struct ioat_dma_device *dev = ioat_device_get_next();
        dma_bench_run_default((struct dma_device *)dev);
    }
#endif

#if IOAT_DMA_OPERATION == IOAT_DMA_OPERATION_SERVICE

    iref_t svc_iref;
    char svc_name[30];
    uint8_t numa_node = (disp_get_core_id() >= 20);
    snprintf(svc_name, 30, "%s.%u", IOAT_DMA_SERVICE_NAME, numa_node);
    err = dma_service_init_with_name(svc_name, &dma_svc_cb, NULL, &svc_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to start the DMA service");
    }

    err = dma_manager_register_driver(0, 1ULL << 40, DMA_DEV_TYPE_IOAT, svc_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to register with the DMA manager\n");
    }

    DEBUGPRINT("Driver registered with DMA manager. Serving requests now.\n");

#endif

#if IOAT_DMA_OPERATION == IOAT_DMA_OPERATION_LIBRARY

#endif

    uint32_t idle_counter = IOAT_IDLE_COUNTER;
    while (1) {
        uint8_t idle = 0x1;
        err = ioat_device_poll();
        switch (err_no(err)) {
            case DMA_ERR_DEVICE_IDLE:
                break;
            case SYS_ERR_OK:
                idle = 0;
                break;
            default:
                debug_printf("I/O AT DMA driver terminated: in poll, %s\n",
                             err_getstring(err));
                return err;
        }
        err = event_dispatch_non_block(get_default_waitset());
        switch (err_no(err)) {
            case SYS_ERR_OK:
                idle = 0;
                break;
            case LIB_ERR_NO_EVENT:
                break;
            default:
                debug_printf("I/O AT DMA driver terminated in dispatch,  %s\n",
                             err_getstring(err));
                return err;
        }
        if (idle) {
            idle_counter--;
            if (idle_counter == 0) {
                thread_yield();
                idle_counter = IOAT_IDLE_COUNTER;
            }
        } else {
            idle_counter = IOAT_IDLE_COUNTER;
        }
    }

    debug_printf("I/O AT DMA driver terminated");

    return 0;
}

