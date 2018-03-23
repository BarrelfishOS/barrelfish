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
#include <barrelfish/nameservice_client.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>

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
#include <pci/pci_types.h>

#define IOAT_BENCHMARK_CORE 20

#define IOAT_IDLE_COUNTER 0xFFF

#if 0
static struct dma_service_cb dma_svc_cb = {
    .connect = dma_svc_connect_cb,
    .addregion = dma_svc_addregion_cb,
    .removeregion = dma_svc_removeregion_cb,
    .memcpy = dma_svc_memcpy_cb
};
#endif



int main(int argc, char *argv[])
{
    errval_t err;
    debug_printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
    debug_printf("I/O AT DMA driver started\n");


#if DMA_BENCH_RUN_BENCHMARK
    if (disp_get_core_id() < IOAT_BENCHMARK_CORE) {
        struct ioat_dma_device *dev = ioat_device_get_next();
        dma_bench_run_default((struct dma_device *)dev);
    }
#endif

    err = driverkit_iommu_client_init(NULL_CAP, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize the IOMMU library");
    }

    debug_printf("IOMMU PRESENT: %u", driverkit_iommu_present(NULL));


    iref_t kaluga_iref = 0;
    err = nameservice_blocking_lookup("ddomain_controller", &kaluga_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to connect to ddomain controller");
    }

    err = ddomain_communication_init(kaluga_iref, atoi(argv[2]));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initiate communication with Kaluga");
    }


#if 0
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

