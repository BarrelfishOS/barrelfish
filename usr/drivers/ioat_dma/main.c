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

#include <string.h>

#include <barrelfish/barrelfish.h>

#include <pci/devids.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_request.h"
#include "ioat_dma_descriptors.h"

#include "debug.h"

struct ioat_dma_ctrl dma_ctrl;

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

int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("I/O AT DMA driver started\n");

    memset(&dma_ctrl, 0, sizeof(dma_ctrl));

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

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1], "%x:%x:%x:%x:%x", &vendor_id,
                                 &device_id, &addr.bus, &addr.device,
                                 &addr.function);
        if (parsed != 5) {
            IODEBUG("WARNING: cmdline parsing failed. Using PCI Address [0,0,0]");
        } else {
            if (vendor_id != 0x8086 || (((device_id & 0xFFF0) != PCI_DEVICE_IOAT_IVB0)
                            && ((device_id & 0xFFF0) != PCI_DEVICE_IOAT_HSW0))) {
                USER_PANIC("unexpected vendor / device id: [%x, %x]", vendor_id,
                           device_id);
                return -1;
            }
            IODEBUG("Initializing I/O AT DMA device with PCI address [%u,%u,%u]\n",
                    addr.bus, addr.device, addr.function);
        }
    } else {
        IODEBUG("WARNING: Initializing I/O AT DMA device with unknown PCI address "
                "[0,0,0]\n");
    }

    err = ioat_dma_device_discovery(addr, device_id, &dma_ctrl);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "DMA Device discovery failed");
    }

    /* TODO: start service */

    impl_test();

    return 0;
}

