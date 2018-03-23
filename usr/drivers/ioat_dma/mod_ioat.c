/**
 * \file
 * \brief Driver module example template.
 *
 * In summary, every driver (struct bfdriver) shall implement the five
 * life-cycle functions init/set_sleep_level/attach/detach/destroy
 * (along with additional IRQ handler functions etc.).
 *
 * A driver module is linked with a driver domain (see main.c in this directory).
 * At runtime, a driver domain will instantiate a driver instance (struct bfdriver_instance)
 * of a given module (or class if you want) using the `init` function. During the lifetime
 * of a driver instance it may be `detached` from and re-`attach`-ed to the
 * device, set in different sleep levels, and finally it can be `destroy`-ed.
 *
 * For every driver instance (i.e., struct bfdriver_instance), a corresponding
 * control interface created and exported. The interface is defined in dcontrol.if,
 * the corresponding code is located in the driverkit library (dcontrol_service.c).
 *
 * \note For more information about driver domains check the main.c file in this
 * directory.
 */
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>

#include <pci/pci.h>

#include <dma/dma.h>
#include <dma/dma_device.h>
#include <dma/dma_request.h>
#include <dma/ioat/ioat_dma.h>
#include <dma/ioat/ioat_dma_device.h>
#include <dma/ioat/ioat_dma_request.h>

#include "device.h"
#include "ioat_mgr_service.h"
#include "debug.h"

static uint8_t device_next = 0;
static uint8_t device_count = 0;
struct ioat_dma_device **devices = NULL;

#if 0
static void handle_device_interrupt(void *arg)
{

//    struct ioat_dma_device *dev = *((struct ioat_dma_device **) arg);
//    struct dma_device *dma_dev = (struct dma_device *) dev;

    INTR_DEBUG("interrupt! device %u", dma_device_get_id(arg));

}
#endif

struct ioat_dma_device *ioat_device_get_next(void)
{
    if (device_next >= device_count) {
        device_next = 0;
    }
    return devices[device_next++];
}

errval_t ioat_device_poll(void)
{
    errval_t err;

    uint8_t idle = 0x1;
    for (uint8_t i = 0; i < device_count; ++i) {
        err = ioat_dma_device_poll_channels((struct dma_device *)devices[i]);
        switch (err_no(err)) {
            case SYS_ERR_OK:
                idle = 0;
                break;
            case DMA_ERR_DEVICE_IDLE:
                break;
            default:
                return err;
        }
    }
    if (idle) {
        return DMA_ERR_DEVICE_IDLE;
    }
    return SYS_ERR_OK;
}

#define TEST_IMPLEMENTATION 1
#if TEST_IMPLEMENTATION
#include <dma/dma_bench.h>


#define BUFFER_SIZE (1<<22)

uint32_t done = 0;

static void impl_test_cb(errval_t err, dma_req_id_t id, void *arg)
{
    debug_printf("impl_test_cb\n");
    assert(memcmp(arg, arg + BUFFER_SIZE, BUFFER_SIZE) == 0);
    debug_printf("test ok\n");

    done = 1;
}

static void impl_test(struct ioat_dma_device *dev)
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
    err = vspace_map_one_frame(&buf, id.bytes, frame, NULL, NULL);
    assert(err_is_ok(err));

    uint64_t address = id.base;
    if (driverkit_iommu_present(NULL)) {
        address = (lpaddr_t)buf;
        debug_printf("Setting id.base to %lx\n", address);
    }

    memset(buf, 0, id.bytes);
    memset(buf, 0xA5, BUFFER_SIZE);



    struct dma_req_setup setup = {
            .args.memcpy = {
                .src = address,
                .dst = address + BUFFER_SIZE,
                .bytes = BUFFER_SIZE,
            },
        .type = DMA_REQ_TYPE_MEMCPY,
        .done_cb = impl_test_cb,
        .cb_arg = buf
    };
    int reps = 10;
    do {
        memset(buf, 0, id.bytes);
        memset(buf, reps + 2, BUFFER_SIZE);
        assert(memcmp(buf, buf + BUFFER_SIZE, BUFFER_SIZE));

        debug_printf("!!!!!! NEW ROUND\n");
        dma_req_id_t rid;
        err = ioat_dma_request_memcpy((struct dma_device *)dev, &setup, &rid);
        assert(err_is_ok(err));

        done = 0;
        while(done == 0) {
            ioat_dma_device_poll_channels((struct dma_device *)dev);
        }
#if 0
        if (reps == 1) {
            debug_printf("using phys addr!\n");
            setup.args.memcpy.src = id.base;
            setup.args.memcpy.dst = id.base + BUFFER_SIZE;
        }
#endif

    } while(reps--);


}
#endif

/**
 * Driver initialization function. This function is called by the driver domain
 * (see also 'create_handler' in ddomain_service.c).
 * Typically through a request from the device manager.
 *
 * The init function is supposed to set `dev` to the exported service iref.
 * The init function may use `bfi->dstate` to store additional state about the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \param[in]   name  The name of this driver instance.
 * \param[in]   flags Additional flags (The exact flags supported is device/driver specific).
 * \param[in]   c     Capabilities (for registers etc.) as provided by the device manager.
 *                    The exact layout of the `c` is device specific.
 * \param[out]  dev   The service iref over which the device can be contacted.
 *
 * \retval SYS_ERR_OK Device initialized successfully.
 * \retval LIB_ERR_MALLOC_FAIL Unable to allocate memory for the driver.
 */
static errval_t init(struct bfdriver_instance *bfi, uint64_t flags, iref_t* dev) {

    errval_t err;
    // 1. Initialize the device:

    debug_printf("[ioat]: attaching device '%s'\n", bfi->name);

    struct ioat_dma_device **devices_new;
    devices_new = realloc(devices, (device_count + 1) * sizeof(void *));
    if (devices_new == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    devices = devices_new;

    struct capref devid;
    err = driverkit_get_devid_cap(bfi, &devid);
    if (err_is_fail(err)) {
        return err;
    }

    struct device_identity id;
    err = invoke_device_identify(devid, &id);
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("[ioat]: '%s' is at %u.%u.%u\n", bfi->name, id.bus, id.device,
                 id.function);

    struct capref regs;
    err = driverkit_get_bar_cap(bfi, 0, &regs);
    if (err_is_fail(err)) {
        return err;
    }

    struct pci_addr pciaddr = {
        .bus      = id.bus,
        .device   = id.device,
        .function = id.function
    };

    debug_printf("IOMMU PRESENT: %u", driverkit_iommu_present(NULL));
    if (driverkit_iommu_present(NULL)) {

        struct vnode_identity vid;
        err = invoke_vnode_identify(cap_vroot, &vid);
        assert(err_is_ok(err));
        debug_printf("[ioat] using ptable root: %lx\n", vid.base);

        err = driverkit_iommu_create_domain(cap_vroot, devid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to create the iommu domain\n");
            return err;
        }

        err = driverkit_iommu_add_device(cap_vroot, devid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to add device to domain\n");
            return err;
        }
    }


    /* initialize the device */
    err = ioat_dma_device_init(regs, &pciaddr, driverkit_iommu_present(NULL),
                               &devices[device_count]);
    if (err_is_fail(err)) {
        DEV_ERR("Could not initialize the device: %s\n", err_getstring(err));
        return err;
    }

    #if TEST_IMPLEMENTATION
    impl_test(devices[device_count]);
    #endif

    device_count++;


    // 2. Export service to talk to the device:

    // 3. Set iref of your exported service (this is reported back to Kaluga)
    *dev = 0x00;

    return SYS_ERR_OK;
}

/**
 * Instructs driver to attach to the device.
 * This function is only called if the driver has previously detached
 * from the device (see also detach).
 *
 * \note After detachment the driver can not assume anything about the
 * configuration of the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t attach(struct bfdriver_instance* bfi) {

    return SYS_ERR_OK;
}

/**
 * Instructs driver to detach from the device.
 * The driver must yield any control over to the device after this function returns.
 * The device may be left in any state.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t detach(struct bfdriver_instance* bfi) {

    return SYS_ERR_OK;
}

/**
 * Instructs the driver to go in a particular sleep state.
 * Supported states are platform/device specific.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {

    return SYS_ERR_OK;
}

/**
 * Destroys this driver instance. The driver will yield any
 * control over the device and free any state allocated.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t destroy(struct bfdriver_instance* bfi) {

    // XXX: Tear-down the service
    bfi->device = 0x0;

    return SYS_ERR_OK;
}



static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{
    return SYS_ERR_OK;
}
/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(ioat_dma_module, init, attach, detach, set_sleep_level, destroy, get_ep);
