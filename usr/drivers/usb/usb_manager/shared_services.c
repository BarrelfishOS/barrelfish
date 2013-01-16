/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <usb/shared_state.h>
#include <usb/usb_device.h>
#include <usb/mem/usb_mem.h>

#include "shared_services.h"
#include "ehci_client.h"

usb_shared_state_t *dev_toggle_arr = NULL;

void dev_toggle_reset(uint8_t dev)
{
    //Reset all I/O toggles 
    dev_toggle_arr[dev].toggle[0] = 0;
    dev_toggle_arr[dev].toggle[1] = 0;
}

static void init_shared_arr(void)
{
    printf("USBD: %s \n", __func__);
    int i;
    for (i = 0; i < N_USB_DEVICES; i++) {
        dev_toggle_arr[i].status = DISCONN_RH;
        dev_toggle_reset(i);
    }
    // Default device 0 is always connected
    // Zero does not have any bulk end points  
    dev_toggle_arr[0].status = CONN_RH;
}

void set_dev_status(uint8_t add, uint8_t status)
{
    if (!dev_toggle_arr)
        return;

    dev_toggle_arr[add].status = status;
}

uint8_t get_dev_status(uint8_t dev)
{
    // Before init DISCONN_RH would make sense
    if (!dev_toggle_arr)
        return DISCONN_RH;

    return dev_toggle_arr[dev].status;
}

static void init_shared_state(struct capref cap, uint32_t sz)
{
    printf("USBD: %s \n", __func__);
    map_dev_page(cap, sz);
}

void map_init(void)
{
    printf("USBD: %s \n", __func__);
    static bool done = false;
    if (!done) {
        assert(dev_toggle_arr == NULL);
        uint32_t sz = sizeof (usb_shared_state_t) * N_USB_DEVICES;
        usb_mem mem = malloc_iobuff(sz, USB_NEAR_EHCI);
        dev_toggle_arr = mem.va;
        init_shared_arr();
        init_shared_state(mem.cap, mem.size);
        done = true;
    }
}
