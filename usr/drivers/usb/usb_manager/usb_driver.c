/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb/usb.h>
#include <usb/class/usb_hid.h>

#include <usb_device.h>
#include <usb_driver.h>
#include <usb_hub.h>
#include <usb_interface.h>

static struct usb_device *devices_pending = NULL;
static struct usb_device *device_process = NULL;

static char *usb_driver_query(uint8_t class, uint8_t subclass, uint8_t protocol,
        uint16_t vendor, uint16_t product)
{
    char *path = NULL;
    switch (class) {
        case USB_DEVICE_CLASS_COMPOSITE:
            USB_DEBUG_DRIVER("Device is a composite device\n");
            break;
        case USB_DEVICE_CLASS_AUDIO:
            USB_DEBUG_DRIVER("Device is a audio device\n");
            break;
        case USB_DEVICE_CLASS_COMM:
            USB_DEBUG_DRIVER("Device is a communication device\n");
            break;
        case USB_DEVICE_CLASS_HID:
            if (subclass == USB_HID_SUBCLASS_CODE_BOOT) {
                /* boot interface class */
            }

            switch (protocol) {
                case USB_HID_PROTOCOL_NONE:
                    USB_DEBUG_DRIVER("HID device with no ptorocol\n");
                    break;
                case USB_HID_PROTOCOL_KEYBOARD:
                    USB_DEBUG_DRIVER("HID device: Keyboard\n");
                    path = "/armv7/sbin/usb_keyboard";
                    break;
                case USB_HID_PROTOCOL_MOUSE:
                    USB_DEBUG_DRIVER("HID device: Mouse\n");
                    break;
                default:
                    USB_DEBUG_DRIVER("HID device with reserved protocol\n");
                    break;
            }
            break;
        case USB_DEVICE_CLASS_PHYSICAL:
            USB_DEBUG_DRIVER("Device is a physical device\n");
            break;
        case USB_DEVICE_CLASS_IMAGE:
            USB_DEBUG_DRIVER("Device is a imaging device\n");
            break;
        case USB_DEVICE_CLASS_PRINTER:
            USB_DEBUG_DRIVER("Device is a printing device\n");
            break;
        case USB_DEVICE_CLASS_MSD:
            USB_DEBUG_DRIVER("Device is a mass storage device\n");
            break;
        case USB_DEVICE_CLASS_HUB:
            USB_DEBUG_DRIVER("Device is a hub device\n");
            break;
        case USB_DEVICE_CLASS_CDC:
            USB_DEBUG_DRIVER("Device is a cdc device\n");
            break;
        case USB_DEVICE_CLASS_SMARTCARD:
            USB_DEBUG_DRIVER("Device is a smartcard device\n");
            break;
        case USB_DEVICE_CLASS_SECURITY:
            USB_DEBUG_DRIVER("Device is a security device\n");
            break;
        case USB_DEVICE_CLASS_VIDEO:
            USB_DEBUG_DRIVER("Device is a video device\n");
            break;
        case USB_DEVICE_CLASS_HEALTH:
            USB_DEBUG_DRIVER("Device is a health device\n");
            break;
        case USB_DEVICE_CLASS_AV:
            USB_DEBUG_DRIVER("Device is a audio/video device\n");
            break;
        case USB_DEVICE_CLASS_DIAG:
            USB_DEBUG_DRIVER("Device is a diag device\n");
            break;
        case USB_DEVICE_CLASS_WIFI:
            USB_DEBUG_DRIVER("Device is a wifi device\n");
            break;
        case USB_DEVICE_CLASS_MISC:
            USB_DEBUG_DRIVER("Device is a misc device\n");
            break;
        case USB_DEVICE_CLASS_APPL:
            USB_DEBUG_DRIVER("Device is a application specific device\n");
            break;
        case USB_DEVICE_CLASS_VENDOR:
            USB_DEBUG_DRIVER("Device is a vendor specific device\n");
            break;
        default:
            USB_DEBUG("WARNING: Unknown device class!");
            return (NULL);
    }

    if (path != NULL) {

        char *ret_path = malloc(strlen(path));
        memcpy(ret_path, path, strlen(path));
        return (ret_path);
    }
    return (NULL);
}

static char *usb_driver_lookup_iface(struct usb_device *dev)
{
    USB_DEBUG_TR_ENTER;

    struct usb_interface *iface = dev->ifaces;
    struct usb_interface_descriptor *idesc;

    char *path = NULL;
    USB_DEBUG_DRIVER("looking up device class from interface\n");
    USB_DEBUG_DRIVER(">> Parsing %u interfaces\n", dev->iface_max);

    uint32_t i = 0;
    while (iface != NULL && i < dev->iface_max) {
        idesc = iface->descriptor;
        if (idesc == NULL) {
            USB_DEBUG("IFACE == NULLL");
        }
        path = usb_driver_query(idesc->bInterfaceClass,
                idesc->bInterfaceSubClass, idesc->bInterfaceProtocol,
                dev->device_desc.idVendor, dev->device_desc.idProduct);
        if (path != NULL) {
            break;
        }
        iface++;
        i++;
    }

    return (path);
}

static char *usb_driver_lookup_comm(struct usb_device *dev)
{
    return usb_driver_query(dev->device_desc.bDeviceClass, 0, 0,
            dev->device_desc.idVendor, dev->device_desc.idProduct);
}

static char *usb_driver_lookup_diag(struct usb_device *dev)
{
    return (NULL);
}

static char *usb_driver_lookup_misc(struct usb_device *dev)
{
    return (NULL);
}

static char *usb_driver_lookup_vendor(struct usb_device *dev)
{
    return (usb_driver_lookup_iface(dev));
}

static char *usb_driver_lookup(struct usb_device *dev)
{
    USB_DEBUG_TR_ENTER;

    switch (dev->device_desc.bDeviceClass) {
        case USB_DEVICE_CLASS_HUB:
            USB_DEBUG("tried to start external hub driver...\n");
            return (NULL);

        case USB_DEVICE_CLASS_COMM:
            USB_DEBUG("communication device found...\n");
            return (usb_driver_lookup_comm(dev));
            break;
        case USB_DEVICE_CLASS_DIAG:
            USB_DEBUG("diag device found...\n");
            return (usb_driver_lookup_diag(dev));
            break;
        case USB_DEVICE_CLASS_MISC:
            USB_DEBUG("misc device found...\n");
            return (usb_driver_lookup_misc(dev));
            break;
        case USB_DEVICE_CLASS_VENDOR:
            USB_DEBUG("vendor specific device found...\n");
            return (usb_driver_lookup_vendor(dev));
            break;
    }

    /*
     * the device has to be identified via the interface
     * descriptor...
     */
    return (usb_driver_lookup_iface(dev));
}

static void usb_driver_spawn(void)
{
    USB_DEBUG_TR_ENTER;

    /*
     * can only handle one device spawn at a time
     */
    if (device_process != NULL) {
        return;
    }

    /*
     * there are no devices left
     */
    if (devices_pending == NULL) {
        return;
    }

    /* get the next device */
    device_process = devices_pending;
    devices_pending = device_process->next_pending;

    assert(device_process != NULL);
    assert(device_process->path != NULL);

    coreid_t core = 0;

    domainid_t new_domain = -1;

    char *argv[1] = {
        [0] = NULL, //"armv7/sbin/usb_keyboard",//"device_process->path,
    };

    errval_t err = spawn_program(0, device_process->path, argv, NULL, 0,
            &new_domain);

    if (err_is_fail(err)) {
        DEBUG_ERR(err,
                "failed to spawn %s on core %i", device_process->path, core);
        device_process = NULL;
    } else {
        USB_DEBUG_DRIVER("driver %s spawned\n", device_process->path);
    }
}

static void usb_driver_insert_pending(struct usb_device *new_device)
{
    new_device->next_pending = devices_pending;
    devices_pending = new_device;
}

void usb_driver_connected(struct usb_manager_binding *bind, uint16_t config)
{
    USB_DEBUG_DRIVER("Finalizing driver binding\n");
    usb_error_t err = USB_ERR_OK;;
    assert(device_process != NULL);

    if (device_process->config_number != config) {
        USB_DEBUG_DRIVER("Updating configuration to %u\n", config);
        err = usb_device_set_configuration(device_process,config);
        if (err != USB_ERR_OK) {
            bind->st = NULL;
            return;
        }

    }

    device_process->usb_manager_binding = bind;
    bind->st = device_process;


    /* set the current device to be processed */
    device_process = NULL;

    /* spawn possible next driver */
    usb_driver_spawn();
}

void usb_driver_start(struct usb_device *dev)
{
    USB_DEBUG_TR_ENTER;

    usb_error_t err;

    if (dev->device_desc.bDeviceClass == USB_HUB_CLASS_CODE
            && dev->device_desc.bDeviceSubClass == USB_HUB_SUBCLASS_CODE) {
        USB_DEBUG_DRIVER("New Hub device. Starting internal driver.\n");
        err = usb_hub_init(dev);
        if (err != USB_ERR_OK) {
            USB_DEBUG("ERROR: Could not initialize the hub device!");
        }
        return;
    }

    char *path = usb_driver_lookup(dev);

    if (path == NULL) {
        USB_DEBUG("WARNING: no suitable usb device driver found!\n");
        return;
    }

    dev->path = path;

    // insert the device into the pending lists...
    usb_driver_insert_pending(dev);

    usb_driver_spawn();
}

