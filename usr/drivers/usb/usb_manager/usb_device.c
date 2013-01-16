/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stdlib.h>

#include <usb/utility.h>

#include <usb/usb_device.h>
#include <usb/usb_services.h>
#include <usb/usb_topology.h>

//#define USB_LOCL_DEBUG
#include<usb/usb_debug.h>
#include "shared_services.h"

/*
 * Verifies if a given byte is a valid request and does not 
 * contain any reserved fields
 */

int verify_usb_request(uint8_t req)
{
    return 1;
    // Because right now all requests are made from internal functions
    // but perhaps needed to verify requests from the client drivers
    // XXX: Most of the client driver requests are protocol specific 
    // which USB can not interpret. So better leave it to client 
    // drivers. Return error code if expected result is not found.  
}

/*
 * \brief A typical dump printing function for the USB device descriptor
 *
 * \param udd USB device descriptor to be printed
 */

void print_usb_device_descriptor(usb_device_descriptor udd)
{
    printf("\n -----USB DEVICE DESCRIPTOR DUMP (in HEX) --------\n");
    printf("\n bLenght : %x", udd.bLength);
    printf("\n bDescriptor type : %x", udd.bDescriptorType);
    printf("\n bcdUSB : %x", udd.bcdUSB);
    printf("\n bDeviceClass : %x", udd.bDeviceClass);
    printf("\n bDeviceSubClass : %x", udd.bDeviceSubClass);
    printf("\n bDeviceProtocol : %x", udd.bDeviceProtocol);
    printf("\n bMaxPacketSize0 : %x", udd.bMaxPacketSize0);
    printf("\n idVendor : %x", udd.idVendor);
    printf("\n idProduct : %x", udd.idProduct);
    printf("\n bcdDevice : %x", udd.bcdDevice);
    printf("\n iManufacturer : %x", udd.iManufacturer);
    printf("\n iProduct : %x", udd.iProduct);
    printf("\n iSerial Number : %x", udd.iSerialNumber);
    printf("\n bNumConfigurations : %x", udd.bNumConfigurations);
    printf("\n --------- END OF USB DEVICE DESCRIPTOR DUMP ------\n\n");

}

/*
 * \brief A typical dump printing function for the USB 
 *        configuration descriptor.
 *
 * \param ucd USB configuration descriptor to be printed
 */
void print_usb_configuration_descriptor(usb_configuration_descriptor ucd)
{
    printf("\n\n\t ----USB CONFIGURATION DESCRIPTOR DUMP (in HEX)-------\n");
    printf("\n\t bLenght : %x", ucd.bLength);
    printf("\n\t bDescriptor type : %x", ucd.bDescriptorType);
    printf("\n\t wTotalLength : %x", ucd.wTotalLength);
    printf("\n\t bNumInterfaces : %x", ucd.bNumInterfaces);
    printf("\n\t bConfigurationValue : %x", ucd.bConfigurationValue);
    printf("\n\t iConfiguration : %x", ucd.iConfiguration);
    printf("\n\t bmAttributes : %x", ucd.bmAttributes);
    printf("\n\t bMaxPower : %x", ucd.bMaxPower);
    printf("\n\t ----- END OF USB CONFIGURATION DESCRIPTOR DUMP ------\n\n");
}

/*
 * \brief A typical dump printing function for the USB 
 *        interface descriptor.
 *
 * \param uid USB interface descriptor to be printed
 */
void print_usb_interface_descriptor(usb_interface_descriptor uid)
{
    printf("\n\n\t\t ---USB INTERFACE DESCRIPTOR DUMP (in HEX) ----------");
    printf("\n\t\t bLenght : %x", uid.bLength);
    printf("\n\t\t bDescriptor type : %x", uid.bDescriptorType);
    printf("\n\t\t bInterfaceNumber : %x", uid.bInterfaceNumber);
    printf("\n\t\t bAlternateSetting : %x", uid.bAlternateSetting);
    printf("\n\t\t bNumEndpoints : %x", uid.bNumEndpoints);
    printf("\n\t\t bInterfaceClass : %x", uid.bInterfaceClass);
    printf("\n\t\t binterfaceSubClass : %x", uid.bInterfaceSubClass);
    printf("\n\t\t bInterfaceProtocol : %x", uid.bInterfaceProtocol);
    printf("\n\t\t iInterface : %x", uid.iInterface);
    printf("\n\t\t ---- END OF USB INTERFACE DESCRIPTOR DUMP------\n\n");

}

/*
 * \brief A typical dump printing function for the USB 
 *        endpoint descriptor.
 *
 * \param upd USB endpoint descriptor to be printed
 */
void print_usb_ep_descriptor(usb_endpoint_descriptor upd)
{

    printf("\n\n\t\t\t ----USB ENDPOINT DESCRIPTOR DUMP (in HEX) ------");
    printf("\n\t\t\t bLenght : %x", upd.bLength);
    printf("\n\t\t\t bDescriptor type : %x", upd.bDescriptorType);
    printf("\n\t\t\t bEndpointAddress : %x", upd.bEndpointAddress);
    printf("\n\t\t\t bmAttributes : %x", upd.bmAttributes);
    printf("\n\t\t\t wMaxPacketSize : %x", upd.wMaxPacketSize);
    printf("\n\t\t\t bInterval : %x", upd.bInterval);
    printf("\n\t\t\t  ---- END OF USB ENDPOINT DESCRIPTOR DUMP -----\n\n");

}

/*
 * \brief A typical dump printing function for the USB 
 *        interface descriptor.
 *
 * \param udd USB device request to be printed
 */
void print_usb_device_request(usb_device_request udd)
{
    printf("\n\n ---------USB DEVICE REQUST DUMP (in HEX) -----------");
    printf("\n bmRequestTyep : %x", udd.bmRequestType);
    printf("\n bRequest : %x", udd.bRequest);
    printf("\n wValue : %x", udd.wValue);
    printf("\n wIndex : %x", udd.wIndex);
    printf("\n wLength : %x", udd.wLength);
    printf("\n --------- END OF USB DEVICE REQUEST DUMP -----------\n\n");

}

/*
 * Internal functions to help in recursive printing 
 * of device dump 
 */

static void ep_print(usb_interface_t intf)
{
    int i;
    if (intf.ep == NULL)
        return;
    for (i = 0; i < intf.intf.bNumEndpoints; i++) {
        print_usb_ep_descriptor(intf.ep[i].ep);
    }

}

static void intf_print(usb_config_t config)
{
    int i;
    if (config.intf == NULL)
        return;

    for (i = 0; i < config.config.bNumInterfaces; i++) {
        print_usb_interface_descriptor(config.intf[i].intf);
        ep_print(config.intf[i]);
    }
}

void print_usb_device_info(usb_device_t dev)
{
    int i = 0;
    printf("\n\n ****** USB DEVICE INFORMATION [IN HEX]*************\n");
    printf("\n Attached at port : %x", dev.port);
    printf("\n USB Address index: %x", dev.address);
    printf("\n Number of config : %x", dev.no_config);
    printf("\n Device State : %x", dev.device_state);
    if (dev.device_manufacturer != NULL) {
        printf("\n Device Manufacturer : ");
        print_str(dev.device_manufacturer, dev.man_sz);
    } else
        printf("\n Device Manufacturer : NULL");


    if (dev.device_name != NULL) {
        printf("\n Device Name : ");
        print_str(dev.device_name, dev.dev_sz);
    } else
        printf("\n Device Name : NULL");

    if (dev.serial_number != NULL) {
        printf("\n Device Serial Number : ");
        print_str(dev.serial_number, dev.sr_sz);
    } else
        printf("\n Device Serial Number : NULL");

    print_usb_device_descriptor(dev.desc);


    if (dev.config != NULL) {
        for (i = 0; i < dev.desc.bNumConfigurations; i++) {
            print_usb_configuration_descriptor(dev.config[i].config);
            intf_print(dev.config[i]);
        }

    }

    printf("\n ************* END OF USB DEVICE INFORMATION ***********\n\n");

}

/*
 * Internal functions to help release resources when a device is removed 
 * from the USB system
 */

static void ep_release(usb_interface_t intf)
{
    if (intf.ep)
        free(intf.ep);
}

static void intf_release(usb_config_t config)
{
    int i;
    if (config.intf == NULL)
        return;

    for (i = 0; i < config.config.bNumInterfaces; i++)
        ep_release(config.intf[i]);

    free(config.intf);

}

void release_resources(usb_device_t dev)
{
    int i = 0;
    printf("\n\n ********** RELEASING RESOURCES for dev****************");
    printf("\n Attached at port : %x", dev.port);
    printf("\n USB Address index: %x", dev.address);
    printf("\n Number of config : %x", dev.no_config);
    printf("\n Device State : %x", dev.device_state);
    if (dev.device_manufacturer != NULL)
        free(dev.device_manufacturer);
    if (dev.device_name != NULL)
        free(dev.device_name);
    if (dev.serial_number != NULL)
        free(dev.serial_number);
    if (dev.config != NULL) {
        for (i = 0; i < dev.desc.bNumConfigurations; i++)
            intf_release(dev.config[i]);
    }

    free(dev.config);
    printf("\n ************* Dev resources free ***********\n\n");
}

void release_address(uint8_t add)
{
    usb_device_arr[add].free = 1;

}

/*
 * \brief This function is responsible for allocating non-zero USB address to 
 *        the newly enumerated device. It walks through the device array and 
 *        retruns the first free slot. 
 */

uint8_t allocate_new_usb_address(void)
{
    uint8_t i;
    dprintf("%s: Searching for free slot in USB array"
            "to assign location/address\n", __func__);
    for (i = 0; i < N_USB_DEVICES; i++) {
        if (usb_device_arr[i].free == 1) {
            dprintf("USB_DEV: Free slot found at [%d]\n", i);
            usb_device_arr[i].free = 0; // Mark it as used
            // connect device in shared tree 
            set_dev_status(i, CONN_RH);
            insert_node(NULL, &usb_device_arr[i]);
            // Return the free slot, 
            // and device address will be used 
            // to index into it 
            return i;
        }
    }
    dprintf("Maximum device limit reached ...not enumerating the device\n");
    dprintf("Try removing some other devices and then try again\n");
    return 0;
}


/*
 * \breif This is primary init logic for internal data struct of USB manager. 
 *        Upon calling, it frees all slots in dev array and call to init the 
 *        tree topology. 
 */

void usb_device_init(void)
{
    dprintf("%s\n", __func__);
    static bool init = false;
    if (!init) {
        int i;
        // Initialize the USB device array 
        for (i = 0; i < N_USB_DEVICES; i++) {
            usb_device_arr[i].free = 1;
        }

        usb_device_arr[0].free = 0;     // Address zero is reserved.
        init_usb_tree();
        init = true;
    }

}

/*
 * \brief Checks if a particular address is valid to access. 
 */
static inline bool check_validity(uint8_t dev)
{
    if (usb_device_arr[dev].free == 1 ||        // Invalid address
        dev == 0 ||             // default address
        get_dev_status(dev) == DISCONN_RH)      // device disconnected 
        return false;
    else
        return true;
}

/*
 * \brief Returns the current enabled config on the USB device
 *
 * \param dev USB device to be probed 
 * \param config Contains the configuration descriptor value
 */

int get_curr_config(uint8_t dev, usb_configuration_descriptor * config)
{
    if (!check_validity(dev)) {
        config = NULL;
        return -1;
    }
    int curr_config = usb_device_arr[dev].curr_config;

    *(config) = usb_device_arr[dev].config[curr_config].config;
    return 0;
}


/*
 * \brief Returns the number of configs available on a particular device 
 *
 * \param dev USB device to be probed 
 * \param num Number of configs 
 */

void get_num_config(uint8_t dev, uint8_t * num)
{
    if (!check_validity(dev)) {
        *num = 0;
        return;
    }

    *num = usb_device_arr[dev].desc.bNumConfigurations;
}

/*
 * \brief Fetches a particular config from the device struct 
 *
 * \param dev USB dev to be probed 
 * \param num Index of the config to be fetched 
 * \param config Value of the config descriptor
 */

int get_config(uint8_t dev, uint8_t num, usb_configuration_descriptor * config)
{
    if (!check_validity(dev)) {
        config = NULL;
        return -1;
    }
    // Invalid config index
    if (num > usb_device_arr[dev].desc.bNumConfigurations) {
        config = NULL;
        return -1;
    }

    *config = usb_device_arr[dev].config[num].config;
    return 0;
}

/*
 * \brief Enables a particualr config on the device 
 *        Not yet implemented, because USB flash driver just has one
 */

int set_config(uint8_t dev, uint8_t num)
{
    assert(!"NYI: Set config on an USB device");
    // Set config request 
    return -1;
}

/*
 * \brief Returns the current enabled interface on the USB device
 *
 * \param dev USB device to be probed 
 * \param config Contains the interface descriptor value
 */

int get_curr_intf(uint8_t dev, usb_interface_descriptor * intf)
{
    if (!check_validity(dev)) {
        intf = NULL;
        return -1;
    }

    int curr_config = usb_device_arr[dev].curr_config;
    int curr_intf = usb_device_arr[dev].config[curr_config].curr_intf;


    *intf = usb_device_arr[dev].config[curr_config].intf[curr_intf].intf;
    return 0;
}

/*
 * \brief Returns the number of interfaces available on a particular device 
 *
 * \param dev USB device to be probed 
 * \param num Number of interfaces 
 */


void get_num_intf(uint8_t dev, uint8_t * num)
{
    if (!check_validity(dev)) {
        *num = 0;
        return;
    }

    int curr_config = usb_device_arr[dev].curr_config;

    *num = usb_device_arr[dev].config[curr_config].no_intf;
}

/*
 * \brief Fetches a particular interface from the device struct 
 *
 * \param dev USB dev to be probed 
 * \param num Index of the interface to be fetched 
 * \param config Value of the interface descriptor
 */


int get_intf(uint8_t dev, uint8_t num, usb_interface_descriptor * intf)
{
    if (!check_validity(dev)) {
        intf = NULL;
        return -1;
    }

    int curr_config = usb_device_arr[dev].curr_config;

    // Invalid index 
    if (num > usb_device_arr[dev].config[curr_config].no_intf) {
        intf = NULL;
        return -1;
    }

    *intf = usb_device_arr[dev].config[curr_config].intf[num].intf;
    return 0;
}

/*
 * \brief Enables a particualr interface on the device 
 *        Not yet implemented, because USB flash driver just has one
 */

int set_intf(uint8_t dev, uint8_t num)
{
    assert(!"NYI: Set interface on an USB device");
    // Set interface request 
    return -1;
}
