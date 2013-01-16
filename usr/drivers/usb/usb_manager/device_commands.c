/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * device_commands.c
 *
 * Contains definition of device request constructors
 * See section 9.4 USB 2.0 documentation for more details
 */

#include <usb/device_commands.h>

/*
 * \brief Constructor for reading device descriptor request
 */
usb_device_request get_usb_device_descriptor_request(void)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_DEVICE_TO_HOST | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b10000000

    // Sanity check
    assert(usb_req.bmRequestType == 0x80);

    usb_req.bRequest = USB_GET_DESCRIPTOR;
    usb_req.wValue = USB_DEVICE;        // Device type

    // String zero for device descriptors
    usb_req.wValue = (usb_req.wValue << 8);
    usb_req.wIndex = 0;
    usb_req.wLength = sizeof (usb_device_descriptor);

    return usb_req;
}

/*
 * \brief Constructor for fetching descriptor strings from the device
 *
 * \param index Index of the string in the string descriptor
 */

usb_device_request get_usb_device_string_desc_request(uint8_t index)
{


    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_DEVICE_TO_HOST | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b10000000

    // Sanity check
    assert(usb_req.bmRequestType == 0x80);

    usb_req.bRequest = USB_GET_DESCRIPTOR;
    usb_req.wValue = USB_STRING;        // Descriptor type
    // manfactuer ...product....serial number as indexed as number

    usb_req.wValue = (usb_req.wValue << 8) | (index);   // String zero

    //FIXME: Language id, 0x409 is for English !!
    //Not so important ...fix it in case we are trying to support
    //non-English symbols. Id found by doing experiments.
    // We should break this as 2 step process
    // Step 1. Get whole descriptor and read language id
    // Step 2. Fetch the strings using that id
    usb_req.wIndex = 0x409;

    usb_req.wLength = sizeof (usb_string_descriptor);

    return usb_req;
}


/*
 * \brief Constructs request to fetch device configuration descriptor
 * \param index Index of configuration to fetch
 */

usb_device_request get_usb_device_configuration_descriptor_request(uint8_t
                                                                   index)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_DEVICE_TO_HOST | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b10000000

    // Sanity check
    assert(usb_req.bmRequestType == 0x80);

    usb_req.bRequest = USB_GET_DESCRIPTOR;
    usb_req.wValue = USB_CONFIGURATION; // Descriptor type

    usb_req.wValue = (usb_req.wValue << 8) | (index);

    usb_req.wIndex = 0x0;

    usb_req.wLength = sizeof (usb_configuration_descriptor);

    return usb_req;
}


/*
 * \brief Request to set non-zero device address of enumeration process
 * \param address New non-zero device address
 */

usb_device_request get_usb_device_set_address_request(uint8_t address)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_HOST_TO_DEVICE | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b00000000

    // Sanity check
    assert(usb_req.bmRequestType == 0);

    usb_req.bRequest = USB_SET_ADDRESS;
    usb_req.wValue = address;   // address to be set

    usb_req.wIndex = 0x0;

    usb_req.wLength = 0x0;      // NO data stage here

    return usb_req;
}


/*
 * \brief Request constructor to fetch current enabled device config
 */

usb_device_request get_usb_device_current_configuration_request(void)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_DEVICE_TO_HOST | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b00000000

    // Sanity check
    assert(usb_req.bmRequestType == 0x80);

    usb_req.bRequest = USB_GET_CONFIGURATION;
    usb_req.wValue = 0x0;       // address to be set

    usb_req.wIndex = 0x0;

    usb_req.wLength = 0x1;      // 1 byte data stage here

    return usb_req;
}

/*
 * \brief Set a particular configuration to the device
 * \param config_idx Index of configuration which has to enable
 */

usb_device_request get_usb_device_set_config_request(uint8_t config_idx)
{
    // XXX: This may seems confusing but both requests have
    // simillar kind of struct and their parameters are
    // inter-changebale.
    return get_usb_device_set_address_request(config_idx);
}

/*
 * \brief Constructs a request to fetch endpoint status
 * \param ep Endpoint of interest
 */

usb_device_request get_usb_device_ep_status_request(uint8_t ep)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_DEVICE_TO_HOST | USB_REQTYPE_STANDARD | USB_RECP_ENDPOINT;  // Eval = 0b10000010

    // Sanity check
    assert(usb_req.bmRequestType == 0x82);

    usb_req.bRequest = USB_GET_STATUS;

    usb_req.wValue = 0x0;       // zero

    // wIndex takes whole ep address
    // as showed in figure 9.2, including direction
    usb_req.wIndex = ep;

    usb_req.wLength = 2;        // 2 byte data stage here

    return usb_req;
}

/*
 * \brief Request to clear halt on an endpoint
 * \param ep Endpoint to be cleared
 */

usb_device_request get_usb_device_clear_halt_ep_request(uint8_t ep)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_HOST_TO_DEVICE | USB_REQTYPE_STANDARD | USB_RECP_ENDPOINT;  // Eval = 0b00000010

    // Sanity check
    assert(usb_req.bmRequestType == 2);

    usb_req.bRequest = USB_CLEAR_FEATURE;

    usb_req.wValue = USB_ENDPOINT_HALT; // zero

    // Figure 9.2
    usb_req.wIndex = ep;

    usb_req.wLength = 0;        // No data transfer

    return usb_req;
}

/*
 * \brief Some devices support explicit software enabled endpoint
 *        halt settings. This constructor gnerates that request
 *
 * \param ep Endpoint of interest
 */

usb_device_request get_usb_device_set_halt_ep_request(uint8_t ep)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_HOST_TO_DEVICE | USB_REQTYPE_STANDARD | USB_RECP_ENDPOINT;  // Eval = 0b00000010

    // Sanity check
    assert(usb_req.bmRequestType == 2);

    usb_req.bRequest = USB_SET_FEATURE;

    usb_req.wValue = USB_ENDPOINT_HALT;

    // Use only lower byte of word when specifying the endpoint
    // as told on page 249, USB 2.0 specification (9.3.4)
    // & fig 9.2
    usb_req.wIndex = ep;

    usb_req.wLength = 0;        // No data transfer

    return usb_req;
}

/*
 * \brief Some devices support remote wakeup after the USB bus is suspended.
 *        This is to try that.
 */

usb_device_request get_usb_device_set_rwakeup_request(void)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_HOST_TO_DEVICE | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b00000000

    // Sanity check
    assert(usb_req.bmRequestType == 0);

    usb_req.bRequest = USB_SET_FEATURE;

    usb_req.wValue = USB_DEVICE_REMOTE_WAKEUP;

    usb_req.wIndex = 0x0;

    usb_req.wLength = 0;        // No data transfer

    return usb_req;
}

/*
 * \brief Constructor to clear wakeup flag on the device
 */

usb_device_request get_usb_device_clear_rwakeup_request(void)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_HOST_TO_DEVICE | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b00000000

    // Sanity check
    assert(usb_req.bmRequestType == 0);

    usb_req.bRequest = USB_CLEAR_FEATURE;

    usb_req.wValue = USB_DEVICE_REMOTE_WAKEUP;

    usb_req.wIndex = 0x0;

    usb_req.wLength = 0;        // No data transfer

    return usb_req;
}

/*
 * \breif Constructor to fetch device status
 */

usb_device_request get_usb_device_status_request(void)
{
    struct usb_device_request usb_req;

    usb_req.bmRequestType = USB_DIR_DEVICE_TO_HOST | USB_REQTYPE_STANDARD | USB_RECP_DEVICE;    // Eval = 0b10000010

    // Sanity check
    assert(usb_req.bmRequestType == 0x80);

    usb_req.bRequest = USB_GET_STATUS;

    usb_req.wValue = 0x0;       // zero

    usb_req.wIndex = 0x0;

    usb_req.wLength = 2;        // 2 byte data stage here

    return usb_req;
}
