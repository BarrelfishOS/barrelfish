/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <usb/utility.h>

#include <usb/device_commands.h>
#include <usb/usb_topology.h>
#include <usb/usbd.h>
#include <usb/mem/usb_mem.h>


#include "ehci_client.h"
#include "shared_services.h"
#include "driver_managment.h"

//#define USB_LOCAL_DEBUG
#include <usb/usb_debug.h>
#define ARR_SZ 100

static struct thread_sem dev_sem = THREAD_SEM_INITIALIZER;
static struct thread *device_enum_t = NULL;
static int port = -1;


/*
 * \brief Copies valid number of bytes from source to destination. It 
 *        is reuired because for few descriptors, USB does not know how 
 *        many number of bytes to expect. They are reported in descriptor
 *        itself. So it checkes, which one is smaller (data from descriptor
 *        or from expected bytes) it copies that. 
 * \param src Address containg the descriptor 
 * \param dest Address of the destinaiton buffer
 * \param exp_sz Number of bytes expected. 
 */


static uint32_t copy_usb_message(void *src, void *dest, uint32_t exp_sz)
{
    uint8_t byte1 = *((uint8_t *) src);
    uint32_t sz;
    if (exp_sz <= byte1)
        sz = exp_sz;
    else
        sz = byte1;

    copy_data(src, dest, sz);
    return sz;
}

/*
 * \brief Recursivelty parse endpoint information from the interface 
 *        descriptor and copy them to internal interface wrapper struct.
 * \param intf internal USB interface wrapper  
 * \param buff Where the data is 
 * \param num_ep Number of endpoints expected
 */

static void set_ep_descriptor(usb_interface_t * intf,
                              uint8_t * buff, uint8_t num_ep)
{
    uint8_t num;
    usb_endpoint_descriptor epd;

    dprintf("Number of end points found = %d\n", num_ep);
    intf->no_ep = num_ep;
    intf->ep = (usb_endpoint_t *) malloc(sizeof (usb_endpoint_t) * num_ep);

    for (num = 0; num < num_ep; num++) {
        assert(*(buff + 1) == USB_ENDPOINT);
        assert(*buff == sizeof (usb_endpoint_descriptor));

        copy_data(buff, &epd, *buff);   // copy
        intf->ep[num].ep = epd; // assign 
        intf->ep[num].pipe_allocated = 0;       // Free to use 
        buff = buff + (*(buff));        // update to next ep
        //print_usb_ep_descriptor(epd);
    }

}

/*
 * \brief Recursivelty parse interface information from the configuration 
 *        descriptor and copy them to internal configuration wrapper struct.
 * \param config Internal USB configuration wrapper  
 * \param buff Where the data is 
 * \param sz Length of the total config descriptor 
 * \param num_ep Number of interfaces expected
 */

static void set_interface_descriptor(usb_config_t * config,
                                     uint8_t * buff, int sz, uint8_t num_intf)
{
    usb_interface_descriptor uid;
    uint8_t *ptr = buff;
    uint8_t i, offset;

    dprintf("Descriptor type = %u\n", *(ptr + 1));

    dprintf("Interface descriptor size = %u\n ", *ptr);
    dprintf("Copying the interface descriptor ....\n");


    config->intf =
        (usb_interface_t *) malloc(sizeof (usb_interface_t) * num_intf);

    for (i = 0; i < num_intf; i++) {
        assert(*(ptr + 1) == USB_INTERFACE);

        assert(*ptr == sizeof (usb_interface_descriptor));

        // copy interface desc 
        copy_data((void *)ptr, (void *)&uid, *ptr);
        //print_usb_interface_descriptor(uid);

        config->intf[i].intf = uid;
        config->intf[i].no_ep = uid.bNumEndpoints;

        offset = sizeof (usb_endpoint_descriptor) * uid.bNumEndpoints
            + sizeof (usb_interface_descriptor);

        set_ep_descriptor(&(config->intf[i]), ptr + (*(ptr))
                          /*start of ep desc */
                          , uid.bNumEndpoints);

        ptr = ptr + offset;
    }
    // FIXME: assumption 
    // Assuming that interface zero is set by defualt with config 
    config->curr_intf = 0;
}

/*
 * \brief The function reads the configuration descriptor from the USB device.
 *        When the request is completed successfully, then it reads the whole 
 *        block and parse the interface & endpoint descriptors as well.
 * \param max_num Number of configurations available on the device
 * \param device USB device to be probed for the requests
 * \param dev Interal USB dev struct where all the information is saved
 */


static void fill_configuration_descriptors(int max_num, uint8_t device,
                                           usb_device_t * dev)
{
    int i, r;
    struct usb_device_request req;
    struct usb_configuration_descriptor resp;
    uint8_t *buff;
    usb_mem data_mem = malloc_64();

    dprintf("Number of configuration found are [%d]\n", max_num);
    dev->config = (usb_config_t *) malloc(sizeof (usb_config_t) * max_num);
    dev->no_config = max_num;


    for (i = 0; i < max_num; i++) {
        req = get_usb_device_configuration_descriptor_request(i);
        r = usb_dctrl_exe(req, data_mem.pa,
                          sizeof (struct usb_configuration_descriptor),
                          device, EHCI_NO_DEBUG);
        if (r > 0) {
            // Configuration is read. 
            dprintf("The CONFIGURATION descriptor read"
                    "sucessfully dumping data ...\n");
            copy_usb_message(data_mem.va, (void *)&resp, req.wLength);

            //print_usb_configuration_descriptor(resp);
            dev->config[i].config = resp;
            dev->config[i].no_intf = resp.bNumInterfaces;

            // Now trying to read config + interface + ep. 
            buff = (uint8_t *) malloc(resp.wTotalLength);

            // Modify the request to read whole block 
            req.wLength = resp.wTotalLength;
            r = usb_dctrl_exe(req, data_mem.pa,
                              resp.wTotalLength, device, EHCI_NO_DEBUG);
            if (r > 0) {
                buff = (uint8_t *) data_mem.va;
                dprintf("The combined reading returned"
                        "...[%d] bytes of data  first "
                        "byte = %u\n", r, *buff);
                set_interface_descriptor(&(dev->config[i]), buff + (*(buff))
                                         /*start of interface descriptor */
                                         , req.wLength, resp.bNumInterfaces);
                assert(dev->config[i].intf != NULL);
            } else
                assert(0);

        } else
            assert(!"The configuration descriptor reading req" "FAILED ...");
    }




}

/*
 * \brief Function to read device descriptor from the device.
 * \param address Device address. 
 *                  zero  = use default address, before address assignment
 *                  !zero = Assigned dev address 
 */
static usb_device_descriptor read_device_descriptor(int address)
{
    struct usb_device_request req;
    struct usb_device_descriptor resp;
    int r;
    usb_mem data_mem = malloc_64();

    dprintf("Initiating the  device descriptor reading logic\n");
    req = get_usb_device_descriptor_request();
    r = usb_dctrl_exe(req, data_mem.pa,
                      sizeof (struct usb_device_descriptor),
                      address, EHCI_NO_DEBUG);
    if (r > 0) {
        dprintf("The device descriptor read sucessfully\n");
        copy_usb_message(data_mem.va, (void *)&resp, req.wLength);
        //print_usb_device_descriptor(resp);
    } else
        assert(!"The device descriptor reading req FAILED ...\n");
    free_64(data_mem);
    return resp;
}

/*
 * \brief Fetch all the string descriptors available on the device 
 *
 * \param dev USB device to be probed for the strings
 */

static void fill_string_descriptors(usb_device_t * dev)
{
    struct usb_device_request req;
    struct usb_device_descriptor resp = dev->desc;
    int r;
    char str[64];
    usb_mem data_mem = malloc_64();

    req = get_usb_device_string_desc_request(resp.iManufacturer);

    r = usb_dctrl_exe(req, data_mem.pa, req.wLength,
                      USB_DEFAULT_ADDRESS, EHCI_NO_DEBUG);

    if (r > 0) {
        r = copy_usb_message(data_mem.va, (void *)&str, req.wLength);
        dprintf("Manufacturer - \n\n");
        dprintf("\n\n");
        dev->device_manufacturer = (char *)malloc(sizeof (char) * (r - 2));
        clear_arr((void *)(dev->device_manufacturer), r - 2);
        copy_data(str + 2, dev->device_manufacturer, r - 2);
        dev->man_sz = r - 2;

    } else
        dprintf("\n USBD: Manufacturer reading failed ...?\n");



    req = get_usb_device_string_desc_request(resp.iProduct);

    r = usb_dctrl_exe(req, data_mem.pa, req.wLength,
                      USB_DEFAULT_ADDRESS, EHCI_NO_DEBUG);

    if (r > 0) {
        r = copy_usb_message(data_mem.va, (void *)&str, req.wLength);
        dprintf("Product - \n\n");
        dev->device_name = (char *)malloc(sizeof (char) * (r - 2));
        clear_arr(dev->device_name, r - 2);
        copy_data(str + 2, dev->device_name, r - 2);
        dev->dev_sz = r - 2;
    } else
        dprintf("Product Reading failed ...?\n");



    req = get_usb_device_string_desc_request(resp.iSerialNumber);
    clear_arr((void *)&str, 64);

    r = usb_dctrl_exe(req, data_mem.pa, req.wLength,
                      USB_DEFAULT_ADDRESS, EHCI_NO_DEBUG);
    if (r > 0) {
        r = copy_usb_message(data_mem.va, (void *)&str, req.wLength);
        dprintf("Serial Number - \n\n");
        dprintf("\n");
        dev->serial_number = (char *)malloc(sizeof (char) * (r - 2));
        clear_arr(dev->serial_number, r - 2);
        copy_data(str + 2, dev->serial_number, r - 2);
        dev->sr_sz = r - 2;
    } else
        dprintf("Serial Number Reading failed ...?\n");
}

/*
 * \brief Sets the non-zero device address to the newly enumerated USB 
 *        device. 
 * \param address Address to be assigned
 */


static void set_usb_device_address(uint8_t address)
{
    struct usb_device_request req;
    struct usb_device_descriptor udd;
    int r;
    dprintf(" Moving the device to the addressed mode \n");
    req = get_usb_device_set_address_request(address);
    r = usb_ctrl_exe(req, USB_DEFAULT_ADDRESS, EHCI_NO_DEBUG);
    if (r == 0) {
        dprintf("The device is moved to addressed mode successfully.\n");
        udd = read_device_descriptor(address);
        dprintf("The device PASSED the addresses test\n");

    } else
        USER_PANIC("\n USBD: The device ADDRESSING FAILED ...");

}

/*
 * \brief Reads the current device configuration index from the device.
 *
 * \param dev_add Device address from there configuration has to be read. 
 */

static uint8_t read_current_device_configuration(uint8_t dev_add)
{
    struct usb_device_request req;
    uint8_t resp;
    int r;
    usb_mem data32 = malloc_32();

    dprintf("Reading the current device descriptor index\n ");
    req = get_usb_device_current_configuration_request();
    r = usb_dctrl_exe(req, data32.pa, sizeof (uint8_t), dev_add, EHCI_NO_DEBUG);
    if (r > 0) {
        resp = *((uint8_t *) data32.va);
        dprintf("The current device descriptor read sucessfully"
                "...and is --> [%u]\n", resp);
    } else
        dprintf("The current device descriptor reading" "req FAILED ...\n");

    free_32(data32);
    return resp;
}


/*
 * \brief Sets a particualr configuration paased. 
 *
 * \param config_idx Configuration index to be enabled 
 * \param dev Address of the device on which it has to 
 */

static void set_usb_device_configuration(uint8_t config_idx, usb_device_t * dev)
{
    struct usb_device_request req;
    int r;
    uint8_t test;
    dprintf("Moving the device to the configuration mode\n");
    req = get_usb_device_set_config_request(config_idx);
    r = usb_ctrl_exe(req, dev->address, EHCI_NO_DEBUG);
    if (r == 0) {
        dprintf("The device is moved to configuration mode successfully\n");
        test = read_current_device_configuration(dev->address);
        dprintf("Test = %u config_idx = %u\n", test, config_idx);
        assert(test == config_idx);
        dprintf("The device passed the assert test for config\n");

        // RESET the data toggle status
        // this qualifies as a reset event as mentioned 
        // in 8.5.2, 9.1.1.5 and 9.4.5
        dev_toggle_reset(dev->address);
    } else
        USER_PANIC("\n USBD: The device config assignment FAILED ...");

}

/*
 * \brief Probe a particular endpoint to get status update. Status = working, 
 *        or halted. 
 * \param dev USB device containing the endpoint 
 * \param ep Endpoint number 
 */

int get_ep_status(usb_device_t dev, uint8_t ep)
{

    int r;
    uint16_t data;
    usb_mem data32 = malloc_32();

    usb_device_request req = get_usb_device_ep_status_request(ep);

    r = usb_dctrl_exe(req, data32.pa, 2, dev.address, EHCI_NO_DEBUG);
    dprintf("Status command completed ....checking status\n");

    if (r > 0) {
        data = *((uint16_t *) data32.va);
        dprintf("\n STATUS of ep %x is %x \n", ep, data);
        r = data;
    } else {
        dprintf("EP STATUS querry failed\n");

    }

    free_32(data32);
    return r;
}

/*
 * \brief Clears the endpoint halt from a device. The device might not support
 *        the software clearing of the command, hence command might fail. 
 *
 * \param dev USB device containing the endpoint
 * \param ep Endpoint address to be cleared
 */

int clear_ep_halt(usb_device_t dev, uint8_t ep)
{
    int r = 0;
    usb_device_request req = get_usb_device_clear_halt_ep_request(ep);
    print_usb_device_request(req);
    r = usb_ctrl_exe(req, dev.address, EHCI_NO_DEBUG);
    dprintf("EP [%x] Halt reset, command completed, checking status\n", ep);
    if (r == 0) {
        r = 0;
    } else {
        dprintf("EP Halt clear querry failed\n");
    }

    return r;
}

/*
 * \brief Explicitly set halt on an endpoint. The USB device might not support 
 *        this and command can fail. 
 *
 * \param dev USB device containg the endpoint 
 * \param ep Endpoint address
 */

int set_ep_halt(usb_device_t dev, uint8_t ep)
{
    int r = 0;
    dprintf("Passed ep in set halt ep is %x\n", ep);
    usb_device_request req = get_usb_device_set_halt_ep_request(ep);
    print_usb_device_request(req);
    r = usb_ctrl_exe(req, dev.address, EHCI_NO_DEBUG);
    dprintf("EP [%x] Halt set, command completed, checking status\n", ep);
    if (r == 0) {
        dprintf("EP Halt set querry completed succesfully\n ");
        r = 0;
    } else {
        dprintf("EP Halt set querry failed\n");
    }

    return r;

}


/*
 * \brief Fetches the device status. Device can be in active or suspended
 *        state and waiting for the remote wakeup. Might not be supported 
 *        by all devices.  
 *
 * \param dev USB device to be probed
 */

int get_device_status(usb_device_t dev)
{
    int r;
    uint16_t data;
    usb_mem data32 = malloc_32();

    usb_device_request req = get_usb_device_status_request();

    r = usb_dctrl_exe(req, data32.pa, 2, dev.address, EHCI_NO_DEBUG);
    dprintf("Device status probing finished ....checking status\n");

    if (r > 0) {
        data = *((uint16_t *) data32.va);
        dprintf("DEVICE [%d] STATUS is [%x] \n", dev.address, data);
    } else {
        dprintf("Device status querry failed\n");

    }
    free_32(data32);
    return r;
}

/*
 * \brief Set device remote wakeup feature. Look for USB 2.0 documentation 
 *        for more details. 
 * \param dev Device address 
 */


int set_device_remote_wakeup(usb_device_t dev)
{

    int r;
    usb_device_request req = get_usb_device_set_rwakeup_request();

    r = usb_ctrl_exe(req, dev.address, EHCI_NO_DEBUG);
    dprintf("Setting rwakeup finished ....checking status\n");

    if (r == 0) {
        dprintf("Setting rwakeup on device [%d] OK ------ \n", r);
    } else {
        dprintf(" Setting rwakeup on device FAILED \n");

    }

    return r;
}

/*
 * \brief Clears the remote wakeup feature of a USB device 
 *
 * \param dev Device address to be cleared
 */

int clear_device_rwakeup(usb_device_t dev)
{

    int r;
    usb_device_request req = get_usb_device_clear_rwakeup_request();

    r = usb_ctrl_exe(req, dev.address, EHCI_NO_DEBUG);
    dprintf("Clearing rwakeup finished ....checking status\n");

    if (r == 0) {
        dprintf("Clearing rwakeup on device [%d] OK ------ \n", r);
    } else {
        dprintf("Clearing rwakeup on device failed \n");
    }

    return r;
}

/*
 * \breif Release all associated resources with a particular device.
 *
 * \param idx Address of the USB device to be freed. 
 */

void release_device(uint8_t idx)
{
    release_resources(usb_device_arr[idx]);
    release_address(idx);
    //FIXME: 
    //Call diconnect on driver 
    // No topology entry has been made so not required to 
    // remove device from there 
}


/*
 * \brief Choose from a given set of configurations which one to enabale 
 *        on the USB device. 
 */

static int get_optimal_config(usb_device_t dev)
{
    // FIXME: This should consult USB bandwidth and power managment 
    // logic to give proper index. BUT useful only for periodic endpoints.
    // Bulk and control eps do not require it.  
    return dev.config[0].config.bConfigurationValue;
}


/*
 * \brief This is the thread entry point of the device enumeration code. 
 *        Thread is used because, enumeration process also sends and takes
 *        messages from the EHCI server. But if more then one device 
 *        renumeration request is sent by EHCI then they are sync over mutex. 
 */

static int device_enumeration_t(void *args)
{
    while (1) {
        thread_sem_wait(&dev_sem);
        assert(port != -1);

        struct usb_device_descriptor udd;

        // allocate address in USB device array
        uint8_t idx = allocate_new_usb_address();
        int config_idx;
        uint8_t class_code, subclass_code, protocol_code;

        assert(idx > 0);

        //read device descriptor
        udd = read_device_descriptor(USB_DEFAULT_ADDRESS);

        //init the array slot 
        usb_device_arr[idx].port = port;
        usb_device_arr[idx].address = idx;
        usb_device_arr[idx].desc = udd;
        usb_device_arr[idx].no_config = udd.bNumConfigurations;
        usb_device_arr[idx].config =
            (usb_config_t *) malloc(sizeof (usb_config_t) *
                                    udd.bNumConfigurations);

        //Read and fill configuration details
        fill_configuration_descriptors(udd.bNumConfigurations,
                                       USB_DEFAULT_ADDRESS,
                                       &usb_device_arr[idx]);
        dprintf("Calling to fill string descriptors\n");

        fill_string_descriptors(&usb_device_arr[idx]);
        print_usb_device_info(usb_device_arr[idx]);
        dprintf("Now moving device to the ADDRESSED mode"
                "...assigning address [%d]", idx);

        // Move device to the addressed mode 
        set_usb_device_address(idx);

        dprintf("\n\n\n\n");
        dprintf("Now moving device to the CONFIGURED mode "
                "...assigning configuration [%d]\n",
                usb_device_arr[idx].config[0].config.bConfigurationValue);

        config_idx = get_optimal_config(usb_device_arr[idx]);
        // This is current configuration of the device 
        usb_device_arr[idx].curr_config = config_idx - 1;

        // Enable optimal configuration 
        set_usb_device_configuration(config_idx, &usb_device_arr[idx]);
        //print_usb_device_info(usb_device_arr[idx]);

        dprintf("\n\n\n\n");
        dprintf("Device [%d] is now, in CONFIGURED + ADDRESSED state "
                "...\n USBD: Now probing for device drivers ?? \n\n\n", idx);

        dprintf("step 1\n");
        uint8_t curr_intf =
            usb_device_arr[idx].config[config_idx - 1].curr_intf;
        assert(usb_device_arr[idx].config[config_idx - 1].intf != NULL);

        usb_interface_t uit =
            usb_device_arr[idx].config[config_idx - 1].intf[curr_intf];
        dprintf("step 2\n");
        class_code = uit.intf.bInterfaceClass;
        dprintf("step 3\n");
        subclass_code = uit.intf.bInterfaceSubClass;
        dprintf("step 4\n");
        protocol_code = uit.intf.bInterfaceProtocol;
        dprintf("step 5\n");
        printf("Sending infortioan to driver ....%llu\n",
               (unsigned long long int)rdtsc());
        locate_and_notify_driver(idx, class_code, subclass_code, protocol_code);

        dprintf("\n\n\n Driver handling done ....\n");

        continue;
        /* old set of code used with 1-domain testing */

#if 0



        // OK
        set_device_remote_wakeup(usb_device_arr[idx]);
        get_device_status(usb_device_arr[idx]);
        clear_device_rwakeup(usb_device_arr[idx]);
        port = -1;

        // OK
        //get_ep_status(usb_device_arr[idx], 0x01);
        //get_ep_status(usb_device_arr[idx], 0x82);

        //set_ep_halt(usb_device_arr[idx], 0x82);
        //assert(0);
        //clear_ep_halt(usb_device_arr[idx], 0x82);
        //assert(0);

        /*if(func == NULL)
           {
           dprintf("\n USBD: USB was not able to find appropriate driver for the device ...");
           dprintf("\n USBD: Releasing all associated resoources ...");
           release_device(idx);
           }else
           {
           // Insert node at root hub
           insert_node(NULL, &usb_device_arr[idx]);

           // FIXME: In a new thread
           // Just pass the index for the device  
           func(idx);
           } */

        assert(!"NO CODE FURTHER FROM THIS POINT ONWARDS");
        release_device(idx);

        // Make entry and fill the device structure and entry in tree 
        // Probe for STATUS, on device, interface and eps  
#endif
    }
    thread_exit();
    return 0;
}

/*
 * \brief Event handler function. To notify thread about new port activity
 */
void notify_new_device(int p)
{
    // Notify the thread and return ....
    port = p;
    thread_sem_post(&dev_sem);
}


/*
 * \brief The init_pipe function handles all pipe related allocation requests. 
 *        A device driver can request USB manager to look and allocate/give 
 *        details about a particualr type of pipe. If such a pipe is found
 *        on the device then the pipe related informtion is sent to the 
 *        driver function and driver knows rest of the procedure. A all
 *        ready allocated pipe is not allocated again. 
 *        
 * \param dev_add USB device on which pipe has to look for
 * \param type Type of pipe
 * \param dir Direction for the pipe Input/Output
 * \param pipe If such pipe is found then assign it to pipe. 
 */

void init_pipe(uint8_t dev_add, uint8_t type, uint8_t dir, usb_pipe_t * pipe)
{
    usb_device_t *dev = &usb_device_arr[dev_add];
    uint8_t curr_intf = dev->config[dev->curr_config].curr_intf;
    uint8_t no_ep = dev->config[dev->curr_config].intf[curr_intf].no_ep;
    uint8_t i;
    usb_endpoint_t *ep_buff = dev->config[dev->curr_config].intf[curr_intf].ep;
    bool flag = false;
    dprintf("Pipe searching ...dev [%u], type [%u] dir [%u]\n",
            dev_add, type, dir);

    // Assuming that there is just one of (type, dir)
    // Code will not do reallocation of a pipe 
    for (i = 0; i < no_ep; i++) {
        if (GET_EP_DIR(ep_buff[i].ep.bEndpointAddress) == dir
            && GET_EP_TYPE(ep_buff[i].ep.bmAttributes) == type
            && ep_buff[i].pipe_allocated == 0) {
            pipe->dev = dev_add;
            ep_buff[i].pipe_allocated = 1;
            pipe->ep_number = GET_EP_NUMBER(ep_buff[i].ep.bEndpointAddress);
            pipe->ep_address = ep_buff[i].ep.bEndpointAddress;
            pipe->ep_dir = GET_EP_DIR(ep_buff[i].ep.bEndpointAddress);
            pipe->ep_type = GET_EP_TYPE(ep_buff[i].ep.bmAttributes);
            pipe->ep_psz = GET_EP_PSZ(ep_buff[i].ep.wMaxPacketSize);
            pipe->multi = GET_EP_MULTI(ep_buff[i].ep.wMaxPacketSize);
            pipe->valid = 1;
            flag = true;
            dprintf("Requested pipe located %d\n", flag);
        }
    }


    if (!flag)                  // No match 
    {
        dprintf("Requested pipe related information not found\n");
        // Mark *pipe content as invalid
        pipe->valid = 0;
    }

}

/*
 * \brief When a device is removed from a port, then the function removed 
 *        all the realted metadata from the USB manager registry. It includes 
 *        releasing all associated resources. 
 */

void notify_device_removal(int p)
{
    int i;
    for (i = 0; i < N_USB_DEVICES; i++)
        if (usb_device_arr[i].port == p)
            break;

    if (i == N_USB_DEVICES) {
        //Perhaps a non enumerated device has been detached 
        //from the USB
        //Just return ignoring the interrupt
        return;
    }

    set_dev_status(usb_device_arr[i].address, DISCONN_RH);
    remove_node(&usb_device_arr[i]);
    release_resources(usb_device_arr[i]);
    release_address(i);
}


/*
 * \brief Init the USB manager. Init responsibilities include 
 *        1. Device related management init 
 *           -- Array 
 *           -- Tree
 *        2. Create a mapping of shared page and sent it to EHCI 
 *        3. Create enumeration thread. 
 */

void usb_manager_init(void)
{
    dprintf("\n USBD: %s", __func__);
    static bool init = false;
    if (!init) {

        usb_device_init();
        map_init();
        device_enum_t = thread_create(device_enumeration_t, 0);
        assert(device_enum_t != NULL);
        errval_t err = thread_detach(device_enum_t);
        assert(err_is_ok(err));
        init = true;

    }
}
